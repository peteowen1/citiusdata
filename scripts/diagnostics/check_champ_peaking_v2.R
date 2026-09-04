# The championship comparison, with the two controls the first version lacked.
#
# WHY THIS EXISTS. check_champ_peaking.R found that favourites win MORE often at
# championships than at ordinary meets at the same rating gap - +2.25 to +5.69
# across nine of ten gap deciles. An audit then pointed out that it holds the
# GAP fixed and nothing else, while loading `rc` and `n_eff` and using neither.
#
# THE CONFOUND, STATED PROPERLY. Championships run heats, semis and finals;
# ordinary meets are disproportionately single-round finals. And the sibling
# analysis found that round matters on its own - in the hurdles, semis scored
# -3.80 against season best and finals -0.21. So "championship" and "non-final
# round" are correlated, and an occasion effect could be a round-composition
# effect wearing a different label. The direction is NOT obvious in advance:
# heats have wide-open fields, which flatters a favourite, and the gap control
# only partly absorbs that. So it has to be measured, not argued.
#
# The fix is to stratify: compare championship against ordinary WITHIN each
# round type, so composition cannot contribute. If the effect survives inside
# finals alone - the cleanest slice, and the one that decides medals - it is an
# occasion effect. If it only appears in the mixed pool, it was composition.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
YRS <- as.integer(strsplit(Sys.getenv("PEAK_YEARS", "2025,2026"), ",")[[1]])

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","event_id","athlete_id","date","r_pre",
                                       "r_use","place","perf","seen","rc","n_eff")))
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 & is.finite(perf)]
h <- h[year(date) %in% YRS]

cp <- unique(setDT(read_parquet(file.path(OUT, "athletics_corpus.parquet"),
                                col_select = c("race_key","competition_id"))), by = "race_key")
cp[, competition_id := as.character(competition_id)]
cg <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet"),
                         col_select = c("competition_id","class","meet_tier")))
cg[, competition_id := as.character(competition_id)]
h <- merge(h, merge(cp, cg, by = "competition_id", all.x = TRUE)[, .(race_key, class, meet_tier)],
           by = "race_key", all.x = TRUE)
MAJ <- c("olympics","world_champs","european_champs","commonwealth","world_indoor")
h[, occasion := fifelse(!is.na(class) & class %chin% MAJ, "championship", "ordinary")]
h[, rnd := fifelse(grepl("final", rc, ignore.case = TRUE) &
                   !grepl("semi", rc, ignore.case = TRUE), "final",
                   fifelse(grepl("semi", rc, ignore.case = TRUE), "semi", "heat"))]
dup <- h[, .(n = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
         n > 1 & marks > 1, unique(race_key)]
h <- h[!race_key %chin% dup]

cat("=== round composition, which is the confound ===\n")
comp <- dcast(h[, .N, by = .(occasion, rnd)], occasion ~ rnd, value.var = "N", fill = 0)
print(comp)
cat("\nIf championships are more heat/semi heavy than ordinary meets, an occasion\n")
cat("comparison that ignores round is partly a round comparison.\n")

a <- h[, .(rid = .GRP, i = seq_len(.N), place, r_use, n_eff,
           occasion = occasion[1], rnd = rnd[1], fs = .N), by = race_key]
m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
stopifnot("the two sides disagree about the race" =
            all(m$occasion.x == m$occasion.y) && all(m$rnd.x == m$rnd.y))
setnames(m, c("occasion.x","rnd.x","fs.x"), c("occasion","rnd","fs"))
m[, c("occasion.y","rnd.y","fs.y") := NULL]
m[, `:=`(gap = abs(r_use.x - r_use.y),
         fav_won = as.numeric(((r_use.x > r_use.y) & (place.x < place.y)) |
                              ((r_use.y > r_use.x) & (place.y < place.x))),
         # THE THINNER of the pair, which is what a depth control should use -
         # the first version banded rows and then paired within a band, which
         # silently compares only athletes of MATCHED experience.
         nmin = pmin(n_eff.x, n_eff.y))]
m <- m[gap > 0]
m[, band := cut(gap, quantile(gap, seq(0, 1, 0.1), na.rm = TRUE),
                include.lowest = TRUE, labels = FALSE)]

cmp <- function(d, lbl) {
  o <- d[occasion == "ordinary", .(band, base = mean(fav_won)), by = band][, .(band, base)]
  o <- unique(o)
  x <- d[occasion == "championship", .(pairs = .N, champ = mean(fav_won)), by = band]
  r <- merge(x, o, by = "band")
  if (!nrow(r)) return(NULL)
  data.table(slice = lbl, pairs = sum(r$pairs),
             # weight each decile by its championship pairs, so the summary is
             # the average championship pair's advantage at its own gap
             diff = round(100 * stats::weighted.mean(r$champ - r$base, r$pairs), 2),
             floor = round(100 * sqrt(0.25 / sum(r$pairs)), 2),
             deciles_positive = sum(r$champ > r$base), deciles = nrow(r))
}

cat("\n=== championship advantage at the same gap, STRATIFIED BY ROUND ===\n")
res <- rbindlist(list(
  cmp(m, "all rounds pooled (the original, uncontrolled)"),
  cmp(m[rnd == "final"], "finals only"),
  cmp(m[rnd == "semi"],  "semis only"),
  cmp(m[rnd == "heat"],  "heats only")), fill = TRUE)
print(res)
cat("\nIf 'finals only' keeps the effect, it is an occasion effect and the\n")
cat("original conclusion stands. If it collapses, it was round composition.\n")

cat("\n=== and controlling depth as well, inside finals ===\n")
res2 <- rbindlist(list(
  cmp(m[rnd == "final" & nmin <= 3],            "finals, thinner athlete <=3 races"),
  cmp(m[rnd == "final" & nmin > 3 & nmin <= 8], "finals, 4-8"),
  cmp(m[rnd == "final" & nmin > 8],             "finals, 9+")), fill = TRUE)
print(res2)

f <- file.path(OUT, "champ_peaking_v2.json")
writeLines(jsonlite::toJSON(list(tag = TAG, composition = comp, by_round = res, by_depth = res2),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
