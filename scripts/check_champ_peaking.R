# Do favourites underperform at championships, and is the rating scale itself
# too wide?
#
# THE CLAIM ON RECORD: "dominant athletes underperform their rating by 0.5-0.7%
# as a group, and it replicates across years, but test them one at a time and
# not a single athlete survives correction for multiple comparisons." Separately:
# "athletes who peak at championships are systematically under-rated - measured,
# and not yet fixed."
#
# THE STRUCTURAL POINT THAT DECIDES THE DESIGN. `surprise` is what is left after
# the race shock, and the shock is the field's shared miss - so surprise is very
# nearly demeaned WITHIN each race. "Everyone runs fast at the Olympics" cannot
# show up in it; that is the same cancellation the whole engine rests on. What
# CAN show up is a differential: favourites doing systematically worse than
# outsiders relative to their ratings. Within a race those are the same
# statement, because if favourites underperform, someone else must overperform.
#
# AND THAT IS NOT AN ATHLETE FACT, IT IS A SCALE FACT. If the model's rating
# GAPS are too wide, the favourite wins less often than the gap implies, and it
# will look exactly like "dominant athletes underperform". So the question to ask
# is not about athletes at all: is a 0.02 rating gap worth what the model thinks?
#
# WHY NOT REGRESS THE MARK GAP ON THE RATING GAP. Because r_pre is measured with
# error, so OLS slope is attenuated by construction - this project already
# mistook exactly that for a finding (slope 0.459, which was 0.501 x 0.917 to
# three decimals). A calibration curve on the BINARY outcome has no such
# problem: bucket the pairs by predicted gap and ask how often the favourite
# actually won. Predicted-versus-actual on a proportion is attenuation-free.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
YRS  <- as.integer(strsplit(Sys.getenv("PEAK_YEARS", "2025,2026"), ",")[[1]])

h <- setDT(read_parquet(file.path(OUT, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","event_id","athlete_id","date","r_pre",
                                       "place","perf","seen","rc","n_eff")))
h <- h[seen == TRUE & is.finite(r_pre) & is.finite(place) & place > 0 & is.finite(perf)]
# SCORE THE ORDERING VALUE. r_pre is the bare rating; r_use is that rating after
# the ceiling and cross-event blends, and it is what the engine actually orders
# a field with. Any concordance, win-rate or accuracy number here must use
# r_use; r_pre understates the model. On 2026-08-21/22 this same confusion
# inverted four separate conclusions - the hurdles "losing" to season best, the
# model "losing" on thin records, a pooled margin of 1.15 that is 1.79, and a
# "semi-final deficit" that does not exist. Set BASELINE_PRED=r_pre to score
# the bare rating deliberately.
if (!"r_use" %chin% names(h)) h[, r_use := r_pre]
h[!is.finite(r_use), r_use := r_pre]
MODEL_COL <- Sys.getenv("BASELINE_PRED", "r_use")
stopifnot("BASELINE_PRED names a column that does not exist" = MODEL_COL %chin% names(h))
cat(sprintf("scoring the model as `%s`\n", MODEL_COL))

h[, yr := year(date)]
h[, mv := get(MODEL_COL)]   # the value the engine orders with
h <- h[yr %in% YRS]
stopifnot("no rows in the window" = nrow(h) > 10000)

cp <- unique(setDT(read_parquet(file.path(OUT, "athletics_corpus.parquet"),
                                col_select = c("race_key","competition_id"))), by = "race_key")
cp[, competition_id := as.character(competition_id)]
cg <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet"),
                         col_select = c("competition_id","class","meet_tier")))
cg[, competition_id := as.character(competition_id)]
h <- merge(h, merge(cp, cg, by = "competition_id", all.x = TRUE)[, .(race_key, class, meet_tier)],
           by = "race_key", all.x = TRUE)
MAJ <- c("olympics","world_champs","european_champs","commonwealth","world_indoor")
h[, occasion := fifelse(!is.na(class) & class %chin% MAJ, "championship",
                fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", "T1 meet", "ordinary"))]
# merged races out, as everywhere
dup <- h[, .(n = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
         n > 1 & marks > 1, unique(race_key)]
h <- h[!race_key %chin% dup]
cat(sprintf("%s scored rows, %s races, by occasion:\n",
            format(nrow(h), big.mark = ","), format(uniqueN(h$race_key), big.mark = ",")))
print(h[, .(rows = .N, races = uniqueN(race_key)), by = occasion][order(-rows)])

# --- pairs, with the predicted and actual outcome ---------------------------
a <- h[, .(rid = .GRP, i = seq_len(.N), place, mv, n_eff, occasion = occasion[1]), by = race_key]
m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
# The self-merge suffixes EVERY shared column, occasion included, so the pair
# carries occasion.x and occasion.y. They are the same race and therefore always
# equal - assert that rather than assume it, then collapse to one column.
stopifnot("the two sides of a pair disagree about the occasion" =
            all(m$occasion.x == m$occasion.y))
setnames(m, "occasion.x", "occasion"); m[, occasion.y := NULL]
stopifnot("no pairs" = nrow(m) > 10000)
# orient each pair so .x is the FAVOURITE, so "gap" is always positive and
# "won" is always "did the favourite win"
m[, `:=`(gap = abs(mv.x - mv.y),
         fav_won = as.numeric(((mv.x > mv.y) & (place.x < place.y)) |
                              ((mv.y > mv.x) & (place.y < place.x))))]
m <- m[gap > 0]
cat(sprintf("\n%s pairs with a non-zero rating gap\n", format(nrow(m), big.mark = ",")))

# --- calibration: does a bigger gap deliver what it promises? ---------------
# The model has no explicit win-probability, so the honest reference is the
# EMPIRICAL curve fitted on ordinary meets, then applied to championships. If
# championships sit below their own gap's ordinary-meet rate, the favourite is
# underperforming THERE specifically, which is the claim.
m[, band := cut(gap, quantile(gap, seq(0, 1, 0.1), na.rm = TRUE),
                include.lowest = TRUE, labels = FALSE)]
tab <- m[, .(pairs = .N, gap_mid = round(stats::median(gap), 5),
             fav_win = round(100 * mean(fav_won), 2),
             floor = round(100 * sqrt(0.25 / .N), 2)), by = .(band, occasion)]
ord <- tab[occasion == "ordinary", .(band, base = fav_win)]
tab <- merge(tab, ord, by = "band", all.x = TRUE)
tab[, vs_ordinary := round(fav_win - base, 2)]
setorder(tab, band, occasion)
cat("\n=== favourite win rate by rating-gap decile and occasion ===\n")
print(tab[, .(band, occasion, pairs, gap_mid, fav_win, vs_ordinary, floor)])
cat("\nvs_ordinary is the championship (or T1) rate minus the ordinary-meet rate\n")
cat("AT THE SAME GAP. Consistently negative = favourites underperform there.\n")

cat("\n=== pooled, by occasion ===\n")
pool <- m[, .(pairs = .N, fav_win = round(100 * mean(fav_won), 3),
              mean_gap = round(mean(gap), 5),
              floor = round(100 * sqrt(0.25 / .N), 3)), by = occasion][order(-pairs)]
print(pool)
cat("\nA raw pooled difference is NOT evidence on its own - championship fields\n")
cat("are tighter, so the gaps are smaller and the favourite wins less often for\n")
cat("that reason alone. The decile table above holds the gap fixed; this does not.\n")

# --- is the SCALE too wide, regardless of occasion? -------------------------
# If rating gaps are systematically over-stated, the favourite win rate will
# rise with gap more slowly than the model's own spread implies. Compare the
# top decile against the bottom: a well-scaled rating should separate them hard.
cat("\n=== does a bigger gap deliver? (all occasions) ===\n")
sc <- m[, .(pairs = .N, gap_mid = round(stats::median(gap), 5),
            fav_win = round(100 * mean(fav_won), 2)), by = band][order(band)]
print(sc)

f <- file.path(OUT, "champ_peaking.json")
writeLines(jsonlite::toJSON(list(tag = TAG, years = YRS, by_band = tab, pooled = pool, scale = sc),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
