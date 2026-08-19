# Should a rank built on one race sit above a rank built on twelve?
#
# THE PROBLEM, visible on the published page. 350 of 819 top-ten rows rest on
# fewer than three effective races. The men's 10,000m top ten contains five
# athletes with n_eff <= 1.7; Barega was ranked FIRST in the 1500m on 1.60 races
# until the cross-event blend was dialled back. Their marks are usually real -
# Barega has run 26:44 twice - but a rank resting on one race is not the same
# claim as one resting on twelve, and the table presents them identically.
#
# WHY NOT A HARD CUTOFF. "n_eff >= 3 to appear" throws away a genuinely fast
# athlete who has raced twice, and picks a threshold with nothing behind it. The
# model already has a better tool: shrink toward the event mean in proportion to
# the evidence, w = n_eff / (n_eff + k). A deep record barely moves; a single race
# is pulled most of the way back. That is the same empirical-Bayes shape used for
# the ratings themselves, so it needs no new idea, only a value for k.
#
# HOW IT IS JUDGED. Precision@10 and rank correlation against the World Athletics
# order, the same outside referee as everywhere else, plus what it does to the
# named cases. A change that improves the referee AND removes the one-race
# athletes from the top ten is worth having; one that only does the second is
# cosmetics.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")

d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
d[, athlete_id := as.character(athlete_id)]
stopifnot("display table has no rank_mark" = "rank_mark" %chin% names(d),
          "display table is empty" = nrow(d) > 1000)
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, orientation)]
d <- merge(d, reg, by = "event_id")
ASOF <- max(d$last, na.rm = TRUE)

wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]
wa[, sex := fifelse(grepl("^Men", event_group), "M", "W")]
wa[, disc := gsub(",", "", sub("^(Men|Women)'s ", "", event_group))]
wa[, disc := fcase(
  disc == "110mH", "110MetresHurdles", disc == "100mH", "100MetresHurdles",
  disc == "400mH", "400MetresHurdles", disc == "3000mSC", "3000MetresSteeplechase",
  disc == "20km Race Walking", "20KilometresRaceWalk",
  disc == "35km Race Walking", "35KilometresRaceWalk",
  grepl("^[0-9]+m$", disc), paste0(sub("m$", "", disc), "Metres"),
  default = gsub(" ", "", disc))]
wa[, event_id := paste0("AT-", disc, "-", sex)]
wa <- wa[event_id %chin% unique(d$event_id)]
stopifnot("WA mapping produced nothing usable" = uniqueN(wa$event_id) > 10)
cat(sprintf("%s: %s rows, %d events | WA covers %d events | as at %s\n", TAG,
            format(nrow(d), big.mark = ","), uniqueN(d$event_id),
            uniqueN(wa$event_id), ASOF))

# the ranking key in perf space, so shrinkage is symmetric in the units the model
# actually works in rather than in seconds or metres
d[, key := orientation * log(rank_mark)]
d[, mu := mean(key), by = event_id]

# THREE REFEREES, because they answer different questions and have very
# different power:
#   precision@N  - does the TOP of the table look right? A page is judged on its
#                  top. At N = 10 that is 440 slots across 44 events, so a single
#                  athlete moves it 0.2pp. WA ranks 20+ deep in EVERY event and
#                  50+ in most, so N = 20 doubles the sample for nothing.
#   spearman     - the whole ordering, every matched athlete. Most power by far
#                  (4,270) but weights the deep field equally with the top.
#   spearman_top - restricted to WA's top 30: top-focused AND better powered than
#                  precision@10. Believe this one when the others disagree.
score <- function(k) {
  x <- copy(d)
  if (is.finite(k)) {
    x[, w := n_eff / (n_eff + k)]
    x[, key := mu + w * (key - mu)]
  }
  setorder(x, event_id, -key)
  x[, rk := seq_len(.N), by = event_id]
  prec <- function(N) {
    res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
      wN <- wa[event_id == EV][order(wa_place)][1:min(N, .N)]
      oN <- x[event_id == EV & rk <= N, .(athlete_id)]
      if (!nrow(oN) || !nrow(wN)) return(NULL)
      data.table(wa_n = nrow(wN),
                 hits = sum(as.character(wN$athlete_id) %chin% oN$athlete_id))
    }))
    if (!nrow(res)) return(NA_real_)
    round(100 * sum(res$hits) / sum(res$wa_n), 1)
  }
  m <- merge(wa[, .(event_id, athlete_id = as.character(athlete_id), wa_place)],
             x[, .(event_id, athlete_id, rk)], by = c("event_id", "athlete_id"))
  mt <- m[wa_place <= 30]
  thin <- x[rk <= 10, .(thin = sum(n_eff < 3), n = .N)]
  data.table(k = k,
             `p@10` = prec(10), `p@16` = prec(16), `p@20` = prec(20),
             spearman = round(stats::cor(m$rk, m$wa_place, method = "spearman"), 4),
             sp_top30 = round(stats::cor(mt$rk, mt$wa_place, method = "spearman"), 4),
             n_top30 = nrow(mt), thin_top10 = thin$thin)
}

cat("\n=== shrinking the ranking key by evidence: w = n_eff / (n_eff + k) ===\n")
out <- rbindlist(lapply(c(Inf, 0, 0.5, 1, 2, 4, 8), score))
out[!is.finite(k), k := NA_real_]
print(out)
cat("\nk = NA is today's ranking, no shrinkage. k = 0 is also no shrinkage (w = 1),\n")
cat("and should reproduce it exactly - if it does not, the key was rebuilt wrong.\n")
cat("thin_top10 counts published top-ten rows with fewer than 3 effective races.\n")

base <- out[is.na(k)]
zero <- out[k == 0]
stopifnot("k = 0 did not reproduce the unshrunk ranking - the key is wrong" =
            abs(base$spearman - zero$spearman) < 1e-9 &&
            base$thin_top10 == zero$thin_top10)
cat("control passes: k = 0 reproduces today's ranking exactly\n")

cat("\n=== who leaves the men's 10,000m top ten, and at what k ===\n")
nm <- unique(d[, .(athlete_id, athlete_name)])
for (kk in c(NA_real_, 1, 4)) {
  x <- copy(d)
  if (is.finite(kk)) { x[, w := n_eff / (n_eff + kk)]; x[, key := mu + w * (key - mu)] }
  setorder(x, event_id, -key); x[, rk := seq_len(.N), by = event_id]
  y <- x[event_id == "AT-10000Metres-M" & rk <= 10]
  cat(sprintf("\n-- k = %s --\n", ifelse(is.na(kk), "none (today)", kk)))
  print(y[, .(rk, athlete = substr(athlete_name, 1, 22),
              mark = round(rank_mark, 1), n_eff = round(n_eff, 1))])
}
