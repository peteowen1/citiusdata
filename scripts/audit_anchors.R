# Does the model rate the athletes who are actually running fast?
#
# The standing anchor check. Unlike `audit_evidence.R`, which asks whether the
# probabilities are honest, this asks the blunter question: take the athletes
# with the best RECENT MARKS in an event, and see where the model puts them.
#
# The anchor is deliberately built on form rather than titles. "Reigning world
# champion should be top three" fails for honest reasons -- athletes age, get
# injured, retire. "An athlete whose last five races are the fastest in the
# world should be near the top of the rating" has no such excuse, needs no
# external source of truth, and is checkable from our own data.
#
# It exists because the same failure has now happened twice: the Olympics filed
# as a second-tier meet, and Noah Lyles rated fifth on the best recent form in
# the field. Both times the output looked plausible and was explained rather
# than tested. An anchor that runs automatically cannot be talked round.
#
# Usage:  Rscript scripts/audit_anchors.R
#         CITIUS_ANCHOR_EVENTS="AT-100Metres-M,AT-1500Metres-M" Rscript ...
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")

EV <- Sys.getenv("CITIUS_ANCHOR_EVENTS", "")
EVENTS <- if (nzchar(EV)) trimws(strsplit(EV, ",")[[1]]) else
  as.data.table(citius_events())[!is.na(family)]$event_id
ACTIVE_DAYS <- 730L
MIN_RECENT  <- 3L        # need a few recent races before "form" means anything

cal <- deployed_calibration(OUT)
h <- deployed_history(OUT, events = EVENTS, from = as.Date("2005-01-01"), to = Sys.Date())
h <- h[!is.na(perf) & !is.na(date)]
h[, athlete_id := as.character(athlete_id)]
AS_OF <- max(h$date, na.rm = TRUE)
cli::cli_alert_info("as of {AS_OF}: {format(nrow(h), big.mark=',')} marks, {uniqueN(h$event_id)} events")

ab <- deployed_ability(h, as_of = AS_OF, calibration = cal)
ab[, athlete_id := as.character(athlete_id)]

# Recent form: the mean of an athlete's last five marks, oriented so higher is
# better. No context adjustment, no shrinkage -- the point is that this is the
# naive number, and the model is supposed to beat it.
setorder(h, athlete_id, event_id, -date)
form <- h[, .(l5 = mean(head(perf, 5)), n_recent = sum(date >= AS_OF - ACTIVE_DAYS),
              last = max(date), best = max(perf)),
          by = .(athlete_id, event_id)]
form <- form[n_recent >= MIN_RECENT & last >= AS_OF - ACTIVE_DAYS]

d <- merge(ab[, .(athlete_id, event_id, ability)], form, by = c("athlete_id", "event_id"))
d[, n_ev := .N, by = event_id]
d <- d[n_ev >= 15]
d[, `:=`(r_form = frank(-l5, ties.method = "min"),
         r_model = frank(-ability, ties.method = "min")), by = event_id]
nm <- unique(setDT(readRDS(file.path(OUT, "championship_results.rds")))[
  !is.na(athlete_name), .(athlete_id = as.character(athlete_id), athlete_name)],
  by = "athlete_id")
d <- merge(d, nm, by = "athlete_id", all.x = TRUE)
d[is.na(athlete_name), athlete_name := paste0("id ", athlete_id)]

cat("\n=================================================================\n")
cat("ANCHOR: is the form leader near the top of the rating?\n")
cat("=================================================================\n")
lead <- d[r_form == 1]
for (k in c(1, 3, 5, 10)) {
  ok <- mean(lead$r_model <= k)
  cat(sprintf("  form leader in model top %-2d : %5.1f%%  (%d of %d events)\n",
              k, 100 * ok, sum(lead$r_model <= k), nrow(lead)))
}
cat(sprintf("\n  median model rank of the form leader: %.0f\n", median(lead$r_model)))
cat(sprintf("  worst: rank %d of %d\n", max(lead$r_model), d[event_id == lead[which.max(r_model)]$event_id, .N]))

cat("\n=== ANCHOR: do the top-5 on form appear in the model's top 10? ===\n")
t5 <- d[r_form <= 5]
cat(sprintf("  %.1f%% (%d of %d athlete-events)\n", 100 * mean(t5$r_model <= 10),
            sum(t5$r_model <= 10), nrow(t5)))

cat("\n=== rank agreement, by family ===\n")
reg <- as.data.table(citius_events())[, .(event_id, family)]
d <- merge(d, reg, by = "event_id")
cat("Spearman correlation between form rank and model rank, within event:\n")
print(d[, .(events = uniqueN(event_id), athletes = .N,
            rho = round(cor(r_form, r_model, method = "spearman"), 3)),
        by = family][order(rho)])

cat("\n=== WORST MISSES: fast recently, rated poorly ===\n")
d[, gap := r_model - r_form]
w <- d[r_form <= 10][order(-gap)]
print(head(w[, .(athlete_name, event_id, form_rank = r_form, model_rank = r_model,
                 gap, field = n_ev)], 20))

cat("\n=== and the reverse: rated highly on weak recent form ===\n")
print(head(d[r_model <= 10][order(gap)][, .(athlete_name, event_id,
      form_rank = r_form, model_rank = r_model, gap, field = n_ev)], 10))

FAIL <- mean(lead$r_model <= 3) < 0.60
cat("\n--------------------------------------------------------------\n")
cat(sprintf("%s: form leader reaches the model's top 3 in %.0f%% of events\n",
            if (FAIL) "FAIL" else "PASS", 100 * mean(lead$r_model <= 3)))
if (FAIL) cat("The rating is not tracking who is actually running fast.\n")
cat("--------------------------------------------------------------\n")
