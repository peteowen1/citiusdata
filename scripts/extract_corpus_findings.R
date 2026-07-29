# Everything learnable from the full corpus backtest, extracted once.
#
# That arm took ~4.5 hours over 900 meets and 11,686 races. Day-to-day work now
# runs the elite cohort (~8 minutes), so this will not be repeated soon. The
# derived diagnostics below are the expensive part -- the raw artefact is just
# predictions and outcomes.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

b <- readRDS(file.path(OUT, "backtest_corpus.rds"))
p <- setDT(copy(b$predictions)); o <- setDT(copy(b$outcomes))
p[, athlete_id := as.character(athlete_id)]; o[, athlete_id := as.character(athlete_id)]
pr <- merge(p, o, by = c("race_id", "athlete_id"))
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
truth <- unique(ch[!is.na(race_key) & !is.na(mark) & mark > 0,
  .(race_id = race_key, athlete_id = as.character(athlete_id), actual = mark,
    event_id, age, tier, comp_start)], by = c("race_id", "athlete_id"))
reg <- as.data.table(citius_events()[, c("event_id","family","orientation")])
truth <- merge(truth, reg, by = "event_id", all.x = TRUE)
d <- merge(p[is.finite(median_mark)], truth, by = c("race_id", "athlete_id"))
d[, err := orientation * (log(median_mark) - log(actual))]
d <- d[is.finite(err)]
cal <- readRDS(file.path(OUT, "calibration_corpus.rds"))
d <- merge(d, as.data.table(cal$events)[, .(event_id, sigma_within)],
           by = "event_id", all.x = TRUE)
mae <- function(x) round(100*mean(abs(exp(x)-1)), 3)
pc  <- function(x) round(100*mean(exp(x)-1), 3)

f <- list()
f$meta <- list(races = uniqueN(pr$race_id), marks = nrow(d),
               gold = b$gold$overall$brier_skill, medal = b$medal$overall$brier_skill,
               mae = mae(d$err), bias = pc(d$err), extracted = Sys.time())

f$by_family <- d[, .(n = .N, mae = mae(err), bias = pc(err)), by = family][order(-n)]

f$by_event <- d[, .(n = .N, mae = mae(err), bias = pc(err)),
                by = .(event_id, family)][n >= 200][order(-mae)]

f$reliability <- pr[, .(n = .N, predicted = round(mean(p_gold),4),
                        observed = round(mean(hit),4),
                        gap = round(mean(hit)-mean(p_gold),4)),
                    by = .(bin = cut(p_gold, c(0,.1,.3,.5,.7,.9,1),
                                     include.lowest = TRUE))][order(bin)]

f$by_shrinkage <- d[, .(n = .N, mae = mae(err), bias = pc(err)),
                    by = .(sb = cut(shrinkage, c(-.01,.05,.15,.35,.6,1.01),
                           labels = c("<5%","5-15%","15-35%","35-60%",">60%")))][order(sb)]

f$by_age <- d[!is.na(age), .(n = .N, mae = mae(err), bias = pc(err)),
              by = .(ab = cut(age, c(0,20,23,26,29,32,99)))][order(ab)]

f$by_evidence <- d[, .(n = .N, mae = mae(err), bias = pc(err)),
                   by = .(wb = cut(w_total, c(-.01,1,3,8,1e6),
                          labels = c("<1","1-3","3-8",">8")))][order(wb)]

# Headroom: error above each family's own noise floor. E|N(0,s)| = s*sqrt(2/pi).
f$headroom <- d[!is.na(sigma_within), {
  fl <- mean(sigma_within)*sqrt(2/pi); ob <- mean(abs(err))
  .(n = .N, mae = mae(err), floor_pct = round(100*(exp(fl)-1),3),
    ratio = round(ob/fl, 3))}, by = family][order(-ratio)]

# sigma check on WITHIN-RACE error, which is what sigma must match.
d[, n_in := .N, by = race_id]
w <- d[n_in >= 4]
w[, e_ind := (err - mean(err)) / sqrt((.N-1)/.N), by = race_id]
f$sigma_z <- w[is.finite(sigma_within), .(n = .N,
                sd_z = round(sd(e_ind/sigma_within), 3)), by = family][order(sd_z)]

f$evidence <- d[, .(marks = .N, mean_w_total = round(mean(w_total, na.rm=TRUE), 2),
                    mean_shrinkage = round(mean(shrinkage, na.rm=TRUE), 3))]

f$by_period <- d[!is.na(comp_start), .(n = .N, mae = mae(err), bias = pc(err)),
                 by = .(period = fifelse(comp_start < as.Date("2024-01-01"),
                                         "tuning (pre-2024)", "locked (2024+)"))]

saveRDS(f, file.path(OUT, "corpus_findings.rds"))
for (nm in names(f)) {
  cat(sprintf("\n=== %s ===\n", nm))
  print(f[[nm]])
}
cli::cli_alert_success("Wrote corpus_findings.rds")
