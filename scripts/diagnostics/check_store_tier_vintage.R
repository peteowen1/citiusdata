# Is the parquet store's `meet_tier` a stale snapshot, and does it move ability?
#
# Same rows, same weights, different ability: the harness and the backtest agree
# on `shrinkage` and `w_total` to three decimals and disagree on `ability` by
# 1.2% in throws and jumps
# (docs/reviews/finals-level-bias-is-a-harness-artefact-2026-09-07.md).
# The remaining difference is how history is ADJUSTED before averaging, and the
# tier label those adjustments key on is the suspect: the store bakes in the
# catalogue as it stood when build_stores.R last ran, while a backtest with
# CITIUS_BT_MEET_TIER=1 re-joins the catalogue as it stands today.
#
# This answers two things:
#   1. Do the store's meet_tier labels differ from the catalogue's today, and
#      on how many rows?
#   2. Estimated on identical rows, how far apart are the abilities?
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/check_store_tier_vintage.R'
# Env: CITIUS_TV_EVENTS (comma list, default four throw/jump/sprint events),
#      CITIUS_TV_AS_OF (2024-03-01), CITIUS_TV_CAL
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
EVS   <- trimws(strsplit(Sys.getenv("CITIUS_TV_EVENTS",
          "AT-ShotPut-M,AT-LongJump-M,AT-100Metres-M,AT-5000Metres-M"), ",")[[1]])
AS_OF <- as.Date(Sys.getenv("CITIUS_TV_AS_OF", "2024-03-01"))
CAL   <- Sys.getenv("CITIUS_TV_CAL", "calibration_corpus_wac_coast_0904.rds")
say <- function(...) cat(sprintf(...), "\n", sep = "")
cal <- readRDS(file.path(OUT, CAL))

store <- file.path(OUT, "athletics_corpus_store")
cols <- intersect(c("athlete_id", "event_id", "date", "perf", "age", "round", "tier",
                    "meet_tier", "competition_id", "race_key", "wind", "momentum",
                    "indoor", "venue_country"), names(open_dataset(store)))
stopifnot("store has no meet_tier column" = "meet_tier" %in% cols)
x <- as.data.table(read_results_store(store, events = EVS, from = AS_OF - 365 * 12, to = AS_OF, columns = cols))
x <- flag_implausible(x)[is.finite(perf) & date < AS_OF]
x[, athlete_id := as.character(athlete_id)]; x[, date := as.Date(date)]
x[, competition_id := as.character(competition_id)]
say("history: %s rows, %d events, to %s", format(nrow(x), big.mark = ","), uniqueN(x$event_id), format(AS_OF))

# --- 1. the two labellings ---------------------------------------------------
ct <- as.data.table(open_dataset(file.path(OUT, "competition_catalogue.parquet")) |>
                      dplyr::select(competition_id, meet_tier) |> dplyr::collect())
ct[, competition_id := as.character(competition_id)]
ct <- unique(ct[!is.na(meet_tier)], by = "competition_id")
setnames(ct, "meet_tier", "cat_tier")
x <- merge(x, ct, by = "competition_id", all.x = TRUE)
cov <- mean(!is.na(x$cat_tier))
say("catalogue covers %.1f%% of these history rows", 100 * cov)
cmpr <- x[!is.na(cat_tier) & !is.na(meet_tier)]
disagree <- mean(cmpr$meet_tier != cmpr$cat_tier)
say("store meet_tier vs catalogue today: DISAGREE on %.2f%% of %s comparable rows",
    100 * disagree, format(nrow(cmpr), big.mark = ","))
if (disagree > 0) {
  cat("\nwhere they disagree (store -> catalogue), largest cells:\n")
  print(cmpr[meet_tier != cat_tier, .N, by = .(store = meet_tier, catalogue = cat_tier)][order(-N)][1:10])
}
cat("\nstore labelling:\n"); print(table(x$meet_tier, useNA = "ifany"))
cat("catalogue labelling (on the same rows):\n"); print(table(x$cat_tier, useNA = "ifany"))

# --- 2. does it move ability? ------------------------------------------------
fit <- function(h) {
  reg_f <- as.data.table(citius_events()[, c("event_id", "family")])
  pf <- merge(h, reg_f, by = "event_id", all.x = TRUE)
  pf[is.na(family), family := ""]
  ab <- rbindlist(lapply(split(pf, pf$family), function(g) {
    fam <- g$family[1]
    hl <- if (fam %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[fam]] else DEPLOYED$half_life
    estimate_ability(g[, !"family"], as_of = AS_OF, half_life = hl, calibration = cal)
  }), fill = TRUE)
  ab[, athlete_id := as.character(athlete_id)]
  ab[, .(athlete_id, event_id, ability, shrinkage, w_total)]
}
h_store <- copy(x)[, cat_tier := NULL]
h_cat   <- copy(x)[!is.na(cat_tier), meet_tier := cat_tier][, cat_tier := NULL]
a1 <- fit(h_store); a2 <- fit(h_cat)
j <- merge(a1, a2, by = c("athlete_id", "event_id"), suffixes = c("_store", "_cat"))
j[, d := 100 * (ability_cat - ability_store)]
cat("\n=== ability with the CATALOGUE's tiers minus ability with the STORE's, % of a mark ===\n")
print(j[, .(athletes = .N, mean_diff = round(mean(d), 4), sd = round(sd(d), 4),
            p10 = round(quantile(d, 0.1), 3), p90 = round(quantile(d, 0.9), 3),
            moved = sum(abs(d) > 1e-9)), by = event_id][order(event_id)])
say("\npooled mean %+.4f%% over %s athlete-events; %.1f%% moved at all",
    mean(j$d), format(nrow(j), big.mark = ","), 100 * mean(abs(j$d) > 1e-9))
cat("\nReading: a nonzero mean here means the store's tier column is a VINTAGE, and\n")
cat("every consumer that does not re-join the catalogue is adjusting history on\n")
cat("labels from whenever build_stores.R last ran. Zero means the tiers agree and\n")
cat("the 1% gap is elsewhere in .adjust_history_to_target().\n")
fwrite(j, file.path(OUT, "store_tier_vintage.csv"))
