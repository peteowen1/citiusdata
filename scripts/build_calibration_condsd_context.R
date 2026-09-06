# Attach a CONTEXT-CONDITIONAL shared-shock sd to a calibration.
#
# THE DEFECT (2026-09-06, docs/reviews/spread-and-level-in-t1-finals-2026-09-06.md).
# `calibration$events$condition_sd` is one number per event, fitted on every
# race in the corpus. Measured on T1 finals with the full deployed simulation,
# the race-to-race variance those finals actually show is 16-71% of
# condition_sd^2 by family (hurdles 0.16, throw 0.18, distance 0.17, sprint
# 0.71): a championship final shares far less than a heat or a low-tier meet,
# and the 50% interval covers 62.5% of marks. Neither sigma knob moves this.
#
# WHAT THIS BUILDS. From the SAME fitted race effects (`calibration$race`:
# c_r, n_in_race, round, per race), keyed by the catalogue's meet_tier (joined
# on the competition id at the front of race_key; 100% of races match) and the
# round class, the de-biased shared-shock variance per cell -- the same
# estimator calibrate() uses for the event-wide value: var(c_r) minus the
# sampling noise mean(sigma_within^2 / n_in_race). Cells:
#   event  x meet_tier x round_class   shrunk toward
#   family x meet_tier x round_class   shrunk toward
#   family (all contexts)
# with a pseudo-count of M races (default 20; a smoothing count, not a fitted
# quantity -- flagged, and PIT coverage is the judge of the whole table).
#
# The result is `calibration$condition_sd_context`, read by
# race_conditions(event_id, calibration, context = list(meet_tier, round_class))
# when simulate_event() is given the race's context. Without a context the
# event-wide value is used exactly as before, so a calibration carrying this
# table changes nothing until a caller opts in.
#
# Same "attach a table to a calibration" shape as build_calibration_coast_only.R.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/build_calibration_condsd_context.R'
# Env:
#   CITIUS_CTXSD_SRC           source calibration (default the deployed one)
#   CITIUS_CTXSD_OUT           output file (default <src>_ctxsd.rds)
#   CITIUS_CTXSD_PSEUDO_RACES  shrinkage pseudo-count (default 20)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
OUT <- here::here("citiusdata", "data")
SRC <- Sys.getenv("CITIUS_CTXSD_SRC", "calibration_corpus_wac_coast_0904.rds")
DST <- Sys.getenv("CITIUS_CTXSD_OUT", sub("\\.rds$", "_ctxsd.rds", SRC))
M   <- as.numeric(Sys.getenv("CITIUS_CTXSD_PSEUDO_RACES", "20"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
stopifnot(is.finite(M), M >= 0, DST != SRC)

cal <- readRDS(file.path(OUT, SRC))
stopifnot(inherits(cal, "citius_calibration"), !is.null(cal$race), !is.null(cal$events))
r <- as.data.table(cal$race)[is.finite(c_r) & n_in_race >= 2L]
say("source %s: %s races with a fitted shared effect", SRC, format(nrow(r), big.mark = ","))

# Catalogue tier by competition id (the first field of race_key).
r[, comp_id := sub("\\|.*$", "", race_key)]
ct <- as.data.table(open_dataset(file.path(OUT, "competition_catalogue.parquet")) |>
                      dplyr::select(competition_id, meet_tier) |> dplyr::collect())
ct[, competition_id := as.character(competition_id)]
ct <- unique(ct[!is.na(meet_tier)], by = "competition_id")
r <- merge(r, ct, by.x = "comp_id", by.y = "competition_id", all.x = TRUE)
cov <- mean(!is.na(r$meet_tier))
say("catalogue meet_tier on %.1f%% of races", 100 * cov)
stopifnot("meet_tier coverage below 95%" = cov > 0.95)
r <- r[!is.na(meet_tier)]
r[, round_class := .round_class(round)]

ev <- as.data.table(cal$events)[, .(event_id, sigma_within, condition_sd)]
r <- merge(r, ev, by = "event_id")
r <- r[is.finite(sigma_within)]
reg <- as.data.table(citius_events())[, .(event_id, family)]
r <- merge(r, reg, by = "event_id")
say("%s races across %d events, %d tiers, %d round classes",
    format(nrow(r), big.mark = ","), uniqueN(r$event_id), uniqueN(r$meet_tier), uniqueN(r$round_class))

debias_var <- function(c_r, sw, n) {
  if (length(c_r) < 2L) return(NA_real_)
  max(stats::var(c_r) - mean(sw^2 / n), 0)
}
glob_fam <- r[, .(var_glob = debias_var(c_r, sigma_within, n_in_race)), by = family]
cell_fam <- r[, .(n_races = .N, var_raw = debias_var(c_r, sigma_within, n_in_race)),
              by = .(family, meet_tier, round_class)]
cell_fam <- merge(cell_fam, glob_fam, by = "family")
# A cell with one race has no variance estimate: it takes its parent outright.
cell_fam[is.na(var_raw), var_raw := var_glob]
cell_fam[, var_shr := (n_races * var_raw + M * var_glob) / (n_races + M)]
cell_ev <- r[, .(n_races = .N, var_raw = debias_var(c_r, sigma_within, n_in_race)),
             by = .(event_id, family, meet_tier, round_class)]
cell_ev <- merge(cell_ev, cell_fam[, .(family, meet_tier, round_class, var_parent = var_shr)],
                 by = c("family", "meet_tier", "round_class"), all.x = TRUE)
cell_ev <- merge(cell_ev, glob_fam, by = "family", all.x = TRUE)
cell_ev[is.na(var_parent), var_parent := var_glob]
cell_ev[is.na(var_raw), var_raw := var_parent]
cell_ev[, var_shr := (n_races * var_raw + M * var_parent) / (n_races + M)]

tbl <- rbind(
  cell_ev[, .(level = "event", event_id, family, meet_tier, round_class, n_races,
              cond_sd_raw = sqrt(var_raw), cond_sd = sqrt(var_shr))],
  cell_fam[, .(level = "family", event_id = NA_character_, family, meet_tier, round_class, n_races,
               cond_sd_raw = sqrt(var_raw), cond_sd = sqrt(var_shr))])
bad <- tbl[!is.finite(cond_sd) | cond_sd < 0]
if (nrow(bad)) { print(bad); stop(nrow(bad), " cells have no finite cond_sd") }
setorder(tbl, level, family, meet_tier, round_class, event_id)

# --- what it says, before it is written -----------------------------------------
cat("\n=== T1_elite finals: context cond_sd vs the event-wide value (largest events) ===\n")
show <- merge(tbl[level == "event" & meet_tier == "T1_elite" & round_class == "final"],
              ev[, .(event_id, cond_sd_global = condition_sd)], by = "event_id")
show[, ratio := round(cond_sd / cond_sd_global, 3)]
print(show[order(-n_races)][1:24, .(event_id, n_races, cond_sd_global = round(cond_sd_global, 5),
                                    cond_sd_ctx = round(cond_sd, 5), ratio)])
cat("\n=== family x tier x round (T1 and T2 finals/heats), ratio to the family-wide value ===\n")
fs <- merge(cell_fam, glob_fam, by = c("family"), suffixes = c("", ".g"))
fs[, ratio := round(sqrt(var_shr / var_glob.g), 3)]
print(dcast(fs[meet_tier %in% c("T1_elite", "T2_strong") & round_class %in% c("final", "heat")],
            family ~ meet_tier + round_class, value.var = "ratio"))

cal$condition_sd_context <- tbl
cal$provenance$condition_sd_context <- list(src = SRC, pseudo_races = M, built = Sys.time(),
                                            races = nrow(r), catalogue_coverage = cov)
saveRDS(cal, file.path(OUT, DST))
fwrite(tbl, file.path(OUT, sub("\\.rds$", ".csv", paste0("condition_sd_context_", DST))))
say("wrote %s (%d cells: %d event, %d family)", DST, nrow(tbl), sum(tbl$level == "event"), sum(tbl$level == "family"))
