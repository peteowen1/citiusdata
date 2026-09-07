# MARKS LAB PREP, CHUNKED BY FAMILY: build the cache in ~1 GB instead of ~4.
#
# WHY A SECOND PREP RATHER THAN A CHANGED ONE. marks_lab_prep.R works and built
# the T1 cache every number in docs/reviews/marks-optimisation-2026-09-07.md
# rests on. Rewriting it to fit a memory limit risks changing those numbers
# silently. This writes a different cache directory and leaves that path alone.
#
# THE PROBLEM. The original holds the whole history at once:
#
#   h   <- read_results_store(all events, 12 years)      ~900k rows
#   adj <- .adjust_history_to_target(copy(h), ...)       a SECOND copy of it
#   ... then a month loop that subsets h again
#
# Peak is roughly two full histories plus the 4.5M-row outcomes table. On a
# machine with 1.4 GB free that is not a tuning problem, it is a structural one,
# and no gate low enough to start will make it fit.
#
# THE SEAM IS THE FAMILY. Every expensive step inside the original already works
# per family -- the sigma loop literally does `split(pf, pf$family)` and calls
# estimate_ability() on each -- because a half-life is per family and an event
# belongs to exactly one. So the only thing holding all nine at once is the
# READ. Chunking it means peak memory is the largest single family (jump,
# 262k marks) rather than all of them, and the store is read nine times instead
# of once, which costs seconds against a 127 MB parquet store.
#
# WHAT IS PRESERVED, and why it is safe:
#   - prior_mu and sigma_between are event-level, computed inside
#     estimate_ability from the rows it is given. An event lives in one family,
#     so chunking by family cannot change them.
#   - the robust-sigma scale `k` is a population median WITHIN a call. The
#     original already calls per family, so this changes nothing there either.
#   - the last-5 baseline is per athlete-event. Also unaffected.
# Chunking by EVENT would break the first of those; chunking by family does not.
#
# Resumable at the (family, month) level, so a kill costs one family-month.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/marks_lab_prep_chunked.R'
# Env: CITIUS_LAB_TIERS, CITIUS_LAB_CACHE, CITIUS_LAB_FROM/_TO, CITIUS_LAB_CAL
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
FROM  <- as.Date(Sys.getenv("CITIUS_LAB_FROM", "2020-01-01"))
TO    <- as.Date(Sys.getenv("CITIUS_LAB_TO", "2026-09-01"))
CAL   <- Sys.getenv("CITIUS_LAB_CAL", DEPLOYED$calibration)
CACHE <- file.path(OUT, Sys.getenv("CITIUS_LAB_CACHE", "marks_lab_cache_t1t2"))
TIERS <- trimws(strsplit(Sys.getenv("CITIUS_LAB_TIERS", "T1_elite,T2_strong"), ",")[[1]])
dir.create(file.path(CACHE, "sigma"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(CACHE, "fam"),   recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
rss <- function() {
  z <- tryCatch(sum(gc()[, 2]), error = function(e) NA_real_)
  sprintf("%.0f MB", z)
}
cal <- readRDS(file.path(OUT, CAL))
STORE <- file.path(OUT, "athletics_corpus_store")

# --- test set, read straight out of the store -------------------------------
tf <- file.path(CACHE, "test.rds")
if (!file.exists(tf)) {
  # BUILT FROM THE PARQUET STORE, NOT championship_results.rds.
  #
  # The original prep reads that 4.5M-row, 33-column RDS purely to construct the
  # test set, and it is the largest single object the whole pipeline touches --
  # on the order of a gigabyte, before any history is loaded. The store carries
  # every column needed (race_key, athlete_id, event_id, date, mark, place,
  # round, meet_tier, tier) and arrow selects and filters lazily, so the only
  # thing that reaches R is the ~600k rows that survive.
  #
  # It also removes the catalogue join: meet_tier is already on the store rows,
  # where the original had to merge competition_catalogue.parquet to get it.
  say("building the test set from the store (RSS %s)", rss())
  test <- as.data.table(
    open_dataset(STORE) |>
      dplyr::select(race_key, athlete_id, event_id, date, mark, place, round,
                    meet_tier, tier, competition_id) |>
      dplyr::filter(!is.na(mark), !is.na(race_key), !is.na(event_id),
                    !is.na(place), place > 0, !is.na(meet_tier),
                    meet_tier %in% TIERS, date >= FROM, date < TO) |>
      dplyr::collect())
  say("  %s candidate rows from the store (RSS %s)",
      format(nrow(test), big.mark = ","), rss())
  setnames(test, "tier", "race_tier")
  test[, `:=`(athlete_id = as.character(athlete_id), date = as.Date(date))]
  test <- test[grepl("final", tolower(round)) & !grepl("semi|quarter", tolower(round))]
  test <- unique(test, by = c("race_key", "athlete_id"))
  reg <- as.data.table(citius_events())[, .(event_id, orientation, family)]
  test <- merge(test, reg, by = "event_id")[, act := orientation * log(mark)][is.finite(act)]
  test[, month := as.Date(format(date, "%Y-%m-01"))]
  saveRDS(test[, .(race_key, athlete_id, event_id, family, date, month, act,
                   meet_tier, race_tier)], tf)
  say("test set: %s rows, %s races, %d events, %d months, tiers %s",
      format(nrow(test), big.mark = ","), format(uniqueN(test$race_key), big.mark = ","),
      uniqueN(test$event_id), uniqueN(test$month), paste(TIERS, collapse = "+"))
  rm(test, reg); invisible(gc())
}
test <- readRDS(tf)
months <- sort(unique(test$month))
FAMS <- sort(unique(test$family[!is.na(test$family) & nzchar(test$family)]))
say("%d families to process: %s (RSS %s)", length(FAMS), paste(FAMS, collapse = ", "), rss())

cols <- intersect(c("athlete_id", "event_id", "date", "perf", "age", "round", "tier",
                    "meet_tier", "competition_id", "race_key", "wind", "momentum",
                    "indoor", "venue_country"),
                  names(open_dataset(STORE)))

# --- one family at a time ----------------------------------------------------
for (fam in FAMS) {
  fam_events <- unique(test[family == fam]$event_id)
  af <- file.path(CACHE, "fam", sprintf("adj_%s.rds", fam))
  bf <- file.path(CACHE, "fam", sprintf("base_%s.rds", fam))
  sig_needed <- months[!file.exists(file.path(CACHE, "sigma",
                                              sprintf("%s_%s.rds", fam, format(months))))]
  if (file.exists(af) && file.exists(bf) && !length(sig_needed)) {
    say("%-9s already complete, skipping", fam); next
  }
  t0 <- Sys.time()
  h <- as.data.table(read_results_store(STORE, events = fam_events,
                                        from = FROM - DEPLOYED$history_days, to = TO,
                                        columns = cols))
  h <- flag_implausible(h)[is.finite(perf)]
  h[, athlete_id := as.character(athlete_id)][, date := as.Date(date)]
  say("%-9s %s history rows in %.0fs (RSS %s)", fam,
      format(nrow(h), big.mark = ","),
      as.numeric(difftime(Sys.time(), t0, units = "secs")), rss())

  if (!file.exists(bf)) {
    pk <- unique(test[family == fam, .(athlete_id, event_id, date)])
    jj <- h[, .(athlete_id, event_id, date, perf)][pk, on = .(athlete_id, event_id),
                                                   allow.cartesian = TRUE][date < i.date]
    setorder(jj, athlete_id, event_id, i.date, -date)
    jj[, rk := seq_len(.N), by = .(athlete_id, event_id, i.date)]
    b5 <- jj[rk <= 5, .(base = mean(perf), n_prior = .N),
             by = .(athlete_id, event_id, date = i.date)]
    saveRDS(b5[n_prior >= 3L], bf)
    rm(pk, jj, b5); invisible(gc())
  }
  if (!file.exists(af)) {
    raw_perf <- h$perf
    adj <- citius:::.adjust_history_to_target(copy(h), cal, isTRUE(DEPLOYED$adjust_race))
    adj[, perf_raw := raw_perf]
    adj[, w_static := result_weight(date, tier = tier, round = round, as_of = TO,
                                    half_life = Inf, calibration = cal,
                                    tier_class = citius:::.tier_class_of(adj))]
    adj <- adj[is.finite(perf) & is.finite(perf_raw) & is.finite(w_static) & w_static > 0,
               .(athlete_id, event_id, date, perf, perf_raw, w_static)]
    adj[, family := fam]
    saveRDS(adj, af)
    rm(adj, raw_perf); invisible(gc())
  }
  hl <- if (fam %in% names(DEPLOYED$hl_family)) DEPLOYED$hl_family[[fam]] else DEPLOYED$half_life
  for (m in sig_needed) {
    cut <- as.Date(m, origin = "1970-01-01")
    sf <- file.path(CACHE, "sigma", sprintf("%s_%s.rds", fam, format(cut)))
    want <- unique(test[family == fam & month == cut]$athlete_id)
    if (!length(want)) { saveRDS(data.table(), sf); next }
    past <- h[date < cut]
    if (!nrow(past)) { saveRDS(data.table(), sf); next }
    ab <- estimate_ability(past, as_of = cut, half_life = hl, calibration = cal,
                           adjust_race = isTRUE(DEPLOYED$adjust_race), only = want)
    saveRDS(ab[, .(athlete_id = as.character(athlete_id), event_id, sigma,
                   ref_ability = ability, ref_shrinkage = shrinkage, ref_w = w_total)], sf)
    rm(past, ab); invisible(gc())
  }
  say("%-9s done in %.0fs (RSS %s)", fam,
      as.numeric(difftime(Sys.time(), t0, units = "secs")), rss())
  rm(h); invisible(gc())
}
# PRINT THE RUNNER'S SENTINEL, NOT JUST A HUMAN-READABLE ONE.
#
# _run_t2_lab_build.ps1 breaks its retry loop on `Select-String -Pattern
# "PREP COMPLETE"`, which is what marks_lab_prep.R printed. This script was
# written as a drop-in replacement for that one and preserved every DATA
# interface -- same cache directory, same file names, same columns, same
# resumability -- while silently dropping the one string the runner reads.
#
# The consequence was not a wrong number, it was a wasted build: all nine
# families complete correctly, the runner then loops eleven no-op passes, exits
# 1, and never runs marks_pairs.R or build_fair_baseline.R. A perfect cache
# reporting failure, with nothing in the cache to suggest where to look.
#
# So the completion marker is emitted in BOTH forms. A sentinel read by a
# machine costs nothing to duplicate, and duplicating it means the next
# replacement of this script cannot break the runner by rewording a log line.
say("ALL FAMILIES COMPLETE (RSS %s)", rss())
say("PREP COMPLETE: %d families, %d sigma files (RSS %s)",
    length(FAMS),
    length(list.files(file.path(CACHE, "sigma"), pattern = "\\.rds$")),
    rss())
