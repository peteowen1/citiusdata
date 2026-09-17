# What is in production, what is in test, and what was decided about each.
#
# WHY THIS EXISTS. On 2026-09-17 we could not answer "is the xgboost our mark
# predictor?" from the repo. The arm had been built (2026-08-04), run, and had
# produced a result showing -3.07% RMSE against its baseline -- and then
# nothing. No entry in DECISIONS.md, no entry in NEXT-STEPS.md, no promotion
# and no rejection, for six weeks. The information to judge it existed on disk
# the whole time; nothing joined it up or made its absence visible.
#
# That is the failure this script exists to make impossible: an arm that has
# been scored but never judged now shows up as **UNJUDGED** every time anyone
# runs this.
#
# IT INVENTS NOTHING. Every row is derived from a record that already exists:
#   * DEPLOYED (scripts/_deployed.R)   -- the production pointer
#   * <cache>/_arm.rds                 -- the 52-field config fingerprint
#                                         backtest_athletics.R already writes
#   * backtest_*.rds `meta` + `overall`-- the scored result and its metrics
# The ONE hand-maintained input is the verdict, because a verdict needs a
# human. That lives in citiusdata/model_registry.csv, which is tracked by git
# (unlike data/), so the decision history is versioned.
#
# Usage:  Rscript citiusdata/scripts/model_registry.R
#         CITIUS_REGISTRY_QUIET=1 to skip the printed report and just write.

VERSE <- here::here()
suppressMessages(library(data.table))
D <- file.path(VERSE, "citiusdata", "data")
VERDICTS <- file.path(VERSE, "citiusdata", "model_registry.csv")
QUIET <- nzchar(Sys.getenv("CITIUS_REGISTRY_QUIET", ""))

# --- production: what DEPLOYED actually points at ----------------------------
source(file.path(VERSE, "citiusdata", "scripts", "_deployed.R"))
prod <- data.table(
  name         = "DEPLOYED",
  kind         = "production",
  calibration  = DEPLOYED$calibration,
  tier_filter  = NA_character_,
  races_scored = NA_integer_,
  gold_skill   = NA_real_,
  medal_skill  = NA_real_,
  run_at       = as.POSIXct(NA),
  evidence     = "scripts/_deployed.R"
)

# --- every scored arm --------------------------------------------------------
res_files <- list.files(D, pattern = "^backtest_.*\\.rds$", full.names = TRUE)
# backtest_cache_*/ are DIRECTORIES of per-meet blobs, not scored results.
res_files <- res_files[!dir.exists(res_files)]
.num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
arms <- rbindlist(lapply(res_files, function(f) {
  r <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(r) || is.null(r$meta)) return(NULL)
  data.table(
    name         = sub("\\.rds$", "", sub("^backtest_", "", basename(f))),
    kind         = "backtest_arm",
    calibration  = r$meta$calibration %||% NA_character_,
    tier_filter  = r$meta$tier_filter %||% NA_character_,
    races_scored = as.integer(r$meta$races_scored %||% NA),
    gold_skill   = .num(r$gold$overall$brier_skill),
    medal_skill  = .num(r$medal$overall$brier_skill),
    run_at       = as.POSIXct(r$meta$run_at %||% NA),
    evidence     = file.path("data", basename(f))
  )
}), fill = TRUE)

# --- caches on disk, so an unscored one cannot hide --------------------------
cache_dirs <- list.dirs(D, recursive = FALSE)
cache_dirs <- cache_dirs[grepl("(backtest_cache_|bt_cache_)", basename(cache_dirs))]
caches <- data.table(
  cache   = basename(cache_dirs),
  n_meets = vapply(cache_dirs, function(p) length(list.files(p, pattern = "\\.rds$")) , integer(1)),
  mb      = round(vapply(cache_dirs, function(p)
              sum(file.size(list.files(p, full.names = TRUE)), na.rm = TRUE)/1024^2, numeric(1)), 1)
)
# Naming convention: CITIUS_BT_CACHE=backtest_cache_X pairs with
# CITIUS_BT_OUT=backtest_X.rds. Where that does not hold the row is reported
# unmatched rather than fuzzily joined -- a wrong pairing here would attach a
# verdict to the wrong arm, which is worse than saying "unmatched".
caches[, arm_guess := sub("^(backtest_cache_|bt_cache_)", "", cache)]
caches[, scored := arm_guess %in% arms$name]

# --- the one hand-kept input -------------------------------------------------
if (file.exists(VERDICTS)) {
  v <- fread(VERDICTS, colClasses = "character")
} else {
  v <- data.table(name = character(), kind = character(), status = character(),
                  verdict = character(), decided_on = character())
  fwrite(v, VERDICTS)
  cli::cli_alert_info("Created an empty {.file model_registry.csv}.")
}

reg <- rbind(prod, arms, fill = TRUE)
reg <- merge(reg, v[, .(name, status, verdict, decided_on)], by = "name", all.x = TRUE)

# Registered models that are NOT backtest arms (the xgb residual arm, a fitted
# artefact, anything scored by its own harness) have no backtest_*.rds to be
# found in, and must not be mistaken for a stale registry entry. Carry them
# through on the strength of the CSV alone -- the point of the registry is that
# a model cannot be invisible just because it is scored somewhere unusual.
extra <- v[!name %in% reg$name & kind != "backtest_arm"]
if (nrow(extra)) {
  reg <- rbind(reg, extra[, .(name, kind, status, verdict, decided_on,
                              evidence = NA_character_)], fill = TRUE)
}

# A VERDICT IS NEVER INFERRED. Only DEPLOYED is derivable as "production";
# everything else without a recorded decision is UNJUDGED, loudly.
#
# The first version of this script also set "matches-production-calibration"
# for any arm whose calibration equalled DEPLOYED's, and that was wrong in the
# exact way this script exists to prevent: a CONTROL arm uses the production
# calibration by construction, so the status sounded decided while saying
# nothing about whether anyone had judged it -- and it hid 15 of the 28
# unjudged arms on the first run. Whether an arm ran on the production
# calibration is a useful fact, so it is kept as its own column, where it
# cannot be mistaken for a verdict.
reg[, on_prod_calibration := !is.na(calibration) & calibration == DEPLOYED$calibration]
reg[name == "DEPLOYED", status := "production"]
reg[is.na(status), status := "UNJUDGED"]
setorder(reg, -run_at, na.last = TRUE)

out <- file.path(D, "model_registry.parquet")
suppressMessages(arrow::write_parquet(reg, out))

if (!QUIET) {
  cat("\n=== PRODUCTION ===\n")
  cat(" stamp      :", DEPLOYED$stamp, "\n")
  cat(" calibration:", DEPLOYED$calibration, "\n")
  cat(" event_params:", DEPLOYED$event_params, " aging:", DEPLOYED$aging, "\n")

  cat("\n=== ARMS, newest first ===\n")
  print(reg[kind == "backtest_arm",
            .(name, status, tier = tier_filter, races = races_scored,
              ctrl = on_prod_calibration,
              gold = round(gold_skill, 4), medal = round(medal_skill, 4),
              run = format(run_at, "%Y-%m-%d"))][1:min(12, .N)])

  nj <- reg[status == "UNJUDGED"]
  if (nrow(nj)) {
    cat(sprintf("\n!! %d SCORED ARM%s WITH NO RECORDED VERDICT\n", nrow(nj),
                if (nrow(nj) == 1) "" else "S"))
    cat("   These ran, produced numbers, and nothing says what was decided.\n")
    cat("   Add a row to citiusdata/model_registry.csv for each.\n")
    print(nj[, .(name, races = races_scored, run = format(run_at, "%Y-%m-%d"))][1:min(12, .N)])
  }

  orph <- caches[scored == FALSE]
  if (nrow(orph)) {
    cat(sprintf("\n!! %d CACHE%s WITH NO SCORED RESULT (%.0f MB)\n", nrow(orph),
                if (nrow(orph) == 1) "" else "S", sum(orph$mb)))
    cat("   Either the arm never finished, or its result is named off-convention.\n")
    print(orph[order(-mb)][1:min(10, .N), .(cache, n_meets, mb)])
  }

  stale <- setdiff(v$name, reg$name)
  if (length(stale)) {
    cat("\n!! registry names with nothing on disk:", paste(stale, collapse = ", "), "\n")
  }

  cat(sprintf("\ntotal cache footprint: %.0f MB across %d cache%s\n",
              sum(caches$mb), nrow(caches), if (nrow(caches) == 1) "" else "s"))
  cat("wrote", basename(out), "\n")
}
