# THE DEPLOYED CONFIGURATION -- one definition, sourced by every script that
# ships a number.
#
# Why this file exists. On 2026-07-31 an audit found that all five shipping
# scripts were running a materially different model from the one the backtest
# had validated: the harvest (308k rows) instead of the corpus (4.99M), a
# calibration dated three vintages back, and no field prior or aging projection
# at all. Nothing was broken and nothing errored -- each script simply named its
# own inputs as literals, so promoting a winner meant editing five files and
# forgetting one was invisible.
#
# The same failure had already happened twice at smaller scale (wind silently
# dropped when the corpus was promoted; a widened sigma written to a field
# nothing reads). Both were recorded as lessons; neither changed the structure
# that produced them. This does.
#
# RULE: no shipping script names a model input directly. If a script needs an
# artefact or a parameter, it comes from DEPLOYED, and promoting a change is a
# one-line edit here.

DEPLOYED <- list(
  # Bump on every promotion. Written into prediction outputs so any artefact can
  # be traced to the configuration that produced it.
  stamp = "2026-07-31 csigma",

  # HISTORY -- what the model learns from.
  # The corpus is worth 10-50x every parameter change of the week combined:
  # paired on 28,737 common marks with aging, cohort and outcomes held fixed,
  # marks MAE 2.216% -> 2.007% (t = +23.8), gold Brier t = +18.5, medal t = +24.2.
  # Mean w_total 1.59 -> 7.47. Read through the partitioned store: 0.39s per
  # query against 46.1s for the .rds, because every query filters on event_id.
  history_store = "athletics_corpus_store",
  history_rds   = "athletics_corpus.rds",

  # CALIBRATION -- corpus + wind + sigma rescaled to the forecast context.
  # csigma is the largest placings win recorded: gold Brier 0.05571 -> 0.05428
  # (t = -13.63, p = 3.4e-42), medal 0.12511 -> 0.12354, marks unmoved. Marks
  # staying flat is the point -- sigma is a spread parameter, not a location one.
  calibration = "calibration_corpus_csigma.rds",

  # AGING -- the blended curve, adopted 2026-07-29.
  aging = "aging.rds",

  # HALF-LIFE. 365 selected by A/B on out-of-sample ranking skill over 5,872
  # races, not by fit_half_life() (which optimises next-result MAE and returns
  # 90 for sprints). Road and walk are varied because those two families have
  # real evidence: t = +8.24 on marks, p = 1.8e-16. The mechanism is race
  # FREQUENCY, not physiology -- a marathoner races twice a year.
  half_life = 365,
  hl_family = c(road = 1095, walk = 730),

  # FIELD PRIOR. Shrink a thinly-evidenced entrant toward the FIELD rather than
  # the unconditional event mean, which includes a long tail of athletes who
  # never contest a final. Weight 0.5: marks MAE 2.642% -> 2.533%, medal Brier
  # t = +8.33, no significant gold cost. Weight 1.0 predicts marks best but
  # damages gold (t = -6.34) by compressing the field.
  prior_weight = 0.5,

  # History depth per estimate. TWELVE YEARS, and do not shorten it on the
  # argument that old marks carry negligible weight -- w_total is a SUM and it
  # drives shrinkage. Cutting to seven years moved p_gold by up to 0.246.
  history_days = 4380L
)

# --- accessors ---------------------------------------------------------------

deployed_calibration <- function(dir) readRDS(file.path(dir, DEPLOYED$calibration))

deployed_aging <- function(dir) {
  f <- file.path(dir, DEPLOYED$aging)
  if (!file.exists(f)) {
    cli::cli_abort("Deployed aging curve {.file {DEPLOYED$aging}} is missing.")
  }
  readRDS(f)
}

#' Read deployed history for a set of events and a date window.
#'
#' The store is a derived read layer over `history_rds`; rebuilding it is cheap
#' and it is the only route fast enough to use during a live meet.
#'
#' TODO(pete): decide the missing-store behaviour -- see the note below.
deployed_history <- function(dir, events, from, to) {
  store <- file.path(dir, DEPLOYED$history_store)
  if (dir.exists(store)) {
    return(read_results_store(store, events = events, from = from, to = to))
  }
  .deployed_history_fallback(dir, events, from, to)
}

# What happens when the deployed store is absent (Pete's call, 2026-07-31).
#
# The original framing was abort-versus-degrade, and it was a false choice. The
# store is a BUILD ARTEFACT derived from `history_rds`, not an input: during a
# Games it is built once from pre-Games data and never changes, so a missing
# store is a setup error rather than a runtime condition. The answer is to
# rebuild it and carry on.
#
# What is NOT acceptable is the silent degrade. Dropping to the 308k harvest
# store is how the shipping path came to run a materially worse model for a week
# while producing entirely plausible numbers. Better data slowly beats worse data
# quickly, and no data at all beats worse data presented as good.
#
# Order of preference:
#   1. rebuild the store from the deployed history and read it (minutes, once)
#   2. read the deployed .rds directly -- correct data, ~46s per query
#   3. abort, naming the command that fixes it
.deployed_history_fallback <- function(dir, events, from, to) {
  src <- file.path(dir, DEPLOYED$history_rds)
  if (!file.exists(src)) {
    cli::cli_abort(c(
      "Neither {.file {DEPLOYED$history_store}} nor {.file {DEPLOYED$history_rds}} exists.",
      i = "Deployed history is missing entirely; there is nothing correct to predict from.",
      i = "Fetch the release assets, then run {.code Rscript scripts/build_stores.R}."
    ))
  }
  cli::cli_alert_warning(
    "Deployed store {.file {DEPLOYED$history_store}} is missing; rebuilding it from {.file {DEPLOYED$history_rds}}."
  )
  ok <- tryCatch({
    d <- flag_implausible(data.table::setDT(readRDS(src)))
    keep <- c("athlete_id", "event_id", "date", "perf", "mark", "age", "round",
              "tier", "competition_id", "comp_start", "place", "race_key",
              "sex", "discipline", "wind", "indoor", "comp_name")
    d <- d[, intersect(keep, names(d)), with = FALSE]
    data.table::setorderv(d, intersect(c("event_id", "date"), names(d)))
    write_results_store(d, file.path(dir, DEPLOYED$history_store))
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning("Rebuild failed ({conditionMessage(e)}); reading the .rds directly.")
    FALSE
  })
  if (ok) {
    cli::cli_alert_success("Store rebuilt. Predictions are on the deployed history.")
    return(read_results_store(file.path(dir, DEPLOYED$history_store),
                              events = events, from = from, to = to))
  }
  # Correct data, slow path. flag_implausible() is global, so it runs on the
  # whole table before slicing -- applying it to a slice computes different
  # thresholds and silently flags different marks.
  d <- flag_implausible(data.table::setDT(readRDS(src)))
  d[d$event_id %in% events & d$date >= from & d$date <= to]
}

#' Estimate ability the validated way: per-family half-life.
#'
#' `estimate_ability()` takes a single half-life, so the history is split by
#' family and stacked. Each event belongs to exactly one family, so no
#' athlete-event is estimated twice.
deployed_ability <- function(past, as_of, calibration) {
  hl_map <- DEPLOYED$hl_family
  if (!length(hl_map)) {
    return(estimate_ability(past, as_of = as_of, half_life = DEPLOYED$half_life,
                            calibration = calibration))
  }
  reg_f <- data.table::as.data.table(citius_events()[, c("event_id", "family")])
  pf <- merge(data.table::as.data.table(past), reg_f, by = "event_id", all.x = TRUE)
  data.table::rbindlist(lapply(split(pf, pf$family), function(g) {
    fam <- g$family[1]
    hl <- if (!is.na(fam) && fam %in% names(hl_map)) hl_map[[fam]] else DEPLOYED$half_life
    estimate_ability(g[, !"family"], as_of = as_of, half_life = hl,
                     calibration = calibration)
  }), fill = TRUE)
}

#' Apply the field-conditional prior and the aging projection to one race field.
#'
#' ORDER MATTERS and matches the backtest: prior first, then aging.
#' `project_ability()` scales its shift by (1 - shrinkage), so it must see the
#' shrinkage the prior produced, not the unconditional one.
#'
#' @param entrants ability rows for this field, in the order to simulate in
#' @param ages named vector or data.table of athlete_id -> age on the day, or
#'   NULL to skip the aging projection
deployed_field <- function(entrants, aging = NULL, ages = NULL) {
  entrants <- data.table::as.data.table(entrants)
  if (DEPLOYED$prior_weight > 0) {
    entrants <- condition_prior(entrants, field = entrants$athlete_id,
                                weight = DEPLOYED$prior_weight)
  }
  if (!is.null(aging) && !is.null(ages) && nrow(ages)) {
    entrants[data.table::as.data.table(ages), on = "athlete_id", age_now := i.age_now]
    ok <- entrants[!is.na(age_now) & !is.na(age_ref)]
    if (nrow(ok)) {
      proj <- suppressWarnings(project_ability(ok, aging))
      entrants[proj, on = "athlete_id", ability := i.ability]
    }
  }
  entrants[]
}
