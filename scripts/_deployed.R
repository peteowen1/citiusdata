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


#' A name key that survives being rendered differently by two sources
#'
#' The World Athletics feed writes "Camryn Rogers"; the results system writes
#' "Camryn ROGERS". Deduplicating the union of the two on `athlete_name` matches
#' nothing, so every podium present in both sources appeared twice -- visibly, as
#' two golds in the same event. Compare an order-free, case-free set of name
#' tokens instead, which also absorbs "Meshack Kitsubuli BABU" against
#' "Meshack Kitsubuli Babu".
.name_key <- function(x) {
  x <- toupper(trimws(gsub("[[:space:]]+", " ", gsub("[^A-Za-z ]", " ", x))))
  vapply(strsplit(x, " ", fixed = TRUE),
         function(w) paste(sort(w[nzchar(w)]), collapse = "|"), character(1))
}

#' Trust the page title over the route it was captured under
#'
#' The results app is a single-page app: a capture sets `location.hash`, waits,
#' then reads the DOM. If the render has not finished, the DOM still holds the
#' PREVIOUS page while the route says otherwise, and the rows are filed under the
#' wrong event. A staleness check that only rejects the schedule shell will not
#' catch this, because what was captured is a perfectly valid results page --
#' just the wrong one.
#'
#' Sweep 2 filed the women's 200m individual medley final under the men's route,
#' putting Jenna Forrester's 2:09.24 in the same event as Duncan Scott's 1:56.38.
#' The heading on the captured page said WOMEN'S, so the disagreement is
#' detectable and repairable: the title describes what is on the page, the route
#' only describes what was asked for.
.repair_sex_from_title <- function(g) {
  g <- data.table::as.data.table(g)
  if (!all(c("title", "sex", "event_id") %in% names(g))) return(g[])
  # Check WOMEN first -- "WOMEN'S" contains "MEN'S" -- which then makes a plain
  # "MEN" test safe. It must be plain: "\bMEN" in an R string is a BACKSPACE
  # followed by MEN, not a word boundary, so it matches nothing and silently
  # repaired only the women's-filed-as-men's direction.
  tsex <- data.table::fifelse(grepl("WOMEN", g$title, ignore.case = TRUE), "W",
          data.table::fifelse(grepl("MEN", g$title, ignore.case = TRUE), "M",
                              NA_character_))
  bad <- !is.na(tsex) & !is.na(g$sex) & tsex != g$sex & !is.na(g$event_id)
  if (any(bad)) {
    g[bad & tsex == "W", event_id := sub("-M$", "-W", event_id)]
    g[bad & tsex == "M", event_id := sub("-W$", "-M", event_id)]
    g[bad, sex := tsex[bad]]
    data.table::setattr(g, "sex_repaired", sum(bad))
  }
  g[]
}

#' The Glasgow 2026 swimming capture, as a single table
#'
#' The Commonwealth results system is scraped by hand, and it was scraped twice.
#' The first sweep (27 July) ran while the meet was still going and holds 17 of
#' the 34 individual events; the second (4 August) re-walked every schedule day
#' and picked up 27, including five the first sweep never listed. Neither is a
#' superset of the other, so **both are read and the union is what counts**.
#'
#' Every caller must go through this rather than naming a file. Five scripts
#' independently hardcoded `glasgow2026_swimming.json`, so a second capture
#' would otherwise have improved coverage for whichever one was edited and left
#' the rest quietly reading half the meet.
glasgow_swimming <- function(dir) {
  fs <- file.path(dir, c("glasgow2026_swimming.json",
                         "glasgow2026_swimming_sweep2.json",
                         "glasgow2026_swimming_sweep3.json",
                         # Carries the women's 1500m freestyle, whose plain
                         # /FNL-/000100 route is empty because the final was swum
                         # in two sections.
                         "glasgow2026_gapfill.json"))
  fs <- fs[file.exists(fs)]
  if (!length(fs)) stop("no Glasgow swimming capture found in ", dir)
  m <- data.table::rbindlist(
    lapply(fs, function(f) .repair_sex_from_title(parse_crs_export(f))),
    fill = TRUE)
  m <- m[is.na(event_id) | grepl("^SW-", event_id)]   # gapfill also holds athletics
  # Normalise the round before deduplicating. The same final was captured by more
  # than one sweep under different labels -- "Final", "Final - Fastest Heat",
  # "Final - Slowest Heat 1" -- so keying on the raw label let one race
  # contribute two or three podiums. An athlete swims a given round once.
  m[, .nk := .name_key(athlete_name)]
  m[, .rk := data.table::fifelse(
      grepl("final", round, ignore.case = TRUE) &
        !grepl("semi|qual", round, ignore.case = TRUE), "Final", round)]
  key <- c("event_id", ".rk", ".nk")
  m <- m[!duplicated(m[, ..key])]
  m[, c(".nk", ".rk") := NULL]

  # Distance finals are swum in SECTIONS -- "Final - Slowest Heat 1" and
  # "Final - Fastest Heat" are both the final, and rank restarts in each. Taken
  # at face value the slow section contributes its own 1-2-3, so a swimmer who
  # finished eleventh overall appears on the podium. Re-rank the whole final on
  # time, which is how the medals are actually decided.
  isfin <- grepl("final", m$round, ignore.case = TRUE) &
           !grepl("semi|qual", m$round, ignore.case = TRUE)
  sect <- m[isfin & !is.na(event_id), .N, by = .(event_id, round)][
    , .(nsect = .N), by = event_id][nsect > 1L]
  if (nrow(sect)) {
    for (ev in sect$event_id) {
      i <- which(isfin & m$event_id == ev & is.finite(m$mark))
      if (!length(i)) next
      ord <- order(m$mark[i])                       # lower time is better
      m$place[i[ord]] <- seq_along(ord)
      m$round[i] <- "Final"
    }
  }
  m[]
}


#' The Glasgow 2026 athletics results, feed and results-system combined
#'
#' Athletics is normally taken from the World Athletics feed, which is why the
#' CRS recipe says not to scrape it. The feed did not deliver for Glasgow: it
#' carries four competition days (27, 28, 30, 31 July) and never populated 29
#' July or 1 August, so 16 of the 40 individual events had no final -- both
#' Miles, both long jumps, 400m men and women, the men's 5000m among them.
#' Re-querying on 4 August returned the identical 1,000 rows.
#'
#' The results system has them, so both are read and the union is returned. The
#' feed is preferred where an event appears in both: it carries the full
#' canonical schema, while the CRS is a rendered-page scrape.
glasgow_athletics <- function(dir) {
  fromfeed <- readRDS(file.path(dir, "glasgow2026_results.rds"))
  fromfeed <- data.table::as.data.table(fromfeed)
  fromfeed[, source_feed := "wa"]
  f <- file.path(dir, "glasgow2026_athletics_crs.rds")
  crs <- if (file.exists(f)) data.table::as.data.table(readRDS(f)) else NULL
  # The gap-fill carries the two athletics events the sweeps could not reach:
  # the women's heptathlon, whose overall points live on the `athletic-summaries`
  # route family rather than `athletic-result`, and the women's 10,000m walk.
  gf <- file.path(dir, "glasgow2026_gapfill.json")
  if (file.exists(gf)) {
    g <- .repair_sex_from_title(parse_crs_export(gf))
    g <- g[!is.na(event_id) & grepl("^AT-", event_id)]
    crs <- if (is.null(crs)) g else data.table::rbindlist(list(crs, g), fill = TRUE)
  }
  if (is.null(crs) || !nrow(crs)) return(fromfeed[])
  crs[, source_feed := "crs"]

  # Drop scraped rows whose mark cannot be a mark for the event. Sprints and
  # hurdles split the results header over three lines, so a parser reading only
  # the "Rank..." line falls back to the first value it finds -- the REACTION
  # TIME -- and files 0.196 as a 400m. Nobody runs a lap in a fifth of a second,
  # and no throw measures under a metre, so this is decidable rather than a
  # judgement call. Feed rows are never dropped; they are the trustworthy source.
  ori <- data.table::as.data.table(citius_events())[, .(event_id, .ori = orientation)]
  crs <- merge(crs, ori, by = "event_id", all.x = TRUE)
  impossible <- is.finite(crs$mark) & !is.na(crs$.ori) &
    ((crs$.ori == -1L & crs$mark < 5) | (crs$.ori == 1L & crs$mark < 1))
  if (any(impossible)) {
    data.table::setattr(crs, "impossible_dropped", sum(impossible))
    crs <- crs[!impossible]
  }
  crs[, .ori := NULL]
  keep <- intersect(names(fromfeed), names(crs))
  both <- rbind(fromfeed[, ..keep], crs[, ..keep], fill = TRUE)
  # Feed rows win on collision: same event, round, athlete and mark. The athlete
  # is compared on a normalised name key, NOT the raw string -- the two sources
  # render the same person differently and a raw comparison deduplicates nothing.
  # Key on event + round + athlete, NOT the mark. Including the mark makes a
  # WRONG mark look like a different row and survive deduplication: the results
  # system reports a reaction time where the feed reports a finishing time for
  # sprints and hurdles, so Emmanuel Eseme appeared twice in the 100m final, once
  # at 9.83 and once at 0.139. One athlete cannot finish a round twice, so the
  # athlete is the key and the feed's value is the one kept.
  both[, .nk := .name_key(athlete_name)]
  key <- c("event_id", "round", ".nk")
  both <- both[order(match(source_feed, c("wa", "crs")))]
  both <- both[!duplicated(both[, ..key])]
  both[, .nk := NULL]
  both[]
}


#' Refuse a per-athlete spread that no athlete could have
#'
#' A merged identity shows up as one number: a fitted `sigma` far above what the
#' event itself measures. Guy BROOKS reached the Glasgow card at 43% on a median
#' 25th place because "Guy BROOKS" and "George Brooks" share the surname-plus-
#' initial loose key and were linked into one person_id (`BROOKSGEORGE`), giving
#' a sigma of 0.7897 where the event median is 0.008 -- ninety-nine times too
#' big. A race is decided by the MINIMUM, so spread converts directly into win
#' probability and a physically impossible sigma buys a podium place outright.
#'
#' This is a guard, not the fix. The fix is in the crosswalk, which should not
#' merge Guy with George; until that lands, an athlete whose spread exceeds the
#' field's by this margin is treated as UNRATED rather than as a live threat.
#' The threshold is set against the event's own fitted distribution rather than
#' as an absolute, so it carries no assumption about a sport's scale: legitimate
#' outliers here run to about 1.7x the median, so 10x is far outside anything
#' real and well inside the defect.
#'
#' @param ab An `estimate_ability()` table.
#' @param factor How many times the event's median sigma is still credible.
#' @return `ab` with impossible rows dropped, and a `dropped` attribute naming
#'   them so the caller can report rather than silently lose athletes.
drop_impossible_sigma <- function(ab, factor = 10) {
  ab <- data.table::as.data.table(ab)
  if (!all(c("sigma", "event_id") %in% names(ab))) return(ab[])
  ab[, .med := stats::median(sigma, na.rm = TRUE), by = event_id]
  bad <- ab[is.finite(sigma) & is.finite(.med) & .med > 0 & sigma > factor * .med]
  if (nrow(bad)) {
    keep <- ab[!(is.finite(sigma) & is.finite(.med) & .med > 0 & sigma > factor * .med)]
    keep[, .med := NULL]
    data.table::setattr(keep, "dropped",
      bad[, .(athlete_id, event_id, sigma, event_median = .med,
              times = round(sigma / .med, 1))])
    return(keep[])
  }
  ab[, .med := NULL]
  ab[]
}


#' Stop an athlete being credited for having no history at all
#'
#' `drop_impossible_sigma()` catches a merged identity, whose spread is absurd.
#' This catches the other half of the same symptom: an athlete whose evidence has
#' decayed to nothing. `ability_se` is `sigma / sqrt(w_total + kappa)`, so at
#' `w_total = 0` it sits at its maximum, the simulator draws that uncertainty,
#' and the athlete wins often enough to reach the card while typically finishing
#' fortieth. James SANDERSON made the men's 100m freestyle top five on a
#' predicted 54.8s exactly this way -- normal sigma, seven results, zero weight.
#'
#' This is the effect ce4881e measured out of sample: athletes with `w_total < 1`
#' were credited 0.0509 gold and won 0.0412.
#'
#' They are real entrants and stay in the field -- they can win, and removing
#' them would misstate everyone else's chances. What is removed is the PREMIUM
#' for knowing nothing: their posterior uncertainty is set to the event's typical
#' value instead of the maximum. Ability itself is untouched; with no evidence it
#' is already the prior mean, which is the honest estimate.
#'
#' @param ab An `estimate_ability()` table.
#' @param min_w Total weight below which an athlete carries no usable evidence.
temper_unevidenced <- function(ab, min_w = 0.05) {
  ab <- data.table::as.data.table(ab)
  if (!all(c("w_total", "ability_se", "event_id") %in% names(ab))) return(ab[])
  ab[, .se_med := stats::median(ability_se[w_total >= min_w], na.rm = TRUE), by = event_id]
  n <- ab[is.finite(w_total) & w_total < min_w & is.finite(.se_med) &
          is.finite(ability_se) & ability_se > .se_med, .N]
  ab[is.finite(w_total) & w_total < min_w & is.finite(.se_med) &
     is.finite(ability_se) & ability_se > .se_med, ability_se := .se_med]
  ab[, .se_med := NULL]
  data.table::setattr(ab, "tempered", n)
  ab[]
}
