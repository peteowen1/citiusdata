# Poll the World Athletics feed for Glasgow 2026 Commonwealth Games results.
#
# Why this route rather than the official Games results system: Glasgow's own
# Competition Results System (crs-cg2026-api.glasgow2026.com, Microplus) carries
# richer live data — entries, start lists, live results — but sits behind both a
# Cloudflare challenge and a bearer token, so it cannot run unattended in CI.
#
# World Athletics ingests Commonwealth Games results after the fact: Birmingham
# 2022 (competition 7147633) is fully present. Glasgow will be too. This poller
# simply waits for that to happen, then harvests through the normal path.

suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

GLASGOW_2026 <- 7187518L
OUT <- here::here("citiusdata", "data")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

comp <- find_competition("XXIII Commonwealth Games")
comp <- comp[competition_id == GLASGOW_2026]

if (!nrow(comp)) {
  cli::cli_alert_danger("Competition {GLASGOW_2026} not found.")
} else if (!comp$has_results) {
  cli::cli_alert_info(
    "Glasgow 2026 has no results yet (runs {format(comp$start)} to {format(comp$end)})."
  )
} else {
  results <- harvest_competitions(GLASGOW_2026)
  arrow::write_parquet(results, file.path(OUT, "glasgow2026_athletics.parquet"))
  cli::cli_alert_success(
    "Harvested {nrow(results)} result{?s} across {uniqueN(results$event_id)} event{?s}."
  )
  print(results[, .(n = .N, no_mark = sum(is.na(perf))), by = event_id][order(-n)])
}
