# Attach authoritative ISO country codes to a Diamond-League-shaped card.
#
# WHY. The site renders nationality in a compact inline badge sized for a
# three-letter code. A third-party entry list gives free text, inconsistently:
# the Brussels source mixes bare codes ("USA") with full names ("Saint Lucia",
# "Trinidad and Tobago" -- 19 characters). Rendering that as-is breaks the
# badge layout. The corpus cannot supply the missing codes: it has
# venue_country (where a meet was HELD), which is a different fact.
#
# ADDITIVE, NOT DESTRUCTIVE. `nation` is left exactly as the source wrote it
# and `nation_code` is added alongside, so the page chooses which to render and
# nothing that already reads `nation` changes meaning. Overwriting would also
# destroy the only record of what the source actually said, which is the thing
# a provenance dispute would need.
#
# Codes come from fetch_athlete_country_codes.R's cache (World Athletics' own
# competitor records, keyed by athlete_id). A card whose athletes are not all
# in that cache is reported and left alone rather than half-patched -- see
# below.
#
# RUN THIS AFTER predict_diamond_league_final.R, EVERY TIME. This patches the
# card in place, and predict_* rewrites the card from scratch -- so re-running
# the prediction silently drops nation_code again. Found the obvious way, by
# doing exactly that (2026-08-31).
#
# You no longer have to remember, though. An earlier version of this comment
# claimed the sanity script could not catch this because nation_code is "a
# display concern, not a modelling one" -- review rejected that, correctly:
# sanity_diamond_league_card.R is run by export_athletics_blog.R as a hard
# publish gate, so it is precisely the place that CAN refuse. It now checks
# nation_code coverage and fails the publish if this script was skipped or run
# before predict_*. The order below is enforced, not merely documented:
#
#   fetch_athlete_country_codes.R  (once per new field; cached thereafter)
#   predict_diamond_league_final.R
#   add_nation_codes.R             <- this script
#   sanity_diamond_league_card.R
#
# Usage:  Rscript scripts/add_nation_codes.R <meet_id>

VERSE <- here::here()
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
suppressMessages(library(data.table))
D <- file.path(VERSE, "citiusdata", "data")

args <- commandArgs(trailingOnly = TRUE)
MEET <- if (length(args)) args[[1]] else Sys.getenv("CITIUS_DL_MEET", "brussels2026")

CACHE <- file.path(D, "athlete_country_codes.rds")
if (!file.exists(CACHE)) {
  cli::cli_abort(c("No country-code cache at {.file {basename(CACHE)}}.",
                   i = "Run {.code Rscript scripts/fetch_athlete_country_codes.R {MEET}} first."))
}
cc <- setDT(readRDS(CACHE))[, .(athlete_id = as.character(athlete_id), nation_code = country_code)]

f_rds <- file.path(D, paste0(MEET, "_pretournament.rds"))
if (!file.exists(f_rds)) cli::cli_abort("No card at {.file {basename(f_rds)}}.")
p <- setDT(readRDS(f_rds))
p[, athlete_id := as.character(athlete_id)]

if ("nation_code" %in% names(p)) p[, nation_code := NULL]   # idempotent re-run
before <- nrow(p)
p <- merge(p, cc, by = "athlete_id", all.x = TRUE)
stopifnot("merge changed the row count" = nrow(p) == before)

n_missing <- p[is.na(nation_code) | !nzchar(nation_code), .N]
cov <- 100 * (1 - n_missing / nrow(p))
cli::cli_alert_info("nation_code coverage: {round(cov, 1)}% ({nrow(p) - n_missing} of {nrow(p)} rows).")
if (n_missing) {
  cli::cli_alert_warning("{n_missing} row{?s} have no code -- these keep `nation` only:")
  print(unique(p[is.na(nation_code) | !nzchar(nation_code), .(athlete, nation)]))
}
# A code longer than 3 characters would defeat the entire point of this script
# (the badge is sized for three), so assert rather than assume the source is
# well-behaved.
long <- p[!is.na(nation_code) & nchar(nation_code) > 3L]
if (nrow(long)) {
  print(unique(long[, .(athlete, nation, nation_code)]))
  cli::cli_abort("{nrow(long)} row{?s} have a nation_code longer than 3 characters.")
}

saveRDS(p, f_rds)
arrow::write_parquet(p, file.path(D, paste0(MEET, "_pretournament.parquet")))
cli::cli_alert_success("Patched {basename(f_rds)} and its .parquet: {nrow(p)} rows now carry nation_code.")
print(head(unique(p[, .(nation, nation_code)])[order(nation)], 8))
