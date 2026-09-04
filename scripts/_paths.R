# Path-indirection layer for citiusdata/data.
#
# WHY THIS EXISTS. A 2026-08-30 survey found no indirection anywhere in this
# tree: every script splices a literal filename onto a shared root constant
# (`D`/`OUT`), so moving one file means editing every script that names it --
# up to 51 scripts for competition_catalogue.parquet, 43 for
# championship_results.rds. That is the actual cost of a future reorg, not a
# hypothetical one, and it is what makes `citiusdata/data/`'s current flat,
# 1,400+-file layout expensive to ever clean up once grown.
#
# `citius_data_path(name)` is the fix: one lookup table, one place to update
# when a file's physical location changes. Existing scripts are NOT being
# mass-migrated to this (see the DuckDB store migration's own phased-adoption
# precedent, `citius/R/duckdb_store.R`) -- adopt it when a script is already
# being touched for other reasons. New scripts should use it from the start;
# see scripts/README.md's "Data directory hygiene" section.
#
# Source this after establishing `here::here()` works (i.e. from inside the
# citiusverse tree) -- it resolves its own root independently of whatever
# D/OUT convention the sourcing script uses, so it works regardless of
# which of the two existing root-constant idioms (VERSE-relative or
# here::here()) a caller happens to use.

.CITIUS_DATA_ROOT <- here::here("citiusdata", "data")

# name -> path relative to .CITIUS_DATA_ROOT. Directories (the Arrow stores)
# and files both resolve the same way; callers that need to know which is
# which already know from the name.
CITIUS_DATA_PATHS <- list(
  championship_results     = "championship_results.rds",
  athletics_corpus         = "athletics_corpus.rds",
  athletics_history        = "athletics_history.rds",
  competition_catalogue    = "competition_catalogue.parquet",
  competition_catalogue_csv = "competition_catalogue.csv",
  citius_duckdb             = "citius.duckdb",
  athletics_store           = "athletics_store",
  athletics_corpus_store     = "athletics_corpus_store",
  athletics_careers_store    = "athletics_careers_store",
  athlete_name_lookup        = "athlete_name_lookup.rds",
  athletics_calendar         = "athletics_calendar.csv"
)

#' Resolve a logical data-file name to its current physical path
#'
#' @param name One of `names(CITIUS_DATA_PATHS)`.
#' @return Absolute path (character), not checked for existence -- callers
#'   that need the file to already exist should check that themselves, the
#'   way they would with any other path.
#' @examples
#' \dontrun{
#' citius_data_path("championship_results")
#' }
citius_data_path <- function(name) {
  if (!name %in% names(CITIUS_DATA_PATHS)) {
    cli::cli_abort(c(
      "{.val {name}} is not a known citius data path.",
      i = "Known: {.val {names(CITIUS_DATA_PATHS)}}",
      i = "Add it to CITIUS_DATA_PATHS in _paths.R rather than hardcoding a literal path."
    ))
  }
  file.path(.CITIUS_DATA_ROOT, CITIUS_DATA_PATHS[[name]])
}
