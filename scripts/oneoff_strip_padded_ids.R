# One-off (2026-11-10): strip zero-padded WA athlete ids ("014164603" -> "14164603")
# from the STORED files, so they match the corpus build and the harvest parsers,
# which now strip at read time (2331b8c). Without this, backtest_athletics.R joins
# championship_results.rds fields (padded) against an ability table keyed on the
# stripped ids, and ~6,427 athletes silently drop out of the scored fields.
#
# RUN ONLY when no backtest is running (backtests read championship_results.rds),
# then rebuild the corpus. Backs each file up first; atomic write; asserts.
#   powershell.exe -Command 'Rscript "C:\dev\citiusverse\citiusdata\scripts\oneoff_strip_padded_ids.R"'
suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
pat <- "^0+(?=[0-9])"

fix_file <- function(fn) {
  f <- file.path(D, fn)
  x <- setDT(readRDS(f))
  stopifnot("athlete_id" %in% names(x), nrow(x) > 0)
  n0 <- nrow(x)
  if (!is.character(x$athlete_id)) {
    cat(sprintf("%s: athlete_id is %s, not character -- nothing to strip\n", fn, class(x$athlete_id)[1]))
    return(invisible(NULL))
  }
  pad <- !is.na(x$athlete_id) & grepl("^0+[0-9]", x$athlete_id)
  cat(sprintf("%s: %s rows, %s padded rows (%s ids)\n", fn, format(n0, big.mark = ","),
              format(sum(pad), big.mark = ","), format(uniqueN(x$athlete_id[pad]), big.mark = ",")))
  if (!any(pad)) return(invisible(NULL))
  bak <- file.path(D, sub("\\.rds$", sprintf("_prestrip_%s.rds", stamp), fn))
  stopifnot(file.copy(f, bak, overwrite = FALSE))
  cat("  backup:", bak, "\n")
  x[pad, athlete_id := sub(pat, "", athlete_id, perl = TRUE)]
  stopifnot(nrow(x) == n0, !any(grepl("^0+[0-9]", x$athlete_id[!is.na(x$athlete_id)])))
  tmp <- paste0(f, ".tmp")
  saveRDS(x, tmp)
  stopifnot(nrow(readRDS(tmp)) == n0)
  stopifnot(file.rename(tmp, f))
  cat("  written, rows unchanged\n")
}

fix_file("championship_results.rds")

# Name lookup: a stripped id may only have had its name under the padded form.
# Strip, then keep one row per id (the plain id's row first if both exist).
lf <- file.path(D, "athlete_name_lookup.rds")
if (file.exists(lf)) {
  nm <- setDT(readRDS(lf))
  if (is.character(nm$athlete_id) && any(grepl("^0+[0-9]", nm$athlete_id))) {
    n0 <- nrow(nm)
    bak <- sub("\\.rds$", sprintf("_prestrip_%s.rds", stamp), lf)
    stopifnot(file.copy(lf, bak, overwrite = FALSE))
    nm[, .was_padded := grepl("^0+[0-9]", athlete_id)]
    nm[, athlete_id := sub(pat, "", athlete_id, perl = TRUE)]
    setorder(nm, athlete_id, .was_padded)
    nm <- unique(nm, by = "athlete_id")[, .was_padded := NULL]
    cat(sprintf("athlete_name_lookup.rds: %s -> %s rows (backup %s)\n", n0, nrow(nm), bak))
    saveRDS(nm, lf)
  } else cat("athlete_name_lookup.rds: no padded ids\n")
}
cat("Next: rebuild the corpus (build_athletics_corpus.R), then diagnostics/zero_padded_ids.R should report 0 twins.\n")
