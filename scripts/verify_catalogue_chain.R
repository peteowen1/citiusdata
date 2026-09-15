# Check the rebuilt catalogue against the backup the chain took before it ran.
#
# WHY NOT FILE SIZE. run_catalogue_chain.ps1 used to compare the parquet's
# BYTES to the backup's. On 2026-09-15 that reported "catalogue SHRANK" when
# the truth was the opposite in the dimension that mattered: rows had grown
# 32,088 -> 33,118 while six COLUMNS had silently vanished, because three
# chain steps were missing. One number cannot see two dimensions. This checks
# rows, columns and date sanity separately and names what moved.
#
# Exit 1 on a real regression so the runner can refuse to call the chain done.
#
# Usage: Rscript citiusdata/scripts/verify_catalogue_chain.R <backup.parquet>

suppressMessages({library(arrow); library(data.table)})
D   <- here::here("citiusdata", "data")
CAT <- file.path(D, "competition_catalogue.parquet")
bkf <- commandArgs(trailingOnly = TRUE)[1]

cur <- setDT(read_parquet(CAT))
fail <- character(0)
warn <- character(0)

cat(sprintf("catalogue: %s rows x %d cols\n", format(nrow(cur), big.mark = ","), ncol(cur)))

if (!is.na(bkf) && file.exists(bkf)) {
  bk <- setDT(read_parquet(bkf))
  cat(sprintf("backup   : %s rows x %d cols  (%s)\n",
              format(nrow(bk), big.mark = ","), ncol(bk), basename(bkf)))

  lost_cols <- setdiff(names(bk), names(cur))
  if (length(lost_cols))
    fail <- c(fail, sprintf("%d column(s) lost vs the backup: %s",
                            length(lost_cols), paste(lost_cols, collapse = ", ")))
  gained <- setdiff(names(cur), names(bk))
  if (length(gained)) cat("new columns:", paste(gained, collapse = ", "), "\n")

  # Rows may legitimately grow (new meets). A DROP is the 2026-08-19 failure.
  if (nrow(cur) < nrow(bk))
    fail <- c(fail, sprintf("row count fell %s -> %s",
                            format(nrow(bk), big.mark = ","), format(nrow(cur), big.mark = ",")))

  lost_ids <- setdiff(as.character(bk$competition_id), as.character(cur$competition_id))
  if (length(lost_ids))
    fail <- c(fail, sprintf("%s competition(s) present in the backup are missing now",
                            format(length(lost_ids), big.mark = ",")))
} else {
  warn <- c(warn, "no backup given or found -- comparison skipped")
}

# Date sanity. as.Date(Inf) overflows to a structurally valid Date, so a range
# check is the only thing that catches it; NA would have been the honest value.
bad <- cur[!is.na(first_date) & (first_date < as.Date("1900-01-01") |
                                 first_date > Sys.Date() + 400)]
if (nrow(bad)) fail <- c(fail, sprintf(
  "%d meet(s) have an out-of-range first_date (e.g. %s) -- an all-NA min()/max() overflow",
  nrow(bad), as.character(bad$first_date[1])))

# meet_tier is what form_ratings.R inner-joins on, so an unpopulated one makes
# races invisible rather than down-weighted. Coverage, not presence.
tier_cov <- mean(!is.na(cur$meet_tier) & nzchar(cur$meet_tier))
cat(sprintf("meet_tier populated: %.3f\n", tier_cov))
if (tier_cov < 0.999) fail <- c(fail, sprintf("meet_tier only %.1f%% populated", 100 * tier_cov))
print(cur[, .N, by = meet_tier][order(-N)])

name_cov <- mean(!is.na(cur$comp_name) & nzchar(cur$comp_name))
cat(sprintf("comp_name populated: %.3f\n", name_cov))
if (name_cov < 0.95) warn <- c(warn, sprintf("comp_name only %.1f%% populated", 100 * name_cov))

cat(sprintf("date range: %s .. %s\n",
            as.character(min(cur$first_date, na.rm = TRUE)),
            as.character(max(cur$last_date,  na.rm = TRUE))))

for (w in warn) cat("WARN: ", w, "\n", sep = "")
if (length(fail)) {
  for (f in fail) cat("FAIL: ", f, "\n", sep = "")
  quit(status = 1)
}
cat("VERIFY OK\n")
