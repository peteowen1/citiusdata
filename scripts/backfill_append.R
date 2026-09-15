# Append every staged backfill meet to championship_results.rds, in ONE pass.
#
# WHY BATCHED. append_meet_to_championship_results.R loads and saves the whole
# 4.5M-row table per meet, about 50 seconds each. For the 3,152 meets the
# overnight backfill staged that is roughly 44 hours of pure I/O to add 203,545
# rows. This loads once, maps everything, and saves once.
#
# THE COVERAGE GATE IS APPLIED TO THE BATCH, NOT PER MEET, and that distinction
# is the whole reason this is a separate script. Validated across 24 staged
# meets on 2026-09-15: `wind` is 100% empty on 12 of them, `venue_stadium` on
# 4, `birthdate` on 1. None is a mapping bug -- a five-row regional throws meet
# legitimately has no wind readings and no stadium in its venue string. The
# per-meet gate would reject all of them. What WOULD be a bug is wind being
# empty across all 3,152, so that is what gets asserted.
#
# Usage:
#   Rscript citiusdata/scripts/backfill_append.R          # dry run, reports only
#   CITIUS_APPEND_GO=1 Rscript ...                        # actually write
#   CITIUS_APPEND_MAX=200 ...                             # cap, for a trial

VERSE <- here::here()
suppressMessages({library(data.table); library(cli)})
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
D <- file.path(VERSE, "citiusdata", "data")
STAGE <- file.path(D, "backfill")
GO <- nzchar(Sys.getenv("CITIUS_APPEND_GO", ""))
MAXN <- as.integer(Sys.getenv("CITIUS_APPEND_MAX", "100000"))
say <- function(...) { cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")),
                           sprintf(...), "\n", sep = ""); flush.console() }

# Reuse the mapper from the single-meet script rather than copying it, so the
# two cannot drift. Sourcing the whole file would run it, so the function is
# extracted by line range -- a fixed region of our own repo, not external input.
src <- readLines(file.path(VERSE, "citiusdata", "scripts",
                           "append_meet_to_championship_results.R"))
i0 <- grep("^\\.map_harvest_to_championship <- function", src)
i1 <- grep("^# --- what championship_results\\.rds already holds", src)
if (!length(i0) || !length(i1)) cli_abort("Could not locate the mapper; has the append script been restructured?")
eval(parse(text = paste(src[i0:(i1 - 1)], collapse = "\n")))

CH_F <- file.path(D, "championship_results.rds")
say("loading championship_results.rds ...")
ch <- readRDS(CH_F)
setDT(ch)
say("loaded %s rows x %d cols", format(nrow(ch), big.mark = ","), ncol(ch))
have <- unique(as.character(ch$competition_id))
need <- names(ch)

files <- Sys.glob(file.path(STAGE, "comp_*.rds"))
files <- files[!grepl("_(startlist|summary)\\.rds$", files)]
cids  <- sub("^comp_", "", sub("\\.rds$", "", basename(files)))
keep  <- !cids %in% have
say("%s staged, %s already in championship_results, %s to append",
    format(length(files), big.mark = ","), format(sum(!keep), big.mark = ","),
    format(sum(keep), big.mark = ","))
files <- files[keep]; cids <- cids[keep]
if (length(files) > MAXN) { files <- head(files, MAXN); cids <- head(cids, MAXN) }
if (!length(files)) { say("nothing to do"); quit(status = 0) }

# --- map everything ----------------------------------------------------------
say("mapping %s meets ...", format(length(files), big.mark = ","))
bad <- character(0)
mapped <- vector("list", length(files))
for (k in seq_along(files)) {
  h <- tryCatch(as.data.table(readRDS(files[k])), error = function(e) NULL)
  if (is.null(h) || !nrow(h)) { bad <- c(bad, cids[k]); next }
  m <- tryCatch(.map_harvest_to_championship(h, cids[k], D), error = function(e) NULL)
  if (is.null(m)) { bad <- c(bad, cids[k]); next }
  miss <- setdiff(need, names(m))
  if (length(miss)) { bad <- c(bad, cids[k]); next }
  mapped[[k]] <- m[, ..need]
  if (k %% 250 == 0) say("  mapped %s/%s", format(k, big.mark = ","), format(length(files), big.mark = ","))
}
mapped <- mapped[!vapply(mapped, is.null, logical(1))]
if (length(bad)) say("%s meet(s) could not be mapped and are SKIPPED: %s",
                     length(bad), paste(head(bad, 8), collapse = ", "))
if (!length(mapped)) cli_abort("Nothing mapped successfully.")

new <- rbindlist(mapped, use.names = TRUE, fill = TRUE)
rm(mapped); invisible(gc(verbose = FALSE))
say("mapped %s new rows across %s meets",
    format(nrow(new), big.mark = ","), format(uniqueN(new$competition_id), big.mark = ","))

# --- BATCH-LEVEL coverage gate ----------------------------------------------
cov_new <- vapply(new, function(x) mean(!is.na(x)), numeric(1))
cov_old <- vapply(ch, function(x) mean(!is.na(x)), numeric(1))
cmp <- data.table(column = need, new = round(cov_new[need], 3), existing = round(cov_old[need], 3))
say("column coverage, new batch vs what is already stored:")
print(cmp)

# TYPES, NOT JUST COVERAGE. The gate below counts NAs, and an NA count cannot
# see a storage-type change -- on 2026-09-15 one meet handed back comp_start as
# Date/integer (fread returns IDate) while every other meet gave Date/double,
# rbindlist coerced the column to CHARACTER, and because class stayed "Date"
# is.na() returned FALSE on all 4,748,486 rows. The coverage gate therefore read
# comp_start as 100% populated -- an IMPROVEMENT -- while the values printed as
# NA and arithmetic on them threw "non-numeric argument to binary operator",
# which aborted the backtest. Compare class AND typeof against what is already
# stored, before the rbind that would silently reconcile them.
tcmp <- data.table(
  column = need,
  stored = vapply(need, function(k) sprintf("%s/%s", paste(class(ch[[k]]), collapse="+"),
                                            typeof(ch[[k]])), character(1)),
  incoming = vapply(need, function(k) if (k %in% names(new))
    sprintf("%s/%s", paste(class(new[[k]]), collapse="+"), typeof(new[[k]])) else "<absent>",
    character(1)))
tbad <- tcmp[stored != incoming & incoming != "<absent>"]
if (nrow(tbad)) {
  print(tbad)
  cli_abort(c(
    "{nrow(tbad)} column{?s} arrive with a different class/storage than the stored table.",
    x = "{.field {tbad$column}}",
    i = "rbind would silently reconcile these -- a Date/integer meeting a Date/double
         becomes a character column that still reports class Date, so is.na() and every
         coverage check keep passing. Fix the mapper, do not widen this gate."))
}

exempt <- c("comp_name", "comp_tier", "discipline_code", "value_raw", "birthdate_year_only")
# A column empty across the WHOLE batch, that the existing corpus populates
# well, is a mapping bug rather than a property of these meets.
zero <- cmp[new == 0 & existing > 0.5 & !(column %in% exempt)]$column
if (length(zero)) cli_abort(c(
  "Column{?s} {.field {zero}} are 100% empty across the entire batch but well populated in the existing table.",
  i = "That is a mapping bug, not a property of these meets -- fix it rather than writing empty columns."))

# Key sanity: no competition should already be present, and no exact duplicate.
if (any(as.character(new$competition_id) %in% have))
  cli_abort("Some competitions are already in championship_results -- would double-count.")
say("duplicate rows within the new batch: %s",
    format(nrow(new) - uniqueN(new), big.mark = ","))

if (!GO) {
  say("DRY RUN -- nothing written. Set CITIUS_APPEND_GO=1 to commit %s rows.",
      format(nrow(new), big.mark = ","))
  quit(status = 0)
}

# --- write, with a backup first ---------------------------------------------
bk <- file.path(D, sprintf("championship_results.pre_backfill_%s.rds", format(Sys.Date(), "%Y%m%d")))
if (!file.exists(bk)) { say("backing up to %s ...", basename(bk)); file.copy(CH_F, bk) }

out <- rbind(ch, new, use.names = TRUE)
say("appending: %s + %s = %s rows", format(nrow(ch), big.mark = ","),
    format(nrow(new), big.mark = ","), format(nrow(out), big.mark = ","))
tmp <- paste0(CH_F, ".tmp")
saveRDS(out, tmp)
file.rename(tmp, CH_F)          # write-then-rename: never a half-written table

# --- verify by re-reading ----------------------------------------------------
chk <- readRDS(CH_F)
say("VERIFIED on re-read: %s rows, %d cols, %s competitions",
    format(nrow(chk), big.mark = ","), ncol(chk),
    format(uniqueN(chk$competition_id), big.mark = ","))
if (nrow(chk) != nrow(out)) cli_abort("Re-read row count does not match what was written.")
say("done. athletics_corpus.rds is NOT regenerated -- run build_athletics_corpus.R next.")
