# Verdict for the thin-record arm (version C) against its control, split the
# two ways compare_marks_arms.R cannot:
#   - IN SCOPE (800/1500/5000 m) vs everything else. Everything else must be
#     EXACTLY unchanged; any moved row there is a bug, not an effect.
#   - YEAR. The time-since-race offsets (data/gap_offsets_middle_distance.csv)
#     were fitted on 2024 cuts, so 2024 meets are in-sample for that term. The
#     verdict is 2025+; 2024 and earlier are printed for reference.
# Metrics (citiusverse CLAUDE.md: marks + concordance judge an ability change):
#   marks error = mean |median_mark - actual| / actual, %, LOWER is better;
#   concordance = share of within-race pairs ordered the same way as the
#   result, %, HIGHER is better. CIs are a paired bootstrap over races.
#
#   CITIUS_TC_A=bt_cache_thin_ctrl CITIUS_TC_B=bt_cache_thin_on \
#     powershell.exe -Command 'Rscript citiusdata/scripts/diagnostics/compare_thin_record_arms.R'
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
suppressMessages(library(arrow))
OUT <- here::here("citiusdata", "data")
A <- Sys.getenv("CITIUS_TC_A", "bt_cache_thin_ctrl")
B <- Sys.getenv("CITIUS_TC_B", "bt_cache_thin_on")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")

load_arm <- function(dir) {   # same reader as compare_marks_arms.R
  d <- file.path(OUT, dir)
  fs <- setdiff(list.files(d, pattern = "\\.rds$"), "_arm.rds")
  if (!length(fs)) stop("no cached meets in ", d)
  out <- rbindlist(lapply(fs, function(f) {
    o <- readRDS(file.path(d, f))
    if (!length(o)) return(NULL)
    rbindlist(lapply(o, function(r) {
      if (is.null(r$pred) || !"median_mark" %in% names(r$pred)) return(NULL)
      as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id), median_mark)]
    }), fill = TRUE)
  }), fill = TRUE)
  unique(out, by = c("race_id", "athlete_id"))
}
pa <- load_arm(A); pb <- load_arm(B)
cat(sprintf("%s: %s rows | %s: %s rows\n", A, format(nrow(pa), big.mark = ","), B, format(nrow(pb), big.mark = ",")))

st <- as.data.table(open_dataset(file.path(OUT, "athletics_corpus_store")) |>
  dplyr::select(race_key, athlete_id, mark, event_id, date) |> dplyr::collect())
st[, athlete_id := as.character(athlete_id)]
act <- unique(st[!is.na(mark) & !is.na(race_key) & mark > 0,
                 .(race_id = race_key, athlete_id, actual = mark, event_id, date)], by = c("race_id", "athlete_id"))
rm(st); invisible(gc())

m <- merge(pa[, .(race_id, athlete_id, mark_a = median_mark)],
           pb[, .(race_id, athlete_id, mark_b = median_mark)], by = c("race_id", "athlete_id"))
m <- merge(m, act, by = c("race_id", "athlete_id"))
m <- m[is.finite(mark_a) & is.finite(mark_b) & is.finite(actual)]
stopifnot(nrow(m) > 0)
m[, `:=`(ape_a = abs(mark_a - actual) / actual, ape_b = abs(mark_b - actual) / actual,
         scope = fifelse(event_id %in% EV, "800/1500/5000", "other events"),
         yr = fifelse(year(date) >= 2025, "2025+ (verdict)", fifelse(year(date) == 2024, "2024 (offsets in-sample)", "before 2024")))]
cat(sprintf("paired rows %s, races %s, dates %s\n", format(nrow(m), big.mark = ","),
            format(uniqueN(m$race_id), big.mark = ","), paste(format(range(m$date)), collapse = " to ")))

# Invariance: outside the six events nothing may move at all.
oth <- m[scope == "other events"]
n_moved <- oth[mark_a != mark_b, .N]
cat(sprintf("\nINVARIANCE: other-event rows moved %d of %s (must be 0)\n", n_moved, format(nrow(oth), big.mark = ",")))
if (n_moved > 0) cat("!!! the arm changed predictions outside its scope -- do not trust the verdict below\n")

conc_by_race <- function(x) x[, if (.N >= 2) {
  ii <- combn(.N, 2); da <- sign(mark_a[ii[1, ]] - mark_a[ii[2, ]]); db <- sign(mark_b[ii[1, ]] - mark_b[ii[2, ]])
  do <- sign(actual[ii[1, ]] - actual[ii[2, ]])
  .(ok_a = sum(da == do & do != 0), ok_b = sum(db == do & do != 0), n = sum(do != 0))
}, by = race_id]
boot <- function(r, f) { set.seed(42); q <- vapply(1:2000, function(k) f(r[sample(.N, .N, TRUE)]), 0); quantile(q, c(.025, .975)) }

block <- function(x, label) {
  if (nrow(x) < 30) { cat(sprintf("%-40s n %d, too few\n", label, nrow(x))); return(invisible()) }
  r <- x[, .(sa = sum(ape_a), sb = sum(ape_b), n = .N), by = race_id]
  ci <- 100 * boot(r, function(s) sum(s$sb - s$sa) / sum(s$n))
  cr <- conc_by_race(x)
  cc <- if (nrow(cr) >= 10) 100 * boot(cr, function(s) (sum(s$ok_b) - sum(s$ok_a)) / sum(s$n)) else c(NA, NA)
  cat(sprintf("%-40s rows %6d races %4d moved %5.1f%% | marks err A %.3f B %.3f diff %+.3f [%+.3f,%+.3f] | concord A %.2f B %.2f diff %+.2f [%+.2f,%+.2f] (pairs %d)\n",
              label, nrow(x), uniqueN(x$race_id), 100 * mean(x$mark_a != x$mark_b),
              100 * mean(x$ape_a), 100 * mean(x$ape_b), 100 * (mean(x$ape_b) - mean(x$ape_a)), ci[1], ci[2],
              100 * sum(cr$ok_a) / sum(cr$n), 100 * sum(cr$ok_b) / sum(cr$n),
              100 * (sum(cr$ok_b) - sum(cr$ok_a)) / sum(cr$n), cc[1], cc[2], sum(cr$n)))
}
cat("\nmarks err: % error on the mark, LOWER is better; diff = B - A, NEGATIVE favours the arm.\n")
cat("concord: % of within-race pairs in the right order, HIGHER is better; diff = B - A, POSITIVE favours the arm.\n")
cat("CIs: 95%, paired bootstrap over races.\n\n")
md <- m[scope == "800/1500/5000"]
for (y in c("2025+ (verdict)", "2024 (offsets in-sample)", "before 2024")) block(md[yr == y], paste("800/1500/5000,", y))
block(md, "800/1500/5000, all years")
cat("\nBy event, 2025+:\n")
for (e in EV) block(md[yr == "2025+ (verdict)" & event_id == e], sub("AT-", "", e))
