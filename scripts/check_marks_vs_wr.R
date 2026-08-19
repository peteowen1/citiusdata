# ANCHOR CHECK: does the model claim marks nobody has ever run?
#
# Found via the men's 1000m: the published ranking showed 2:11.12 for Wanyonyi
# against a world record of 2:11.83 - and a full audit of world_records.csv
# confirmed the RECORD is right and current. So the model is predicting a mark
# faster than the fastest performance in history.
#
# This is publicly visible. export_blog_data.R merges this same file and shows a
# "% of world record" column on the live blog, so an over-record prediction
# prints as more than 100%.
#
# The existing guard cannot catch it: validate_world_records.R only flags a
# record contradicted by results already in the corpus, and says so in its own
# header. A predicted mark too fast to be contradicted by held data is invisible
# to it - exactly this case.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

# "1:40.91" / "2:00:35" / "9.58" -> numeric seconds; a bare field mark passes
# through unchanged. Anything unparseable becomes NA rather than a wrong number.
to_num <- function(x) {
  x <- trimws(gsub("[^0-9:.]", "", x))
  vapply(x, function(s) {
    if (!nzchar(s)) return(NA_real_)
    p <- suppressWarnings(as.numeric(strsplit(s, ":", fixed = TRUE)[[1]]))
    if (anyNA(p)) return(NA_real_)
    switch(as.character(length(p)), "1" = p[1], "2" = p[1] * 60 + p[2],
           "3" = p[1] * 3600 + p[2] * 60 + p[3], NA_real_)
  }, numeric(1), USE.NAMES = FALSE)
}

wr <- setDT(utils::read.csv(file.path(D, "world_records.csv"), stringsAsFactors = FALSE))
wr[, wr_num := to_num(mark)]
stopifnot("some world records did not parse" = !any(is.na(wr$wr_num)))
cat(sprintf("world records parsed: %d\n", nrow(wr)))

d <- setDT(read_parquet(file.path(D, "form_display_final.parquet")))
d <- d[is.finite(pred_mark)]
x <- merge(d, wr[, .(event_id, mark, holder, wr_num)], by = "event_id")
stopifnot("nothing merged - event id conventions differ" = nrow(x) > 0)

# seconds: lower is better. metres/points: higher is better.
x[, beats_wr := fifelse(unit == "seconds", pred_mark < wr_num, pred_mark > wr_num)]
x[, pct_wr := fifelse(unit == "seconds", 100 * wr_num / pred_mark,
                                          100 * pred_mark / wr_num)]
cat(sprintf("athlete-events compared: %s across %d events (%s)\n",
            format(nrow(x), big.mark = ","), uniqueN(x$event_id),
            paste(unique(x$unit), collapse = "/")))

over <- x[beats_wr == TRUE]
cat(sprintf("\nPREDICTED BETTER THAN THE WORLD RECORD: %s rows (%.3f%%), %d events\n",
            format(nrow(over), big.mark = ","), 100 * nrow(over) / nrow(x),
            uniqueN(over$event_id)))

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]
if (nrow(over)) {
  o <- merge(over, reg, by = "event_id", all.x = TRUE)
  cat("\n=== events where the model beats the record ===\n")
  print(o[, .(athletes = .N, best_rank = min(rk), worst_pct = round(max(pct_wr), 2)),
          by = .(discipline, sex, family)][order(-worst_pct)])
  cat("\n=== the 15 most extreme rows ===\n")
  print(head(o[order(-pct_wr), .(discipline, sex, athlete_name, rk,
                                 pred = round(pred_mark, 2), wr = mark,
                                 n_eff = round(n_eff, 1),
                                 pct = round(pct_wr, 2))], 15))
}

cat("\n=== top of each event as a % of the world record ===\n")
cat("A healthy event tops out a little under 100.\n")
top <- x[, .(top_pct = round(max(pct_wr), 2), rows = .N), by = event_id]
top <- merge(top, reg, by = "event_id", all.x = TRUE)
setorder(top, -top_pct)
print(head(top[, .(discipline, sex, family, rows, top_pct)], 15))
cat("\n--- the other end ---\n")
print(tail(top[, .(discipline, sex, family, rows, top_pct)], 6))
