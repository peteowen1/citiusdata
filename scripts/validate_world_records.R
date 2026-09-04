# Validate world_records.csv against our OWN results.
#
# Run this whenever world_records.csv changes. Records are hand-maintained
# reference data — the category most likely to be quietly wrong — and a wrong
# denominator makes every athlete in that event read wrong on the site with
# nothing erroring.
#
# The check needs no external source: a ratified record cannot be beaten by a
# legal, wind-legal mark we already hold. If it is, the record is wrong or stale.
#
# It also finds CORROBORATION, which is the stronger signal. On 2026-07-28 eight
# events came back with a gap of exactly 0 because our World Athletics harvest
# contains the record-setting run itself (10,000m W 28:54.14, 1000m M 2:11.83,
# 110mH M 12.75, 3000m SC W 8:44.32 among them). Two independent sources agreeing
# to the hundredth beats an absence of conflict.
#
# Caveat on what it CANNOT catch: our harvest is recent championship results, so
# a record that is too FAST (or a very old one like Griffith-Joyner's 10.49) has
# nothing in range to contradict it. Verify those by targeted lookup.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
D <- here::here("citiusdata", "data")

wr <- fread(file.path(D, "world_records.csv"))
wr[, wr_mark := parse_mark(mark)]
stopifnot(!anyNA(wr$wr_mark))

ev <- citius_events()[, .(event_id, orientation)]
wr <- merge(wr, ev, by = "event_id")
cat("records parsed:", nrow(wr), "of", nrow(fread(file.path(D, "world_records.csv"))), "\n")
miss <- setdiff(fread(file.path(D,"world_records.csv"))$event_id, wr$event_id)
if (length(miss)) cat("!! event_id not in registry:", paste(miss, collapse=", "), "\n")

res <- tryCatch(
  with_citius_db_connection(function(conn) load_championship_results(conn), read_only = TRUE),
  error = function(e) {
    cli::cli_warn("citius.duckdb unavailable ({conditionMessage(e)}); falling back to championship_results.rds.")
    NULL
  }
)
if (is.null(res) || !nrow(res)) res <- setDT(readRDS(file.path(D, "championship_results.rds")))
res <- flag_implausible(res)[!is.na(perf) & !is.na(mark)]
# legal == wind-legal where wind is recorded; NA wind means not a wind-affected event
res <- res[is.na(legal) | legal == TRUE]

# `perf` is already sign-oriented so higher is ALWAYS better, whatever the event
# type — comparing on it avoids depending on an orientation column in `res`.
best <- res[, .(our_best_perf = max(perf, na.rm = TRUE), n = .N), by = event_id]
wr[, wr_perf := to_perf(wr_mark, orientation)]
chk <- merge(wr[, .(event_id, holder, mark, wr_mark, orientation, wr_perf)], best, by = "event_id")
chk[, beats := our_best_perf > wr_perf]
chk[, our_best_mark := round(perf_to_mark(our_best_perf, orientation), 3)]
# how much better our best is than the record, as a % of the mark
chk[, gap_pct := round(100 * (exp(our_best_perf - wr_perf) - 1), 3)]

cat("\n=== events where OUR data beats the claimed record (suspect) ===\n")
bad <- chk[beats == TRUE][order(-abs(gap_pct))]
if (!nrow(bad)) cat("none - every record survives the check\n") else
  print(bad[, .(event_id, holder, record = mark, our_best = our_best_mark, gap_pct, n)])

cat("\n=== closest survivors (sanity: should be plausible near-misses) ===\n")
print(chk[beats == FALSE][order(abs(gap_pct))][1:8,
      .(event_id, record = mark, our_best = our_best_mark, gap_pct)])
cat("\nevents with a record but no data:", nrow(wr) - nrow(chk), "\n")
