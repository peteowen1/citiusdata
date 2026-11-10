# Before/after for the 1500m-inside-a-mile flag (citiusverse NEXT-STEPS, 2026-11-08).
# Rates the 1500m events with the deployed configuration twice: once on the full
# corpus, once with the rows in data/flags/mile_split_1500.csv removed. Changes
# nothing published. Writes data/flags/mile_split_before_after.csv.
# Arrow via PowerShell: Rscript under Git Bash segfaults on arrow.
t0 <- Sys.time()
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages({ library(data.table); library(arrow) })
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV  <- c("AT-1500Metres-M", "AT-1500Metres-W")

corpus <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
cat("corpus rows", nrow(corpus), " date range", format(range(corpus$date, na.rm = TRUE)), "\n")
h <- flag_implausible(corpus[event_id %in% EV])
rm(corpus); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]
h[, athlete_id := as.character(athlete_id)]
cat("1500m rows", nrow(h), " athletes", uniqueN(h$athlete_id), "\n")

fl <- fread(file.path(OUT, "flags", "mile_split_1500.csv"),
            colClasses = list(character = "athlete_id"))
fl[, date := as.Date(date)]
key <- c("athlete_id", "event_id", "date", "mark")
hit <- h[fl[, ..key], on = key, nomatch = 0L, which = TRUE]
cat("flag rows", nrow(fl), " matched corpus rows", length(unique(hit)), "\n")
stopifnot(length(hit) > 0.9 * nrow(fl))   # the join must actually find them
h_drop <- h[-unique(hit)]

# ability is on the perf scale; check perf == -log(mark) so it converts to a time.
chk <- h[!is.na(mark) & mark > 0][1:5000]
cat("max |perf + log(mark)|", max(abs(chk$perf + log(chk$mark))), "
")
AS_OF <- max(h$date, na.rm = TRUE)
calibration <- deployed_calibration(OUT)
rate <- function(x) {
  r <- deployed_ability(x, as_of = AS_OF, calibration = calibration)
  r[, athlete_id := as.character(athlete_id)]
  r[!is.na(last_date) & as.numeric(AS_OF - last_date) <= 730]
}
before <- rate(h);      cat("rated before", nrow(before), format(Sys.time() - t0), "\n")
after  <- rate(h_drop); cat("rated after ", nrow(after),  format(Sys.time() - t0), "\n")

for (x in list(before, after)) x[, pred_mark := exp(-ability)]
cols <- c("athlete_id", "event_id", "ability", "pred_mark", "sigma", "n")
b <- before[, ..cols]; a <- after[, ..cols]
cmp <- merge(b, a, by = c("athlete_id", "event_id"), suffixes = c("_before", "_after"), all = TRUE)
# The corpus carries no names. The lookup's ids drop the corpus's leading zero
# ("14373573" vs "014166673"), so join on the id with leading zeros stripped.
nm <- setDT(readRDS(file.path(OUT, "athlete_name_lookup.rds")))
nm <- unique(nm[, .(id0 = sub("^0+", "", athlete_id), athlete_name)], by = "id0")
cmp[, id0 := sub("^0+", "", athlete_id)]
cmp <- nm[cmp, on = "id0"][, id0 := NULL]
cat("names found for", cmp[!is.na(athlete_name), .N], "of", nrow(cmp), "rows\n")
nflag <- fl[, .(n_flagged = .N), by = .(athlete_id, event_id)]
cmp <- nflag[cmp, on = c("athlete_id", "event_id")][is.na(n_flagged), n_flagged := 0L]
cmp[, rank_before := frank(-ability_before, na.last = "keep", ties.method = "min"), by = event_id]
cmp[, rank_after  := frank(-ability_after,  na.last = "keep", ties.method = "min"), by = event_id]
fwrite(cmp, file.path(OUT, "flags", "mile_split_before_after.csv"))

cat("\nTop 10 per event (before) that have flagged rows:\n")
print(cmp[n_flagged > 0][order(event_id, rank_before)][, head(.SD, 10), by = event_id][
  , .(event_id, athlete_name, n_flagged, rank_before, rank_after,
      pred_before = round(pred_mark_before, 2), pred_after = round(pred_mark_after, 2))])
cat("
Kipyegon:
"); print(cmp[grepl("Kipyegon", athlete_name)])
cat("\nAthletes in active top 50 whose rank moved 3+:",
    cmp[pmin(rank_before, rank_after, na.rm = TRUE) <= 50 & abs(rank_before - rank_after) >= 3, .N], "\n")
cat("dropped from active list:", cmp[!is.na(ability_before) & is.na(ability_after), .N],
    " total time", format(Sys.time() - t0), "\n")
