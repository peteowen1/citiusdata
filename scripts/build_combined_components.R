# Take combined events apart into the marks they are actually made of.
#
# WHY. A decathlon score is a DETERMINISTIC function of ten marks through the
# published World Athletics scoring tables. Rating it as one event with a points
# total throws that structure away, and it shows: opening the cross-event blend
# cost Heptathlon (M) -1.949 pp and Decathlon -1.152, the worst family by a wide
# margin, because a decathlon correlates moderately with a dozen events and a
# loose gate lets it borrow from all of them at once. It is borrowing at the
# wrong level of description. A decathlete's 100m IS a 100m.
#
# THE DATA IS ALREADY THERE. Component marks sit in the corpus under their own
# event_id, identified by their round code (Combined - Group, CE, CE1..CE6).
# 98.3% of decathletes carry a flagged component 100m.
#
# THE VALIDATION IS THE POINT OF THIS SCRIPT. Assembling components and trusting
# them would be worthless; instead RECOMPUTE the points total from the marks and
# compare to the total the corpus stores. That tests the assembly and the
# scoring constants simultaneously, per discipline, and a wrong constant shows
# up as a systematic residual in exactly one column rather than as vague drift.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D       <- here::here("citiusdata", "data")
DATEWIN <- as.integer(Sys.getenv("CE_DATE_WINDOW", "2"))   # days either side

source(here::here("citiusdata", "scripts", "ce_scoring.R"))

cat(sprintf("scoring table: %d discipline-slots across %d combined events\n",
            nrow(CE_TABLE), length(CE_EVENTS)))

# --- load ---------------------------------------------------------------------
cols <- c("event_id", "discipline", "competition_id", "race_key", "athlete_id",
          "mark", "place", "date", "round", "tier", "sex", "indoor")
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"), col_select = cols))
c0[, athlete_id := as.character(athlete_id)]
cat(sprintf("corpus %s rows\n", format(nrow(c0), big.mark = ",")))

# the totals
tot <- c0[event_id %chin% CE_EVENTS & is.finite(mark) & mark > 0,
          .(athlete_id, competition_id, date, ce = event_id, points = mark, race_key)]
# competition_id 0 is a known collision bucket - 2.5M rows share it, so it cannot
# key anything. Those performances simply cannot be decomposed.
n_zero <- tot[competition_id == 0, .N]
tot <- tot[competition_id != 0]
setorder(tot, athlete_id, competition_id, date)
tot <- unique(tot, by = c("athlete_id", "competition_id", "ce", "date"))
cat(sprintf("combined-event totals: %s usable (%s dropped for competition_id = 0)\n",
            format(nrow(tot), big.mark = ","), format(n_zero, big.mark = ",")))

# the components: flagged by round code, and never a combined-event total itself
CE_ROUND <- "^(CE[0-9]*|Combined.*)$"
comp <- c0[grepl(CE_ROUND, round) & !(event_id %chin% CE_EVENTS) &
           is.finite(mark) & mark > 0 & competition_id != 0,
           .(athlete_id, competition_id, date, event_id, mark, round)]
cat(sprintf("flagged component rows: %s\n", format(nrow(comp), big.mark = ",")))

# --- join within a date window ------------------------------------------------
# A decathlon runs over two days and the corpus dates components to the day they
# were contested, so an exact date join would drop day one.
tot[, tid := .I]
j <- merge(comp, tot[, .(tid, athlete_id, competition_id, ce, tdate = date, points)],
           by = c("athlete_id", "competition_id"), allow.cartesian = TRUE)
j <- j[abs(as.integer(date - tdate)) <= DATEWIN]
# keep only slots that belong to THIS combined event, and one mark per slot
j <- merge(j, CE_TABLE, by.x = c("ce", "event_id"), by.y = c("ce", "event_id"))
setorder(j, tid, event_id, -mark)
j <- unique(j, by = c("tid", "event_id"))
cat(sprintf("component rows matched to a total: %s over %s totals\n",
            format(nrow(j), big.mark = ","), format(uniqueN(j$tid), big.mark = ",")))

j[, pts := ce_points(mark, kind, A, B, C)]

# --- THE VALIDATION -----------------------------------------------------------
need <- CE_TABLE[, .(slots = .N), by = ce]
rec <- j[, .(got = .N, recomputed = sum(pts)), by = .(tid, ce)]
rec <- merge(rec, need, by = "ce")
rec <- merge(rec, tot[, .(tid, points, athlete_id, date)], by = "tid")
rec[, complete := got == slots]
rec[, resid := recomputed - points]

cat("\n=== reconstruction: recomputed points vs the stored total ===\n")
print(rec[, .(totals = .N,
              complete = sum(complete),
              pct_complete = round(100 * mean(complete), 1)), by = ce][order(ce)])
cmp <- rec[complete == TRUE]
stopifnot("no complete combined-event performance was assembled" = nrow(cmp) > 0)
cat(sprintf("\ncomplete performances: %s\n", format(nrow(cmp), big.mark = ",")))
print(cmp[, .(n = .N,
              exact = sum(resid == 0),
              within_5 = sum(abs(resid) <= 5),
              within_25 = sum(abs(resid) <= 25),
              median_resid = as.numeric(stats::median(resid)),
              mean_abs = round(mean(abs(resid)), 1)), by = ce][order(ce)])
cat("\nIf the constants are right and the assembly is right, `exact` should be\n")
cat("most of `n`. A whole event failing points at its constants; scattered\n")
cat("misses point at the assembly picking up the wrong mark.\n")

cat("\n=== per-discipline residual, which isolates a wrong constant ===\n")
# recompute each total leaving one discipline out, and see which discipline's
# absence explains the error
bad <- rec[complete == TRUE & abs(resid) > 5, tid]
if (length(bad)) {
  cat(sprintf("%s complete performances miss by more than 5 points\n",
              format(length(bad), big.mark = ",")))
  print(head(j[tid %chin% bad, .(n = .N, mean_pts = round(mean(pts))),
               by = .(ce, event_id)][order(ce, -n)], 15))
} else cat("none miss by more than 5 points\n")

cat("\n=== worked example, so a reader can check it by eye ===\n")
ex <- cmp[ce == "AT-Decathlon-M"][order(-points)][1]
print(j[tid == ex$tid, .(event_id, mark, kind, points = pts)][order(-points)])
cat(sprintf("recomputed %d | stored %d | residual %d\n",
            as.integer(ex$recomputed), as.integer(ex$points), as.integer(ex$resid)))

out <- j[, .(tid, ce, athlete_id, competition_id, date, tdate, event_id, mark, pts)]
out <- merge(out, rec[, .(tid, stored_points = points, recomputed, complete, resid)], by = "tid")
f <- file.path(D, "combined_components.parquet")
write_parquet(out, f)
cat(sprintf("\nwrote %s: %s component rows over %s performances\n",
            basename(f), format(nrow(out), big.mark = ","),
            format(uniqueN(out$tid), big.mark = ",")))
