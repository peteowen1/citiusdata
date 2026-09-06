# Walk ONE race through the excess strip: the Gout Gout 200m by default.
#
# The one-example rule. For the chosen race this prints, per entrant: the
# shrunk race effect c_r, what a race of that kind normally shows (E_cell), the
# excess, beta for the race's tier, the amount stripped, the mark as the model
# now reads it, and what the athlete has actually averaged in the same event
# since. Then it estimates the named athlete's ability with and without the
# strip, as of today, so the effect on their card is a number.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/walk_excess_strip.R'
# Env: CITIUS_WALK_NAME (GOUT), CITIUS_WALK_EVENT (AT-200Metres-M),
#      CITIUS_WALK_RACE (default: the athlete's fastest race),
#      CITIUS_WALK_CAL (calibration_race_eb_perevent_persist.rds)
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(arrow)})
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
NAME  <- Sys.getenv("CITIUS_WALK_NAME", "GOUT")
EVENT <- Sys.getenv("CITIUS_WALK_EVENT", "AT-200Metres-M")
CAL   <- Sys.getenv("CITIUS_WALK_CAL", "calibration_race_eb_perevent_persist.rds")
ORI   <- as.data.table(citius_events())[event_id == EVENT]$orientation[1]
cal   <- readRDS(file.path(OUT, CAL))
stopifnot(!is.null(cal$race_shock))
say <- function(...) cat(sprintf(...), "\n", sep = "")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
hits <- unique(ch[event_id == EVENT & grepl(NAME, athlete_name, ignore.case = TRUE), .(athlete_id, athlete_name)])
if (!nrow(hits)) stop("no athlete matching '", NAME, "' in ", EVENT)
names_tbl <- unique(ch[, .(athlete_id, athlete_name)], by = "athlete_id")
rm(ch); invisible(gc())

store <- file.path(OUT, "athletics_corpus_store")
x <- as.data.table(read_results_store(store, events = EVENT,
                                      columns = c("athlete_id", "event_id", "date", "perf", "mark", "race_key", "round", "tier", "meet_tier", "wind", "competition_id")))
x[, athlete_id := as.character(athlete_id)]; x[, date := as.Date(date)]
x <- x[is.finite(perf)]
mine <- x[athlete_id %chin% hits$athlete_id]
RK <- Sys.getenv("CITIUS_WALK_RACE", mine[order(-perf)]$race_key[1])
field <- x[race_key == RK]
if (!nrow(field)) stop("race ", RK, " not in the store")
field <- merge(field, names_tbl, by = "athlete_id", all.x = TRUE)

# --- the strip, step by step ------------------------------------------------
rr <- as.data.table(cal$race)[race_key == RK]
stopifnot(nrow(rr) == 1)
tcl <- .tier_class(rr$tier); rcl <- .round_class(rr$round)
ex  <- as.data.table(cal$race_shock$expected)[event_id == EVENT & tier_class == tcl & round_class == rcl]
e_cell <- if (nrow(ex)) ex$e_cell[1] else NA_real_
bt <- as.data.table(cal$race_shock$by_tier)
beta <- if (nrow(bt) && tcl %in% bt$tier_class) bt[tier_class == tcl]$beta[1] else cal$race_shock$beta
beta <- min(max(beta, 0), 1)
excess <- rr$c_r - e_cell
ev <- as.data.table(cal$events)[event_id == EVENT]
k  <- (ev$sigma_within / ev$condition_sd)^2
wt <- rr$n_in_race / (rr$n_in_race + k)
strip <- (1 - beta) * excess * wt
cat(sprintf("\n%s\nTHE RACE %s | %s | tier %s (%s) | round %s (%s) | %d in race\n%s\n", strrep("=", 78),
            RK, format(field$date[1]), rr$tier, tcl, rr$round, rcl, rr$n_in_race, strrep("=", 78)))
cat(sprintf("shrunk race effect c_r      %+.4f  (%+.2f%% of a mark)\n", rr$c_r, 100 * rr$c_r))
cat(sprintf("expected for this kind      %+.4f  (event x %s x %s, %d races)\n", e_cell, tcl, rcl, if (nrow(ex)) ex$n_cell[1] else 0L))
cat(sprintf("excess                      %+.4f  (%+.2f%%)\n", excess, 100 * excess))
cat(sprintf("beta (tier %s)              %.3f   -> strip share %.3f\n", tcl, beta, 1 - beta))
cat(sprintf("field-size weight           %.3f   (n/(n+k), k = %.2f)\n", wt, k))
cat(sprintf("STRIPPED from each mark     %+.4f  (%+.2f%%, about %.2f s on a 20 s race)\n", strip, 100 * strip, 20 * strip))

# --- the field, before and after, and what they did since --------------------
since <- x[date > field$date[1], .(n_since = .N, avg_since = mean(mark), best_since = min(mark)), by = athlete_id]
f <- merge(field[, .(athlete_id, athlete_name, mark, perf)], since, by = "athlete_id", all.x = TRUE)
f[, mark_as_read := perf_to_mark(perf - strip, ORI)]
setorder(f, mark)
cat("\nthe field (mark as run -> mark as the model now reads it -> what they averaged in the event since):\n")
print(f[, .(athlete_name, mark, mark_as_read = round(mark_as_read, 2), n_since, avg_since = round(avg_since, 2), best_since)])

# --- the named athlete's card ---------------------------------------------------
aid <- hits$athlete_id[1]
hist <- x[athlete_id == aid]
as_of <- Sys.Date()
off <- estimate_ability(hist, as_of = as_of, calibration = cal, adjust_race = FALSE)
on  <- estimate_ability(hist, as_of = as_of, calibration = cal, adjust_race = TRUE)
cat(sprintf("\n%s: %d results in %s. Ability as of %s -> median mark:\n", hits$athlete_name[1], nrow(hist), EVENT, format(as_of)))
cat(sprintf("  without the strip  %.2f\n  with the strip     %.2f   (delta %+.2f s)\n",
            perf_to_mark(off$ability, ORI), perf_to_mark(on$ability, ORI),
            perf_to_mark(on$ability, ORI) - perf_to_mark(off$ability, ORI)))
cat(sprintf("  their last 5 marks: %s | average since the race: %.2f\n",
            paste(head(hist[order(-date)]$mark, 5), collapse = ", "),
            if (aid %in% since$athlete_id) since[athlete_id == aid]$avg_since else NA))
