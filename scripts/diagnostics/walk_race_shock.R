# Did a whole field run fast together, and did they all come back down?
#
# Pete's test, 2026-09-05: a race where EVERYONE PBs by ~0.3s is a shared race
# shock, and nobody in it should be predicted to repeat that mark. This shows
# the field, the fitted race effect c_r, each athlete's adjusted mark, and --
# the actual test -- what each of them has averaged in the SAME EVENT SINCE.
#
# If the shock is real, subsequent marks regress toward the adjusted times, not
# toward the raw ones.
#
# Usage:
#   CITIUS_SHOCK_NAME="GOUT" CITIUS_SHOCK_EVENT=AT-200Metres-M \
#     Rscript citiusdata/scripts/diagnostics/walk_race_shock.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT   <- here::here("citiusdata", "data")
NAME  <- Sys.getenv("CITIUS_SHOCK_NAME", "GOUT")
EVENT <- Sys.getenv("CITIUS_SHOCK_EVENT", "AT-200Metres-M")
ORI   <- as.data.table(citius_events())[event_id == EVENT]$orientation[1]
cal   <- deployed_calibration(OUT)

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
hits <- unique(ch[event_id == EVENT & grepl(NAME, athlete_name, ignore.case = TRUE),
                  .(athlete_id, athlete_name)])
if (!nrow(hits)) stop("no athlete matching '", NAME, "' in ", EVENT)
cat("matching athletes:\n"); print(hits)

races <- ch[event_id == EVENT & athlete_id %chin% hits$athlete_id & is.finite(mark),
            .(athlete_id, athlete_name, race_key, date, mark, round, competition_id,
              comp_name = if ("comp_name" %in% names(ch)) comp_name else NA_character_)]
setorder(races, mark)
cat("\ntheir fastest marks on record:\n")
print(head(races[, .(athlete_name, date, mark, round, comp_name)], 10))

RK <- Sys.getenv("CITIUS_SHOCK_RACE", races$race_key[1])
target <- races[race_key == RK][1]
cat(sprintf("\n%s\nTHE RACE: %s | %s | %s\n%s\n", strrep("=", 76),
            target$date, target$comp_name, RK, strrep("=", 76)))

# The whole field of that race.
field <- ch[race_key == RK & is.finite(mark),
            .(athlete_id, athlete_name, place, mark, wind = if ("wind" %in% names(ch)) wind else NA_real_)]
setorder(field, place)

# The fitted shared effect for this race.
r <- as.data.table(cal$race)[race_key == RK]
c_r <- if (nrow(r)) r$c_r[1] else NA_real_
n_in <- if (nrow(r)) r$n_in_race[1] else NA_integer_
cat(sprintf("\nfitted race effect c_r = %s  (fitted on %s athletes)\n",
            if (is.finite(c_r)) sprintf("%+.5f", c_r) else "NOT FITTED",
            format(n_in)))
if (is.finite(c_r))
  cat(sprintf("  as a percentage of a mark: %+.3f%%  => on a 20.0s 200m, %+.3f s\n",
              100*(exp(c_r)-1), 20*(exp(c_r)-1)))

# Each athlete's PRIOR best/average (before the race) and SUBSEQUENT average.
hist <- ch[event_id == EVENT & is.finite(mark), .(athlete_id, date, mark)]
prior <- hist[date < target$date, .(n_before = .N, best_before = min(mark),
                                    avg_before = mean(mark)), by = athlete_id]
after <- hist[date > target$date, .(n_after = .N, avg_after = mean(mark),
                                    best_after = min(mark)), by = athlete_id]
f <- merge(field, prior, by = "athlete_id", all.x = TRUE)
f <- merge(f, after, by = "athlete_id", all.x = TRUE)
f[, adjusted := perf_to_mark(ORI * log(mark) - (if (is.finite(c_r)) c_r else 0), ORI)]
f[, vs_before := round(mark - best_before, 3)]
f[, since_vs_race := round(avg_after - mark, 3)]

cat("\nTHE FIELD -- raw mark, what the race effect adjusts it to, and what they\n")
cat("have actually averaged in this event SINCE that day:\n\n")
print(f[, .(place, athlete = substr(athlete_name, 1, 20), mark,
            adjusted = round(adjusted, 3),
            PB_before = best_before, beat_PB_by = -vs_before,
            n_since = n_after, avg_since = round(avg_after, 3),
            slower_since = since_vs_race)])

ok <- f[is.finite(avg_after)]
if (nrow(ok)) {
  # avg_after, not avg_since -- the latter exists only as a display alias in the
  # print above, so referencing it here silently produced NULL and an NaN mean.
  in_race <- mean(ok$mark); adj <- mean(ok$adjusted); since <- mean(ok$avg_after)
  cat(sprintf("\n%d of %d athletes have raced this event since.\n", nrow(ok), nrow(f)))
  cat(sprintf("  mean mark IN that race          : %.3f\n", in_race))
  cat(sprintf("  mean of what they AVERAGE SINCE : %.3f   (%+.3f s)\n", since, since - in_race))
  cat(sprintf("  mean of the ADJUSTED marks      : %.3f   (%+.3f s)\n", adj, adj - in_race))
  cat(sprintf("  beat a personal best that day   : %d of %d\n",
              sum(ok$mark <= ok$best_before, na.rm = TRUE), nrow(ok)))
  cat("\nREAD: the observed regression is the truth the adjustment is trying to\n")
  cat("predict. If the adjustment is LARGER than the regression it over-corrects.\n")
  cat(sprintf("  observed regression : %+.3f s\n", since - in_race))
  cat(sprintf("  c_r adjustment      : %+.3f s\n", adj - in_race))
  cat(sprintf("  => the fitted effect is %.2fx the observed regression\n",
              (adj - in_race) / (since - in_race)))
}
