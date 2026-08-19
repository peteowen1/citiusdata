# Does the model mark athletes DOWN for winning a championship?
#
# Kerr's Mile rating went 3:44.21 -> 3:48.41 across the Commonwealth Games,
# where he won the heat in 3:58.57 and the final in 3:54.12. He won the title
# and the model rated him worse for it.
#
# The suspected mechanism: a championship final is run tactically, so the TIME
# is slow while the QUALITY of the performance is high. The rating tracks time.
# The race-condition shock S is supposed to absorb a uniformly slow race - it is
# the mean surprise over established athletes, scaled by their share of the
# field - so if it works, a tactical final should leave no systematic residue.
#
# This measures whether it does. If T1 finals carry a systematically negative
# residual after S, then championship performances are being read as decline,
# and SEQ_KT1 (which scales the learning rate for T1_elite races, default 1) is
# the existing knob for it.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
TAG <- Sys.getenv("HIST_TAG", "final")
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
cat(sprintf("arm: %s
", TAG))
h <- h[is.finite(perf) & is.finite(r_pre)]   # keep UNSEEN rows: they set the scale

# THE ENGINE'S ACTUAL SHOCK, not the plain race mean.
#   S <- (if (sum(est) >= 3) mean(perf[est] - r_pre[est]) else 0) * (sum(est)/length(a))
# The distinction is the whole measurement. Subtracting the plain mean makes the
# residual mean-centred BY CONSTRUCTION, so it is identically zero and tests
# nothing - which is what the first version of this script did. The scaling by
# the established athletes' share is what leaves a residue: a field that is only
# half rated absorbs only half the shock.
h[, resid := perf - r_pre]
h[, n_all := .N, by = race_key]
h[, n_est := sum(seen == TRUE), by = race_key]
h[, S := fifelse(n_est >= 3L,
                 mean(resid[seen == TRUE], na.rm = TRUE) * (n_est / n_all), 0),
  by = race_key]
h <- h[seen == TRUE]
h[, after_S := resid - S]
cat(sprintf("median established share of a field: %.2f | races where it is under 0.9: %.1f%%\n",
            stats::median(h$n_est / h$n_all),
            100 * mean(unique(h[, .(race_key, sh = n_est / n_all)])$sh < 0.9)))
cat(sprintf("scored athlete-races: %s\n", format(nrow(h), big.mark = ",")))

cat("\n=== residual by round, before and after the shared shock ===\n")
cat("`raw` includes the race being slow; `after_S` is what actually moves a rating.\n")
print(h[, .(races = uniqueN(race_key), rows = .N,
            raw = round(mean(resid), 5),
            after_S = round(mean(after_S), 5)), by = rc][order(rc)])

cat("\n=== the same, split by meet tier ===\n")
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
rk <- unique(h[, .(race_key)])
# race_key carries the competition id as its leading token in this corpus
rk[, competition_id := sub("\\|.*$", "", race_key)]
rk <- merge(rk, cat0[, .(competition_id = as.character(competition_id), class, meet_tier)],
            by = "competition_id", all.x = TRUE)
h2 <- merge(h, rk[, .(race_key, class, meet_tier)], by = "race_key", all.x = TRUE)
print(h2[!is.na(meet_tier), .(races = uniqueN(race_key),
         raw = round(mean(resid), 5), after_S = round(mean(after_S), 5)),
         by = .(meet_tier, rc)][order(meet_tier, rc)])

cat("\n=== championship finals specifically ===\n")
maj <- h2[class %chin% c("olympics", "world_champs", "european_champs",
                         "commonwealth", "world_indoor") & rc == "final"]
if (nrow(maj)) {
  cat(sprintf("major finals: %d races, %d performances\n",
              uniqueN(maj$race_key), nrow(maj)))
  cat(sprintf("  raw residual   %+.5f  (negative = slower than ratings expected)\n",
              mean(maj$resid)))
  cat(sprintf("  after shock S  %+.5f\n", mean(maj$after_S)))
  cat("\nby family, after the shock:\n")
  reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
  mm <- merge(maj, reg, by = "event_id", all.x = TRUE)
  print(mm[, .(rows = .N, after_S = round(mean(after_S), 5)), by = family][order(after_S)])
}
cat("\nIf after_S is near zero the shock is doing its job and a tactical final\n")
cat("costs nothing. A systematically negative value in the endurance families\n")
cat("would mean championship racing is being read as decline.\n")

# --- WHO PAYS, THEN? ---------------------------------------------------------
# The shock removes what the FIELD shared. An athlete rated far above that field
# is expected to beat it by a distance, so in a slow tactical race their personal
# shortfall survives the shock - even if they win. Kerr was rated ~3:42 off one
# Mile and won the Commonwealth final in 3:54.
#
# So the question is not "are championships penalised" (they are not) but
# "are FAVOURITES penalised in slow races".
h[, fav_gap := r_pre - mean(r_pre), by = race_key]
maj2 <- h[race_key %chin% maj$race_key]
if (nrow(maj2)) {
  maj2[, band := cut(fav_gap, breaks = stats::quantile(fav_gap, 0:5/5, na.rm = TRUE),
                     labels = c("weakest 20%", "20-40%", "middle", "60-80%",
                                "strongest 20%"), include.lowest = TRUE)]
  cat("\n=== in major finals: does the FAVOURITE lose rating? ===\n")
  cat("fav_gap is how far an athlete's rating sits above the field's mean.\n")
  print(maj2[!is.na(band), .(rows = .N,
             mean_gap = round(mean(fav_gap), 4),
             after_S = round(mean(after_S), 5),
             pct_negative = round(100 * mean(after_S < 0), 1)), by = band][order(band)])
  cat("\nA monotone decline down this table means the model docks the strongest\n")
  cat("athlete for winning a slow race - the shock cannot help them, because it\n")
  cat("only removes what the whole field shared.\n")
}
