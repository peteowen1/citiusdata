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
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
h[, resid := perf - r_pre]

# the shock the engine removes: mean residual over the race, which is what
# `S` approximates. Whatever is LEFT after that is what moves a rating.
h[, S := mean(resid), by = race_key]
h[, after_S := resid - S]
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
