# Does throw's -2.43% bias come from best-of-N attempt structure?
#
# The refuted "best-of-N tails" hypothesis (refuted-hypotheses.md) is about
# SKEW -- measured skew says field events are the LEAST skewed, so a longer
# bad-side tail was dropped. This is a different claim: not tail SHAPE but
# LOCATION. If the recorded mark is the max of ~6 attempts and the model
# effectively predicts a single draw, the level can be off even when the
# shape is right.
#
# The discriminating cut is INSIDE the jump family, not between families:
#   horizontal (LongJump, TripleJump) -- best of 6 measured attempts,
#     structurally identical to every throw
#   vertical   (HighJump, PoleVault)  -- progressive bar heights, the mark is
#     the last height CLEARED, not the max of six independent draws
# Same family, same context corrections, same fitting code path. If attempt
# structure drives the bias, horizontal jumps should sit with throw and
# vertical jumps should not. If both halves of jump look alike, the mechanism
# is not attempt structure.
#
# Read-only against calibration_sweep_data.rds (written by
# check_calibration_sweep.R, deployed calibration backtest_ctrl_now.rds).
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf(...), "\n", sep = "")

f <- file.path(D, "calibration_sweep_data.rds")
stopifnot("run check_calibration_sweep.R first" = file.exists(f))
d <- as.data.table(readRDS(f))
say("loaded %s predictions, %s races, %s-%s",
    format(nrow(d), big.mark = ","), format(uniqueN(d$race_id), big.mark = ","),
    min(d$year), max(d$year))
stopifnot(nrow(d) > 0, all(c("bias_pct", "family", "discipline", "meet_tier") %in% names(d)))
stopifnot("bias_pct must be populated" = mean(is.finite(d$bias_pct)) > 0.99)

# Attempt structure, assigned explicitly rather than inferred from family.
d[, structure := fifelse(family == "throw", "best_of_6_throw",
                  fifelse(discipline %chin% c("Long Jump", "Triple Jump"), "best_of_6_horiz_jump",
                   fifelse(discipline %chin% c("High Jump", "Pole Vault"), "progressive_vertical",
                    fifelse(family %chin% c("sprint", "hurdles"), "single_attempt_track",
                     "other"))))]
say("\ndisciplines mapped to each structure (guards against a silent name mismatch):")
print(d[, .(n = .N, disciplines = paste(sort(unique(discipline)), collapse = ", ")), by = structure])
stopifnot("no horizontal jumps matched -- discipline names have changed" =
            d[structure == "best_of_6_horiz_jump", .N] > 0)
stopifnot("no vertical jumps matched -- discipline names have changed" =
            d[structure == "progressive_vertical", .N] > 0)

# Race-clustered SE: predictions inside one race share a condition shock, so
# treating rows as independent overstates significance. Cluster on race_id.
clustered <- function(x, race) {
  by_race <- data.table(x = x, race = race)[, .(m = mean(x), n = .N), by = race]
  n_r <- nrow(by_race)
  if (n_r < 2) return(list(est = mean(x), se = NA_real_, races = n_r))
  w <- by_race$n / sum(by_race$n)
  est <- sum(w * by_race$m)
  se <- sqrt(sum(w^2 * (by_race$m - est)^2) * n_r / max(n_r - 1, 1))
  list(est = est, se = se, races = n_r)
}

summarise <- function(dt, label) {
  if (!nrow(dt)) return(NULL)
  cl <- clustered(dt$bias_pct, dt$race_id)
  data.table(cut = label, n = nrow(dt), races = cl$races,
             bias_pct = cl$est, se = cl$se,
             t = cl$est / cl$se,
             lo95 = cl$est - 1.96 * cl$se, hi95 = cl$est + 1.96 * cl$se)
}

report <- function(dt, header) {
  cat("\n================ ", header, " ================\n", sep = "")
  out <- rbindlist(lapply(split(dt, dt$structure), function(s)
    summarise(s, s$structure[1])), fill = TRUE)
  if (!nrow(out)) { say("no rows"); return(invisible(NULL)) }
  setorder(out, bias_pct)
  print(out)
  invisible(out)
}

t1 <- report(d[meet_tier == "T1_elite" & structure != "other"], "T1_elite, by attempt structure")
report(d[structure != "other"], "all tiers pooled, by attempt structure")

cat("\n================ jump split out by discipline (T1_elite) ================\n")
jd <- d[meet_tier == "T1_elite" & family == "jump"]
print(rbindlist(lapply(split(jd, jd$discipline), function(s)
  summarise(s, s$discipline[1])), fill = TRUE)[order(bias_pct)])

cat("\n================ throw split out by discipline (T1_elite) ================\n")
td <- d[meet_tier == "T1_elite" & family == "throw"]
print(rbindlist(lapply(split(td, td$discipline), function(s)
  summarise(s, s$discipline[1])), fill = TRUE)[order(bias_pct)])

# The verdict the whole script exists to deliver, stated in one line so it
# cannot be read off the wrong row.
if (!is.null(t1) && all(c("best_of_6_throw", "best_of_6_horiz_jump", "progressive_vertical") %in% t1$cut)) {
  thr <- t1[cut == "best_of_6_throw"]
  hor <- t1[cut == "best_of_6_horiz_jump"]
  ver <- t1[cut == "progressive_vertical"]
  gap_hor <- thr$bias_pct - hor$bias_pct
  gap_ver <- hor$bias_pct - ver$bias_pct
  cat("\n================ VERDICT ================\n")
  say("throw %.2f%% | horizontal jump %.2f%% | vertical jump %.2f%%",
      thr$bias_pct, hor$bias_pct, ver$bias_pct)
  say("throw vs horizontal jump (same structure, should MATCH if attempts drive it): %+.2f pp", gap_hor)
  say("horizontal vs vertical jump (different structure, should DIFFER if attempts drive it): %+.2f pp", gap_ver)
  say("")
  say("supports best-of-N only if |throw - horizontal| is small AND")
  say("|horizontal - vertical| is large. Both gaps small, or the wrong one large,")
  say("means attempt structure is not the mechanism.")
}
