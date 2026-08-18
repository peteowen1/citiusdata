# Is the favourite penalty attenuation bias? Measure it, do not assume it.
#
# THE SYMPTOM. In major finals the engine's surprise is systematically positive
# for weakly-rated athletes and negative for strongly-rated ones - monotone
# across five rating bands, about 1.4 seconds of rating per major final. Winners
# get docked for winning. That is what puts Barega too high and Almgren too low.
#
# THE HYPOTHESIS. r_pre is a NOISY measure of ability. The engine computes
# surprise = perf - r_pre, which assumes E[perf | r_pre] = r_pre, i.e. a slope of
# exactly 1. Under measurement error the true conditional slope is
#     b = var(ability) / (var(ability) + v_pre)  <  1
# so the engine over-predicts the strong and under-predicts the weak, and the
# residual is monotone in the rating. That is the pattern observed.
#
# WHY IT SHOULD BITE HARDEST IN A MAJOR FINAL. b depends on the SPREAD of true
# ability in the field relative to the noise. A championship final is the
# tightest field of the year, so var(ability) is smallest exactly where the
# rating noise is unchanged - attenuation is worst where it costs most.
#
# WHAT THIS SCRIPT DOES. Nothing is fitted or changed. It estimates b three ways
# and checks they agree, because a fix built on an uncalibrated v_pre would be
# worse than no fix:
#   1. measured b, by regressing within-race-centred perf on centred r_pre;
#   2. predicted b from v_pre alone, 1 - mean(v_pre)/var(r_pre) within race;
#   3. measured b by v_pre quintile and by field spread.
# If (1) and (2) agree, v_pre is calibrated and the correction is exactly the
# shrinkage it implies. If (1) is well below (2), v_pre understates the noise and
# the correction must be scaled empirically rather than derived.
#
# Centring WITHIN race is not a convenience: a shock shared by the whole field
# cancels out of every within-race comparison, so centring removes the race
# conditions term exactly and leaves the athlete-specific part, which is the only
# part that can reorder a field.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D    <- here::here("citiusdata", "data")
TAG  <- Sys.getenv("FORM_TAG", "final")
MINN <- as.integer(Sys.getenv("ATT_MINFIELD", "5"))
# FIT ON THE TUNE WINDOW ONLY. b is a parameter like any other; estimating it
# over 2020-2026 and then "confirming" it on 2026 confirms nothing. The default
# stops at 2025 so the sealed window takes no part in choosing the value.
YRS <- as.integer(trimws(strsplit(Sys.getenv("ATT_YEARS",
         "2020,2021,2022,2023,2024,2025"), ",")[[1]]))
OUT <- Sys.getenv("ATT_OUT", "")
stopifnot("ATT_YEARS parsed to nothing" = length(YRS) > 0 && all(is.finite(YRS)))

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","date","event_id","athlete_id",
                                       "r_pre","v_pre","perf","place","rc","seen","n_eff")))
n0 <- nrow(h)
h <- h[seen == TRUE & is.finite(r_pre) & is.finite(perf) & is.finite(v_pre) & v_pre > 0]
h <- h[year(date) %in% YRS]
h[, nf := .N, by = race_key]
h <- h[nf >= MINN]
stopifnot("nothing survived the filters" = nrow(h) > 1000)
cat(sprintf("fit window: %s
", paste(range(YRS), collapse = "-")))
cat(sprintf("%s: %s of %s athlete-races usable | %s races | %s to %s\n", TAG,
            format(nrow(h), big.mark = ","), format(n0, big.mark = ","),
            format(uniqueN(h$race_key), big.mark = ","), min(h$date), max(h$date)))

# within-race centring: removes the shared shock exactly
h[, `:=`(pc = perf - mean(perf), rc_ = r_pre - mean(r_pre)), by = race_key]
# spread of ratings in this field, and the average rating noise in it
h[, `:=`(vr = mean(rc_^2), vn = mean(v_pre)), by = race_key]

slope <- function(x, y) if (sum(x^2) > 0) sum(x * y) / sum(x^2) else NA_real_
b_all <- slope(h$rc_, h$pc)
cat(sprintf("\nMEASURED slope of perf on rating, all races: %.4f\n", b_all))
cat("A slope of 1 means the engine's surprise is unbiased. Below 1 means every\n")
cat("race docks the favourite and credits the outsider, by construction.\n")

# predicted slope from v_pre alone, per race, pooled by weight
h[, w := nf]
b_pred <- h[, {v <- mean(vr); n <- mean(vn); .(b = max(0, (v - n) / v), w = .N)},
            by = race_key][is.finite(b), weighted.mean(b, w)]
cat(sprintf("PREDICTED slope from v_pre alone:            %.4f\n", b_pred))
cat("These two must agree for v_pre to be trusted as the correction's input.\n")

cat("\n=== by round: a championship final is the tightest field of the year ===\n")
h[, rnd := fifelse(rc == "final", "final", "heat/semi")]
print(h[, .(races = uniqueN(race_key), athlete_races = .N,
            field_spread = round(sqrt(mean(vr)), 4),
            rating_noise = round(sqrt(mean(vn)), 4),
            measured_b = round(slope(rc_, pc), 4),
            predicted_b = round(max(0, (mean(vr) - mean(vn)) / mean(vr)), 4)),
        by = rnd][order(rnd)])

cat("\n=== by how tight the field is (quintile of rating spread) ===\n")
h[, sq := cut(vr, quantile(vr, 0:5/5, na.rm = TRUE), labels = 1:5,
              include.lowest = TRUE)]
print(h[!is.na(sq), .(races = uniqueN(race_key),
                      spread = round(sqrt(mean(vr)), 4),
                      noise = round(sqrt(mean(vn)), 4),
                      measured_b = round(slope(rc_, pc), 4),
                      predicted_b = round(max(0, (mean(vr) - mean(vn)) / mean(vr)), 4)),
        by = sq][order(sq)])
cat("Tightest fields are quintile 1. If measured_b falls as the field tightens,\n")
cat("the mechanism is confirmed: attenuation is worst where the race matters most.\n")

cat("\n=== by the athlete's OWN rating noise (quintile of v_pre) ===\n")
h[, vq := cut(v_pre, quantile(v_pre, 0:5/5, na.rm = TRUE), labels = 1:5,
              include.lowest = TRUE)]
print(h[!is.na(vq), .(athlete_races = .N, v_pre = round(mean(v_pre), 5),
                      median_n_eff = round(median(n_eff), 1),
                      measured_b = round(slope(rc_, pc), 4)),
        by = vq][order(vq)])
cat("A thinly-raced athlete (high v_pre, quintile 5) should show MORE attenuation\n")
cat("than a deeply-raced one. If the slope is flat here, v_pre is not tracking\n")
cat("real uncertainty and a v_pre-based correction would be arbitrary.\n")

cat("\n=== by family ===\n")
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
hf <- merge(h, reg, by = "event_id")
print(hf[, .(races = uniqueN(race_key),
             spread = round(sqrt(mean(vr)), 4),
             measured_b = round(slope(rc_, pc), 4),
             predicted_b = round(max(0, (mean(vr) - mean(vn)) / mean(vr)), 4)),
         by = family][order(measured_b)])

# The symptom itself, restated as the engine sees it, so the fix can be checked
# against the same number it is meant to move.
cat("\n=== the favourite penalty, as mean surprise by rating band ===\n")
h[, rb := cut(rc_, quantile(rc_, 0:5/5, na.rm = TRUE), labels = 1:5,
              include.lowest = TRUE)]
print(h[!is.na(rb), .(athlete_races = .N,
                      mean_rating_vs_field = round(mean(rc_), 4),
                      mean_surprise = round(mean(pc - rc_), 5),
                      corrected = round(mean(pc - b_all * rc_), 5)),
        by = rb][order(rb)])
cat("Column 3 is what the engine feeds its update today; column 4 is what it\n")
cat("would feed after a single global slope. Column 3 monotone in the rating is\n")
cat("the bug. Column 4 flat is the fix working - and it must be flat WITHOUT\n")
cat("having been fitted to flatten it, which is why b came from the regression\n")
cat("above rather than from these bands.\n")

# --- ship the per-family slopes, if asked -------------------------------------
# Written as a per-EVENT override because that is the only channel the engine
# has; every event in a family carries its family's slope.
if (nzchar(OUT)) {
  fam <- hf[, .(races = uniqueN(race_key), n = .N, b = slope(rc_, pc)), by = family]
  hf[, resid := pc - slope(rc_, pc) * rc_, by = family]
  se <- hf[, .(se = sqrt(sum(resid^2) / (.N - 1) / sum(rc_^2))), by = family]
  fam <- merge(fam, se, by = "family")[, sds_below_1 := (1 - b) / se]
  setorder(fam, b)
  cat("
=== per-family slopes to be shipped ===
")
  print(fam[, .(family, races, b = round(b, 4), se = round(se, 4),
                sds_below_1 = round(sds_below_1, 1))])
  reg2 <- as.data.table(citius::citius_events())[, .(event_id, family)]
  o <- merge(reg2, fam[, .(family, atten = b)], by = "family")[, .(event_id, atten)]
  stopifnot("some event got no slope" = !anyNA(o$atten),
            "a slope outside (0, 1.2] is not a correction, it is a bug" =
              all(o$atten > 0 & o$atten <= 1.2))
  write_parquet(o, OUT)
  cat(sprintf("
wrote %s: %d events across %d families
",
              basename(OUT), nrow(o), nrow(fam)))
}
