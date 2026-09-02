# Estimate a VENUE effect empirically, and test it against known altitude.
#
# WHY NOT JUST SOURCE ALTITUDE. We hold venue_city, not elevation, and geocoding
# 25,000 venues is a project. But altitude is not actually the quantity we want:
# for adjusting a mark, what matters is everything about a place that makes it
# fast or slow - thin air, track surface, prevailing temperature, timing. A venue
# effect measured from the marks themselves captures all of it, and needs no
# external data.
#
# THE VALIDATION, which is the point of this script. Altitude has OPPOSITE signs
# by event type: thin air means less drag, so sprints and jumps get FASTER, and
# less oxygen, so distance running gets SLOWER. A venue effect that is merely
# picking up "good meets have fast times" would push both the same way. So:
# estimate venue effects separately for sprints and for distance, then check the
# known high-altitude cities come out fast in one and slow in the other. If they
# do, the effect is real physics and not meet quality.
#
# Ability is absorbed by within-athlete demeaning, exactly as in
# check_wind_effect.R - the outcome is a performance relative to what that
# athlete usually does in that event.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D     <- here::here("citiusdata", "data")
MINV  <- .env_int("VENUE_MIN_MARKS", "200")  # marks per venue
MINA  <- .env_int("VENUE_MIN_ATH", "3")      # marks per athlete-event

# A short, checkable reference set. Elevations in metres, rounded; these are used
# ONLY to validate the empirical effect, never to build it.
# Elevations come from the shared loader, which prefers the 3,101 geocoded
# venues and falls back LOUDLY to the hand-typed 43. Previously this file
# carried its own copy of those 43, which is why the geocoded table sat
# unread after it was built.
source(here::here("citiusdata", "scripts", "_venue_elevation.R"))
ALT <- venue_elevation()[, .(venue_city, alt_m)]

SPRINT <- c("AT-100Metres-M","AT-100Metres-W","AT-200Metres-M","AT-200Metres-W",
            "AT-400Metres-M","AT-400Metres-W","AT-LongJump-M","AT-LongJump-W",
            "AT-TripleJump-M","AT-TripleJump-W")
DIST   <- c("AT-1500Metres-M","AT-1500Metres-W","AT-3000Metres-M","AT-3000Metres-W",
            "AT-5000Metres-M","AT-5000Metres-W","AT-10000Metres-M","AT-10000Metres-W",
            "AT-3000MetresSteeplechase-M","AT-3000MetresSteeplechase-W")

c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id","event_id","perf","date",
                                        "venue_city","indoor","scoreable","tier")))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[scoreable == TRUE & is.finite(perf) & !is.na(venue_city) & nzchar(venue_city) &
         (is.na(indoor) | indoor == FALSE) & event_id %chin% c(SPRINT, DIST)]
c0[, grp := fifelse(event_id %chin% SPRINT, "sprint_jump", "distance")]
c0[, n_ath := .N, by = .(athlete_id, event_id)]
c0 <- c0[n_ath >= MINA]
c0[, y := perf - mean(perf), by = .(athlete_id, event_id)]
cat(sprintf("performances: %s | venues: %s | athlete-events: %s\n",
            format(nrow(c0), big.mark = ","), format(uniqueN(c0$venue_city), big.mark = ","),
            format(uniqueN(paste(c0$athlete_id, c0$event_id)), big.mark = ",")))

# CONTROL FOR MEET TIER. Without it the sprint estimate is confounded: the
# sea-level reference venues are Monaco, Paris, Rome, Zurich, Doha - Diamond
# League meets where athletes peak - while the high-altitude ones are domestic
# South African and Kenyan meets. That made "sea level" mean "elite meet" and
# produced a sprint/elevation correlation of -0.461, the wrong sign for drag.
c0[is.na(tier) | !nzchar(tier), tier := "unknown"]
c0[, y_tier := y - mean(y), by = .(tier, grp)]
cat("marks by tier (the confound being removed):\n")
print(head(c0[, .(marks = .N, mean_dev = round(mean(y), 4)), by = tier][order(-marks)], 8))

ve <- c0[, .(n = .N, eff = mean(y), eff_tier = mean(y_tier)),
         by = .(venue_city, grp)][n >= MINV]
cat(sprintf("venues with >= %d marks in a group: %s\n", MINV,
            format(uniqueN(ve$venue_city), big.mark = ",")))
w <- dcast(ve, venue_city ~ grp, value.var = c("n", "eff", "eff_tier"))
w <- w[is.finite(eff_sprint_jump) & is.finite(eff_distance)]
cat(sprintf("venues with BOTH a sprint and a distance estimate: %d\n", nrow(w)))
stopifnot("no venue has both estimates - cannot run the validation" = nrow(w) > 20)

cat("\n=== THE TEST: do the two groups disagree, as altitude requires? ===\n")
cat(sprintf("correlation between a venue's sprint effect and its distance effect: %.3f\n",
            stats::cor(w$eff_sprint_jump, w$eff_distance)))
cat("Near +1 would mean 'some venues are just fast' - meet quality, not physics.\n")
cat("Near 0 or negative means the two are being pushed by different things.\n")

j <- merge(w, ALT, by = "venue_city")
cat(sprintf("\nreference venues matched: %d of %d\n", nrow(j), nrow(ALT)))
stopifnot("too few reference venues matched to validate" = nrow(j) >= 8)
j[, high := alt_m >= 1000]
cat("\n=== high-altitude venues vs sea level (effect is + = better than usual) ===\n")
print(j[, .(venues = .N,
            sprint_jump = round(mean(eff_sprint_jump), 4),
            distance = round(mean(eff_distance), 4)), by = high][order(-high)])
cat("\nAltitude should make sprint_jump POSITIVE (less drag, faster/further) and\n")
cat("distance NEGATIVE (less oxygen, slower). Both positive would mean the\n")
cat("estimate is picking up meet quality instead.\n")

cat(sprintf("\ncorrelation with elevation: sprint/jump %.3f | distance %.3f\n",
            stats::cor(j$alt_m, j$eff_sprint_jump), stats::cor(j$alt_m, j$eff_distance)))
cat(sprintf("tier-controlled:            sprint/jump %.3f | distance %.3f\n",
            stats::cor(j$alt_m, j$eff_tier_sprint_jump),
            stats::cor(j$alt_m, j$eff_tier_distance)))
cat("A sprint correlation that turns POSITIVE once tier is held constant means\n")
cat("the raw negative was meet quality, and thin air helps as physics says.\n")
print(j[, .(venues = .N,
            sprint_tier = round(mean(eff_tier_sprint_jump), 4),
            distance_tier = round(mean(eff_tier_distance), 4)), by = high][order(-high)])

setorder(j, -alt_m)
cat("\n=== the reference venues, highest first ===\n")
print(j[, .(venue_city, alt_m,
            sprint_jump = round(eff_sprint_jump, 4), n_sj = n_sprint_jump,
            distance = round(eff_distance, 4), n_d = n_distance)])

f <- file.path(D, "venue_effects.parquet")
write_parquet(ve, f)
cat(sprintf("\nwrote %s (%d venue-group effects)\n", basename(f), nrow(ve)))
