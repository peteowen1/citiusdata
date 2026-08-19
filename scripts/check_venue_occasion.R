# Does the venue effect measure the PLACE, or the OCCASION?
#
# THE WORRY. `venue_adj` is meant to absorb altitude, track and climate - real
# properties of a place, which should be corrected away. But athletes run fast at
# Monaco partly because they TARGET Monaco, and that is form, not geography.
# Correcting it away deletes real information. Tier demeaning removes the average
# effect of a meet class; it cannot remove an athlete choosing to peak there.
# The assumed fix was an athlete-meet interaction, i.e. a much bigger model.
#
# THE TEST, which needs no new model. Split every venue's rows by whether the
# athlete was TARGETING that meet, using only properties of the meet and never
# the mark:
#   targeted - the meet is the highest-prestige tier that athlete competed at all
#              season. This is their peak; they built toward it.
#   tune-up  - the athlete competed at a strictly higher tier elsewhere that
#              season, so this meet was on the way to something else.
# Then estimate the venue effect TWICE, once from each pool, and compare.
#
#   If the effect is PLACE, both pools measure the same thin air and the same
#   fast track. The estimates agree, and a regression of tune-up on targeted has
#   slope 1.
#   If the effect is OCCASION, the targeted pool is inflated by peaking that the
#   tune-up pool does not contain. Slope < 1, and targeted effects are wider.
#
# The slope IS the answer, and it is directly actionable: a slope near 1 means
# the deployed number is clean, and a slope well under 1 means it is inflated by
# roughly that factor - fixable by estimating from tune-up rows alone, which is
# a filter, not a bigger model.
#
# WHY THIS IS NOT CIRCULAR. Targeting is assigned from the meet's tier and the
# athlete's season schedule. Nothing in the split looks at how fast they ran.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D     <- here::here("citiusdata", "data")
MINA  <- as.integer(Sys.getenv("ADJ_MIN_ATH",   "4"))    # marks per athlete-event
VKAP  <- as.numeric(Sys.getenv("ADJ_VENUE_KAPPA", "400"))
MINC  <- as.integer(Sys.getenv("OCC_MIN_CELL", "40"))    # marks per pool per cell

reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id","event_id","race_key","perf",
                                        "date","indoor","scoreable","venue_city","tier")))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[scoreable == TRUE & is.finite(perf)]
.n_pre <- nrow(c0)
c0 <- merge(c0, reg, by = "event_id")
stopifnot("the registry merge dropped performances" = nrow(c0) == .n_pre)

# wind comes from the deployed correction rather than being refitted, so this
# sits on exactly the residual the venue term is estimated from
am <- setDT(read_parquet(file.path(D, "adjusted_marks.parquet"),
                         col_select = c("race_key","athlete_id","event_id","wind_adj")))
am[, athlete_id := as.character(athlete_id)]
c0 <- merge(c0, am, by = c("race_key","athlete_id","event_id"), all.x = TRUE)
c0[!is.finite(wind_adj), wind_adj := 0]
cat(sprintf("corpus: %s rows | %s carry a wind correction\n",
            format(nrow(c0), big.mark = ","),
            format(sum(c0$wind_adj != 0), big.mark = ",")))

# --- who was targeting what ---------------------------------------------------
# World Athletics competition categories, most prestigious first. Enumerated and
# asserted rather than sorted alphabetically: "A" would outrank "OW" on a naive
# sort, which silently inverts the whole split.
PRESTIGE <- c("OW", "GW", "DF", "GL", "A", "B", "C", "D", "E", "F", "G")
v <- c0[!is.na(venue_city) & nzchar(venue_city) & (is.na(indoor) | indoor == FALSE)]
v <- v[!is.na(tier) & nzchar(tier)]
stopifnot("a tier value is missing from the prestige order" =
            all(unique(v$tier) %chin% PRESTIGE))
v[, prestige := match(tier, PRESTIGE)]                    # 1 = most prestigious
v[, season := as.integer(format(date, "%Y"))]
v[, best_prestige := min(prestige), by = .(athlete_id, season)]
v[, pool := fifelse(prestige == best_prestige, "targeted", "tuneup")]
pt <- v[, .N, by = pool][order(pool)]
cat(sprintf("\nsplit: %s targeted, %s tune-up (%.1f%% tune-up)\n",
            format(pt[pool == "targeted", N], big.mark = ","),
            format(pt[pool == "tuneup", N], big.mark = ","),
            100 * pt[pool == "tuneup", N] / nrow(v)))
stopifnot("one pool is empty - the split failed" = nrow(pt) == 2 & all(pt$N > 1000))

# --- the venue effect, estimated exactly as deployed but within a pool ---------
# Same three steps as build_adjusted_marks.R: wind, then ability, then the
# leave-one-out tier mean. Run inside each pool so the two estimates are built
# by identical code and differ only in which rows they saw.
# NOT SHRUNK, and the per-cell standard error kept. Shrinkage would pull the two
# pools by different amounts wherever their cell counts differ, manufacturing
# exactly the asymmetry being tested for. The se is needed because both estimates
# are noisy, and the comparison below has to know how noisy.
venue_effect <- function(x) {
  x <- copy(x)
  x[, n_ath := .N, by = .(athlete_id, event_id)]
  x <- x[n_ath >= MINA]
  if (!nrow(x)) return(data.table())
  x[, y := perf - wind_adj]
  x[, y := y - mean(y), by = .(athlete_id, event_id)]
  x[, `:=`(t_sum = sum(y), t_n = .N), by = .(tier, family)]
  x[, `:=`(vt_sum = sum(y), vt_n = .N), by = .(tier, family, venue_city)]
  x[, others_n := t_n - vt_n]
  x[, t_mean := fifelse(others_n >= 30, (t_sum - vt_sum) / pmax(others_n, 1),
                        t_sum / t_n)]
  x[, y := y - t_mean]
  x[, .(n_v = .N, eff = mean(y), ev = stats::var(y) / .N),
    by = .(venue_city, family)]
}
a <- venue_effect(v[pool == "targeted"])[, .(venue_city, family, n_t = n_v,
                                             eff_t = eff, ev_t = ev)]
b <- venue_effect(v[pool == "tuneup"])[,   .(venue_city, family, n_u = n_v,
                                             eff_u = eff, ev_u = ev)]
w <- merge(a, b, by = c("venue_city", "family"))
w <- w[n_t >= MINC & n_u >= MINC & is.finite(ev_t) & is.finite(ev_u)]
stopifnot("no venue-family cell has enough rows in BOTH pools" = nrow(w) > 20)
cat(sprintf("venue-family cells with %d+ marks in BOTH pools: %d\n", MINC, nrow(w)))

# --- the comparison, corrected for measurement error --------------------------
# THE TRAP THIS AVOIDS. An OLS slope of eff_u on eff_t is attenuated by the noise
# in eff_t, so it comes out below 1 even when both pools measure exactly the same
# thing. On the first run it read 0.459, which looked like strong evidence of
# occasion contamination - until the algebra: cor * sd_u/sd_t = 0.501 * 0.917 =
# 0.459 exactly. That is the definition of the OLS slope, not a finding. Any
# "does A reproduce B" question between two NOISY estimates needs errors-in-
# variables, never OLS.
#
# So: subtract the known sampling variance from each pool's observed spread to
# get the TRUE spread, and compare those. If peaking inflates the targeted pool,
# its true spread is genuinely wider - noise cannot do that, because noise has
# been removed from both sides.
disatt <- function(d) {
  vt <- stats::var(d$eff_t); vu <- stats::var(d$eff_u)
  et <- mean(d$ev_t);        eu <- mean(d$ev_u)
  # covariance carries no sampling error: the two pools share no rows, so their
  # errors are independent and the covariance is already an unbiased estimate of
  # the true covariance. This is what makes the split usable at all.
  cv <- stats::cov(d$eff_t, d$eff_u)
  tt <- max(vt - et, 0); tu <- max(vu - eu, 0)
  data.table(
    cells    = nrow(d),
    obs_cor  = cor(d$eff_t, d$eff_u),
    rel_t    = if (vt > 0) tt / vt else NA_real_,   # signal share of each pool
    rel_u    = if (vu > 0) tu / vu else NA_real_,
    true_sd_t = sqrt(tt), true_sd_u = sqrt(tu),
    # ratio > 1 means targeted really is wider once noise is stripped from both
    infl     = if (tu > 0) sqrt(tt / tu) else NA_real_,
    # slope of the TRUE effects, free of attenuation
    true_slope = if (tt > 0) cv / tt else NA_real_,
    true_cor = if (tt > 0 && tu > 0) cv / sqrt(tt * tu) else NA_real_)
}
o <- disatt(w)
cat("\n=== does the tune-up pool reproduce the targeted pool? ===\n")
cat(sprintf("observed correlation             : %+.3f   (n = %d cells)\n",
            o$obs_cor, o$cells))
cat(sprintf("signal share of each estimate    : targeted %.2f | tune-up %.2f\n",
            o$rel_t, o$rel_u))
cat(sprintf("TRUE sd, noise removed           : targeted %.5f | tune-up %.5f\n",
            o$true_sd_t, o$true_sd_u))
cat(sprintf("inflation factor sd_t / sd_u     : %.3f\n", o$infl))
cat(sprintf("disattenuated slope and corr     : %.3f | %+.3f\n",
            o$true_slope, o$true_cor))
cat("\ninflation 1.00 => the effect is the PLACE. Athletes peaking at a venue and\n")
cat("  athletes passing through measure the same thin air and the same track,\n")
cat("  and the deployed number is clean.\n")
cat("inflation >1    => targeted rows carry peaking on top of the place, so the\n")
cat("  deployed number is too big by that factor - fixable by estimating from\n")
cat("  tune-up rows alone, which is a filter, not a new model.\n")
cat("A true correlation below 1 with inflation at 1 means something else again:\n")
cat("  a real athlete-by-venue interaction that no single venue number can hold.\n")

cat("\n=== by family ===\n")
fam <- w[, disatt(.SD), by = family][order(-cells)]
print(fam[, .(family, cells, obs_cor = round(obs_cor, 3),
              true_sd_t = round(true_sd_t, 5), true_sd_u = round(true_sd_u, 5),
              infl = round(infl, 3), true_cor = round(true_cor, 3))])

# --- the external anchor, which is what actually decides it -------------------
# Everything above is internal: the two pools are compared only with each other,
# so a common bias would be invisible. Altitude is outside evidence and its sign
# is not negotiable - thin air means less drag, so sprints and jumps get FASTER,
# and less oxygen, so distance running gets SLOWER. If the tune-up pool is the
# truer measure of the PLACE, it should track elevation at least as well as the
# targeted pool. If it tracks it WORSE, the tune-up pool is just noisier and
# "cleaner" was the wrong word for it.
#
# Same hand-checked reference set as check_venue_effect.R, used only to validate,
# never to build.
# Elevations come from the shared loader, which prefers the 3,101 geocoded
# venues and falls back LOUDLY to the hand-typed 43. Previously this file
# carried its own copy of those 43, which is why the geocoded table sat
# unread after it was built.
source(here::here("citiusdata", "scripts", "_venue_elevation.R"))
ALT <- venue_elevation()[, .(venue_city, alt_m)]
z <- merge(w, ALT, by = "venue_city")
cat(sprintf("\n=== the external anchor: elevation (%d of %d cells matched) ===\n",
            nrow(z), nrow(w)))
if (nrow(z) >= 20) {
  # sprints and jumps go one way, distance the other, so they must be read apart
  z[, grp := fifelse(family %chin% c("sprint", "jump", "hurdles"), "sprint/jump",
             fifelse(family %chin% c("distance", "middle", "road"), "distance", NA_character_))]
  ea <- z[!is.na(grp), .(cells = .N,
                         cor_targeted = round(stats::cor(eff_t, alt_m), 3),
                         cor_tuneup   = round(stats::cor(eff_u, alt_m), 3)), by = grp]
  print(ea)
  cat("\nExpected signs: sprint/jump POSITIVE with elevation (thin air, less\n")
  cat("drag, faster) and distance NEGATIVE (less oxygen, slower). Whichever pool\n")
  cat("tracks elevation better is the better measure of the place itself.\n")
} else {
  cat("too few matched cells to anchor on elevation - reporting nothing\n")
}

# --- the same anchor on the DEPLOYED estimate, not the paired subset ----------
# The comparison above needed cells present in both pools, which threw away most
# venues. The question that matters for deployment is different: if venue_adj
# were estimated from tune-up rows ALONE, would the shipped number track altitude
# better than today's? That needs no pairing, so every venue meeting the normal
# threshold is available and the anchor gets several times the sample.
cat("\n=== if the DEPLOYED venue_adj were built from one pool only ===\n")
shrunk <- function(x) {
  e <- venue_effect(x)
  e[, .(venue_city, family, n_v, eff = eff * n_v / (n_v + VKAP))]
}
arms <- list(deployed = shrunk(v),
             targeted = shrunk(v[pool == "targeted"]),
             tuneup   = shrunk(v[pool == "tuneup"]))
anch <- rbindlist(lapply(names(arms), function(nm) {
  e <- merge(arms[[nm]], ALT, by = "venue_city")
  e[, grp := fifelse(family %chin% c("sprint", "jump", "hurdles"), "sprint/jump",
             fifelse(family %chin% c("distance", "middle", "road"), "distance",
                     NA_character_))]
  e <- e[!is.na(grp) & n_v >= MINC]
  # Spearman as well as Pearson: elevation is bimodal - a sea-level cluster and
  # an altitude cluster with almost nothing between - so a Pearson correlation
  # can be carried by two or three extreme cities. If the rank version does not
  # agree, the effect is those cities and not a gradient.
  e[, .(arm = nm, cells = .N,
        pearson  = round(stats::cor(eff, alt_m), 3),
        spearman = round(stats::cor(eff, alt_m, method = "spearman"), 3),
        hi_alt   = sum(alt_m >= 1000)), by = grp]
}))
stopifnot("the deployed anchor produced nothing" = nrow(anch) > 0)
print(dcast(anch, grp ~ arm, value.var = c("cells", "hi_alt", "pearson", "spearman")))
cat("\nsprint/jump should be POSITIVE and distance NEGATIVE. The arm that anchors\n")
cat("best on outside evidence is the one to ship, regardless of what the\n")
cat("concordance metric says - that metric cannot resolve effects this size.\n")

cat("\n=== the venues where the two pools disagree most ===\n")
w[, gap := eff_t - eff_u]
setorder(w, -gap)
show <- rbind(head(w, 6), tail(w, 6))
print(show[, .(venue_city, family, n_t, n_u, targeted = round(eff_t, 4),
               tuneup = round(eff_u, 4), gap = round(gap, 4))])
cat("\nA positive gap means the venue looks FASTER to athletes peaking there\n")
cat("than to athletes passing through - which is occasion, not geography.\n")

f <- file.path(D, "venue_occasion.json")
writeLines(jsonlite::toJSON(list(overall = o, by_family = fam, by_venue = w),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s (%d cells, %d families)\n",
            basename(f), nrow(w), nrow(fam)))
