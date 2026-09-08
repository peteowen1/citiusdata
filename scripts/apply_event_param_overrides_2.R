# TARGETED OVERRIDE, round 2 -- post TIER_W=1 refit (2026-09-08).
#
# The TIER_W fix (fit_event_params.R) made four of the six events from the
# first override round better than that patch had, from the real fit alone.
# It also regressed the two ultra-distance walks. A joint half_life x
# races_half_life sweep (holding context_scale/trim_tactical at each event's
# own fit) was run for the three still-marginal events -- Pole Vault W and
# both 35km walks. Only two of three transferred to held-out:
#
#   AT-PoleVault-W             7.50% -> 3.97% held gap, real improvement.
#   AT-35KilometresRaceWalk-M 14.40% -> 11.49% held gap. STILL a confirmed
#                              loss (LAST-5 BETTER). Improved, not fixed --
#                              likely a sample-size ceiling (16 held-out
#                              races) rather than a parameter this pair can
#                              reach. Do not chase this one further with a
#                              grid search; it needs either more data or a
#                              different lever.
#   AT-35KilometresRaceWalk-W deliberately NOT overridden. Its fit-year
#                              "best" (14.51% -> nominally better) made
#                              held-out WORSE (14.51% -> 15.44%) -- the
#                              margin over the runner-up was 0.017pp,
#                              indistinguishable from noise, and it did not
#                              generalise. Left at the production fit's
#                              value rather than applying a change that
#                              measurably regressed the number it exists to
#                              improve.
#
# Run apply_event_param_overrides.R's superseded six FIRST if for some reason
# reproducing that history, but this round assumes a clean post-TIER_W=1
# event_params.rds with none of the old six applied.
#
# Usage: powershell -Command 'Rscript citiusdata/scripts/apply_event_param_overrides_2.R'
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
f <- file.path(OUT, "event_params.rds")
ep <- readRDS(f)

OVERRIDES <- list(
  "AT-PoleVault-W"             = list(half_life = 540, races_half_life = 2),
  "AT-35KilometresRaceWalk-M"  = list(half_life = 730, races_half_life = Inf))

for (ev in names(OVERRIDES)) {
  stopifnot("event missing from event_params.rds" = ev %in% ep$event_id)
  ep[event_id == ev, `:=`(half_life = OVERRIDES[[ev]]$half_life,
                           races_half_life = OVERRIDES[[ev]]$races_half_life)]
}

attr(ep, "stamp") <- paste0(attr(ep, "stamp"),
  " | + hand overrides round 2, 2026-09-08 (half_life/races_half_life): ",
  paste(names(OVERRIDES), collapse = ", "))
saveRDS(ep, f)
fwrite(ep, file.path(OUT, "event_params.csv"))
cat("applied", length(OVERRIDES), "overrides to event_params.rds\n")
print(ep[event_id %in% names(OVERRIDES), .(event_id, family, context_scale, trim_tactical, half_life, races_half_life)])
