# TARGETED OVERRIDE of six event_params.rds rows, not a re-run of the fit.
#
# fit_event_params.R's own hierarchy leaves half_life and races_half_life at
# values these six events cannot use well -- most visibly AT-PoleVault-W,
# whose family (jump) shrinkage pulls it toward races_half_life=11.05 while
# its own held-out data wants 2. A joint half_life x races_half_life sweep,
# holding context_scale/trim_tactical at each event's own already-fitted
# value (not the flat global marks_hier_params.R uses), found decisively
# better values for all six -- see docs/HANDOVER-2026-09-08.md, 2026-09-08.
#
# THIS IS A PATCH, NOT A RE-FIT. The proper fix is either a finite
# kappa_event for races_half_life (marks_hier_params.R showed that alone does
# not move the aggregate, most likely because it was tested with the other
# three parameters held at the flat global rather than each event's own fit)
# or the event-sex hierarchy Pete proposed (global -> family -> discipline
# M+W pooled -> event). Until one of those replaces this file's provenance,
# these six rows are hand-set and must be reapplied after any re-run of
# fit_event_params.R.
#
# Usage: powershell -Command 'Rscript citiusdata/scripts/apply_event_param_overrides.R'
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
f <- file.path(OUT, "event_params.rds")
ep <- readRDS(f)

OVERRIDES <- list(
  "AT-PoleVault-W"             = list(half_life = 365, races_half_life = 2),
  "AT-LongJump-W"              = list(half_life = 180, races_half_life = Inf),
  "AT-HighJump-M"              = list(half_life = 270, races_half_life = Inf),
  "AT-HighJump-W"              = list(half_life = 270, races_half_life = 12),
  "AT-35KilometresRaceWalk-M"  = list(half_life = 730, races_half_life = Inf),
  "AT-600Metres-W"             = list(half_life = 270, races_half_life = Inf))

for (ev in names(OVERRIDES)) {
  stopifnot("event missing from event_params.rds" = ev %in% ep$event_id)
  ep[event_id == ev, `:=`(half_life = OVERRIDES[[ev]]$half_life,
                           races_half_life = OVERRIDES[[ev]]$races_half_life)]
}

attr(ep, "stamp") <- paste0(attr(ep, "stamp"),
  " | + hand overrides 2026-09-08 (half_life/races_half_life): ",
  paste(names(OVERRIDES), collapse = ", "))
saveRDS(ep, f)
fwrite(ep, file.path(OUT, "event_params.csv"))
cat("applied", length(OVERRIDES), "overrides to event_params.rds\n")
print(ep[event_id %in% names(OVERRIDES), .(event_id, family, context_scale, trim_tactical, half_life, races_half_life)])
