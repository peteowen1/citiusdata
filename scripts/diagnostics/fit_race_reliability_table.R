# Measure the reliability of the fitted race effect PER EVENT, and write it as
# a table the EB shrinkage can consume.
#
# WHY PER EVENT. A single global reliability (0.647) improves the average but
# scatters the individual cases: applied uniformly it moved shot put 0.349 ->
# 0.649 (good) while pushing long jump 0.704 -> 1.193 (overshot past correct).
# Mean |slope - 1| improved 0.392 -> 0.248 but the SPREAD across events got
# worse, 0.141 -> 0.201. Events genuinely differ in how well a shared race
# effect can be identified -- small fields and idiosyncratic events like the
# throws are measured far more noisily than the sprints.
#
# THE MEASUREMENT. Regress what a field ACTUALLY averaged in LATER races of the
# same event on the fitted c_r. Under classical regression dilution that slope
# IS the reliability tau^2/(tau^2+se^2), which is the same quantity as the EB
# shrinkage weight -- so it can be fed straight back in.
#
# Events with too few races to fit a stable slope fall back to their FAMILY
# median rather than to a global constant, because the family is the level at
# which these clearly differ.
#
# Usage:  Rscript citiusdata/scripts/diagnostics/fit_race_reliability_table.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")
SRC <- Sys.getenv("CITIUS_REL_SRC", "calibration_corpus_wac_coast_0904.rds")
MINF <- as.integer(Sys.getenv("CITIUS_REL_MINFIELD", "6"))
MINRACES <- as.integer(Sys.getenv("CITIUS_REL_MINRACES", "120"))
CAP <- as.integer(Sys.getenv("CITIUS_REL_CAP", "900"))
DEST <- Sys.getenv("CITIUS_REL_OUT", "race_reliability_by_event.csv")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

cal <- readRDS(file.path(OUT, SRC)); rr <- as.data.table(cal$race)
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
reg <- as.data.table(citius_events())[, .(event_id, family, orientation)]
evs <- reg[event_id %chin% unique(rr$event_id)]$event_id
say(sprintf("%d events with fitted race effects", length(evs)))

res <- rbindlist(lapply(evs, function(EV) {
  ORI <- reg[event_id == EV]$orientation[1]
  d <- ch[event_id == EV & is.finite(mark) & !is.na(race_key) & !is.na(date),
          .(athlete_id, race_key, date, mark)]
  if (nrow(d) < 500) return(NULL)
  d[, perf := ORI * log(mark)]
  races <- rr[event_id == EV & n_in_race >= MINF, .(race_key, c_r)]
  if (nrow(races) < 30) return(NULL)
  set.seed(11L); if (nrow(races) > CAP) races <- races[sample(.N, CAP)]
  fld <- merge(d, races, by = "race_key")
  agg <- rbindlist(lapply(split(fld, fld$race_key), function(g) {
    lat <- d[athlete_id %chin% g$athlete_id & date > g$date[1], .(m = mean(perf)), by = athlete_id]
    if (!nrow(lat)) return(NULL)
    gg <- merge(g[, .(athlete_id, perf)], lat, by = "athlete_id")
    if (!nrow(gg)) return(NULL)
    data.table(c_r = g$c_r[1], observed = mean(gg$perf) - mean(gg$m))
  }))
  if (is.null(agg) || nrow(agg) < 40) return(NULL)
  fit <- stats::lm(observed ~ c_r, data = agg)
  s <- summary(fit)
  data.table(event_id = EV, family = reg[event_id == EV]$family[1],
             races = nrow(agg), slope = unname(coef(fit)[2]),
             se = unname(s$coefficients[2, 2]), r2 = s$r.squared)
}))
say(sprintf("fitted a slope for %d events", nrow(res)))

# Reliability is a shrinkage WEIGHT, so it must sit in (0, 1]. A slope above 1
# means the effect is UNDER-sized for that event and no shrinkage is wanted;
# a negative or near-zero slope means it carries no usable signal.
res[, reliability := pmin(pmax(slope, 0.05), 1)]
res[races < MINRACES, reliability := NA_real_]
fam <- res[!is.na(reliability), .(fam_rel = median(reliability)), by = family]
res <- merge(res, fam, by = "family", all.x = TRUE)
res[is.na(reliability), reliability := fam_rel]
res[is.na(reliability), reliability := median(res$reliability, na.rm = TRUE)]

setorder(res, reliability)
print(res[, .(event_id, family, races, slope = round(slope, 3),
              r2 = round(r2, 3), reliability = round(reliability, 3))])
cat("\nby family:\n")
print(res[, .(events = .N, median_reliability = round(median(reliability), 3)),
          by = family][order(median_reliability)])
fwrite(res[, .(event_id, family, races, slope, se, r2, reliability)], file.path(OUT, DEST))
say(sprintf("wrote %s", DEST))
