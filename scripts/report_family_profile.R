# Every adjustment and every engine change, broken down by FAMILY - and what each
# one says about how that kind of event behaves.
#
# The point is not a scorecard. Each number here is a measurement of the sport:
# how much wind is worth in a 110m hurdles, how much altitude costs a
# steeplechaser, whether winning slowly should count against a middle-distance
# runner. Read together they describe what actually determines a result in each
# family, which is more interesting than any single concordance figure.
#
# Everything is read from stored artefacts rather than retyped, so the page and
# the model cannot drift apart.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

am <- setDT(read_parquet(file.path(D, "adjusted_marks.parquet"),
                         col_select = c("athlete_id","event_id","family","unit","mark",
                                        "adj_mark","adj_delta","wind_adj","venue_adj","date")))
am[, athlete_id := as.character(athlete_id)]
cat(sprintf("adjusted performances: %s\n", format(nrow(am), big.mark = ",")))

# --- how big is each correction, in the units of the family --------------------
mag <- am[, .(
  performances  = .N,
  unit          = unit[1],
  median_mark   = round(stats::median(mark), 2),
  wind_share    = round(100 * mean(wind_adj != 0), 1),
  wind_median   = round(stats::median(abs(mark * (exp(-wind_adj) - 1))[wind_adj != 0]), 3),
  wind_p95      = round(stats::quantile(abs(mark * (exp(-wind_adj) - 1))[wind_adj != 0], .95), 3),
  venue_median  = round(stats::median(abs(mark * (exp(-venue_adj) - 1))[venue_adj != 0]), 3),
  venue_p95     = round(stats::quantile(abs(mark * (exp(-venue_adj) - 1))[venue_adj != 0], .95), 3)
), by = family]
mag[is.na(wind_median), `:=`(wind_median = 0, wind_p95 = 0, wind_share = 0)]

# --- does correcting make an athlete look more consistent? ---------------------
am[, n_ath := .N, by = .(athlete_id, event_id)]
sc <- am[n_ath >= 4, .(sd_raw = stats::sd(log(mark)), sd_adj = stats::sd(log(adj_mark))),
         by = .(athlete_id, event_id, family)]
sc <- sc[is.finite(sd_raw) & is.finite(sd_adj)]
scat <- sc[, .(athlete_events = .N,
               scatter_pct = round(100 * (mean(sd_adj) / mean(sd_raw) - 1), 2),
               improved_pct = round(100 * mean(sd_adj < sd_raw), 1)), by = family]

# --- what each engine change was worth, per family ----------------------------
read_fam <- function(f, lab) {
  p <- file.path(D, f)
  if (!file.exists(p)) { cat(sprintf("  missing %s\n", f)); return(NULL) }
  # simplifyDataFrame = TRUE so by_family comes back as a data.frame rather than
  # a list of one-element lists, which is what silently produced "object 'pooled'
  # not found" the first time.
  j <- jsonlite::fromJSON(p, simplifyDataFrame = TRUE)
  bf <- rbindlist(lapply(seq_along(j$by_family), function(i) as.data.table(j$by_family[[i]])),
                  fill = TRUE)
  stopifnot("by_family came back empty or without a pooled column" =
              nrow(bf) > 0 && "pooled" %chin% names(bf))
  bf[, .(family, change = lab, pooled, up, down, events)]
}
eng <- rbindlist(list(
  read_fam("fam_adjusted.json", "Adjusted marks"),
  read_fam("fam_censwin.json",  "Winner censoring"),
  read_fam("fam_shock.json",    "Race-shock weighting")), fill = TRUE)
stopifnot("no engine-change tables loaded" = !is.null(eng) && nrow(eng) > 0)
cat(sprintf("engine changes loaded: %s\n", paste(unique(eng$change), collapse = ", ")))
engw <- dcast(eng, family ~ change, value.var = "pooled")

prof <- Reduce(function(a, b) merge(a, b, by = "family", all = TRUE),
               list(mag, scat, engw))
setorder(prof, -performances)
cat("\n=== the per-family profile ===\n")
print(prof[, .(family, performances, unit, wind_median, venue_median,
               scatter_pct, improved_pct)])
cat("\n=== what each engine change was worth (concordance pp) ===\n")
print(engw)

# --- the wind curve, per event -------------------------------------------------
wf <- file.path(D, "wind_effect_curves.json")
stopifnot("wind curves missing" = file.exists(wf))
wm <- as.data.table(jsonlite::fromJSON(wf)$marginal)
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
wm <- merge(wm, reg, by = "event_id")
taper <- wm[wind %in% c(-2, 0, 4), .(event_id, discipline, sex, family, wind,
                                     per_ms_mark = round(per_ms_mark, 3), edf, n)]
tw <- dcast(taper, event_id + discipline + sex + family + edf + n ~ wind,
            value.var = "per_ms_mark")
setnames(tw, c("-2","0","4"), c("at_head2","at_zero","at_tail4"), skip_absent = TRUE)
tw[, taper_ratio := round(abs(at_head2) / pmax(abs(at_tail4), 1e-9), 1)]
cat("\n=== wind, per event: what +1 m/s buys, and how fast it fades ===\n")
print(tw[order(family, discipline)][, .(discipline, sex, at_head2, at_zero,
                                        at_tail4, taper_ratio, edf)])

f <- file.path(D, "family_profile.json")
writeLines(jsonlite::toJSON(list(profile = prof, engine = eng, wind = tw),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s (%d families, %d engine rows, %d wind events)\n",
            basename(f), nrow(prof), nrow(eng), nrow(tw)))
