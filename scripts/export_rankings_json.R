# Top 10 per event, as JSON for the model explainer page.
#
# Formatting happens HERE, not in the browser: the seconds/metres/points
# distinction and the m:ss.xx convention are properties of the event registry,
# and a second implementation in JavaScript is a second thing to get wrong. The
# page receives display strings plus the raw values it needs for sorting.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table)); library(jsonlite)

D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
TOPN <- as.integer(Sys.getenv("RANK_TOP_N", "10"))
d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family, unit)]
d <- merge(d, reg, by = "event_id", all.x = TRUE, suffixes = c("", ".reg"))
d <- d[!is.na(discipline) & is.finite(pred_mark)]
d[, athlete_id := as.character(athlete_id)]
# 6.1% of ranked athletes had no name in the display table. The competition
# cache carries athlete_name on every result row and covers 30 of the 36; the
# rest are shown as their id rather than blank, so a reader can at least tell
# one anonymous athlete from another and look the id up.
lk <- file.path(D, "athlete_name_lookup.rds")
if (file.exists(lk)) {
  nml <- readRDS(lk); nml[, athlete_id := as.character(athlete_id)]
  d <- merge(d, nml[, .(athlete_id, nm2 = athlete_name)], by = "athlete_id", all.x = TRUE)
  d[(is.na(athlete_name) | !nzchar(athlete_name)) & !is.na(nm2), athlete_name := nm2]
}
d[is.na(athlete_name) | !nzchar(athlete_name), athlete_name := paste0("Athlete ", athlete_id)]

# --- track vs field ----------------------------------------------------------
# Combined events sit with field by convention: they are scored in points and a
# reader looking for "field" expects to find the decathlon there.
# Three groups, not two. Putting the half marathon under "track" because it is
# a running event is the kind of tidy-looking wrongness a reader spots at once.
d[, grp := fifelse(family %chin% c("sprint","middle","distance","hurdles"), "track",
            fifelse(family %chin% c("road","walk"), "road", "field"))]

# --- a distance to sort by ---------------------------------------------------
# Parsed from the discipline name, which carries it explicitly ("800 Metres",
# "20 Kilometres Race Walk"). Field events have no distance, so they fall back
# to alphabetical within their group rather than being forced onto a scale that
# does not apply to them.
d[, dist_num := as.numeric(gsub(",", "", sub("^([0-9,.]+).*$", "\\1", discipline)))]
d[!grepl("^[0-9]", discipline), dist_num := NA_real_]
d[grepl("Kilometre|Mile", discipline) & is.finite(dist_num), dist_num := dist_num * 1000]
d[grepl("Mile", discipline) & is.finite(dist_num), dist_num := dist_num * 1.609]

fmt <- function(mark, unit) {
  ifelse(is.na(mark), NA_character_,
    ifelse(unit == "seconds",
      ifelse(mark >= 60,
             sprintf("%d:%05.2f", floor(mark / 60), mark %% 60),
             sprintf("%.2f", mark)),
      ifelse(unit == "points", format(round(mark), big.mark = ","),
             sprintf("%.2f", mark))))
}
# An event with a handful of ranked athletes cannot support a "top 10", and
# printing one implies a depth of competition that is not there. Dropped rather
# than shown with a caveat: the page is a ranking, and a ranking of three people
# is not a ranking. 11 events fall out, all minor (2000m steeplechase W, 5km
# race walk M, half marathon M each had 1-6).
MINA <- as.integer(Sys.getenv("RANK_MIN_ATHLETES", "10"))
depth <- d[, .(n_ath = .N), by = event_id]
drop <- depth[n_ath < MINA]
if (nrow(drop)) cat(sprintf("dropping %d events with fewer than %d ranked athletes
",
                            nrow(drop), MINA))
d <- d[event_id %chin% depth[n_ath >= MINA, event_id]]
setorder(d, event_id, rk)
top <- d[rk <= TOPN]
top[, `:=`(typical_s = fmt(pred_mark, unit), good_s = fmt(peak_mark, unit))]

ev <- unique(d[, .(event_id, discipline, sex, family, grp, unit, dist_num)])
# Coverage flag. Combined events are the worst case: the men's decathlon is
# topped by 8,172 when world class is 8,500+, because the T1/T2 filter leaves
# the actual decathletes largely outside the corpus. Better to say so on the
# page than to let a reader assume the list is the world order.
lead <- d[rk == 1, .(event_id, lead_n = n_eff)]
ev <- merge(ev, lead, by = "event_id", all.x = TRUE)
ev[, thin := is.finite(lead_n) & lead_n < 6]
setorder(ev, grp, dist_num, discipline)
out <- lapply(seq_len(nrow(ev)), function(i) {
  e <- ev[i]
  a <- top[event_id == e$event_id]
  list(id = e$event_id, name = e$discipline, sex = e$sex, grp = e$grp,
       family = e$family, unit = e$unit, thin = unname(e$thin),
       dist = if (is.finite(e$dist_num)) e$dist_num else NULL,
       athletes = lapply(seq_len(nrow(a)), function(j) list(
         rk = a$rk[j], name = a$athlete_name[j],
         typical = a$typical_s[j],
         # NULL, not a number, where the good-day mark is suppressed: the page
         # must show an explicit dash rather than imply a missing value is zero
         good = if (is.na(a$good_s[j])) NULL else a$good_s[j],
         n = round(a$n_eff[j], 1))))
})
f <- file.path(D, "form_rankings.json")
write_json(out, f, auto_unbox = TRUE, null = "null", pretty = FALSE)
cat(sprintf("wrote %s\n  %d events (%d track, %d field), %s athlete rows, top %d each\n",
    f, nrow(ev), ev[grp == "track", .N], ev[grp == "field", .N],
    format(nrow(top), big.mark = ","), TOPN))
cat(sprintf("  good-day marks shown on %d of %d rows (suppressed below n_eff 8)\n",
    sum(!is.na(top$good_s)), nrow(top)))
cat(sprintf("  ratings as at %s\n", max(d$last, na.rm = TRUE)))
