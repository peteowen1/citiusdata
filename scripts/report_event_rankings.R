# The published top ten for every event, with the column the table is SORTED BY
# shown next to the athlete's typical mark and their actual bests.
#
# WHY THE EXTRA COLUMN. The table ranks on R_rank - the ceiling-blended rating
# plus the cross-event and combined-event blends - while `typical` comes from raw
# R. Those are different numbers, so a page that shows only `typical` reads as
# mis-sorted: the 1500m had Wanyonyi 3:32.2 first and Ingebrigtsen 3:31.3 third
# with nothing to explain it. `ranked_on` is derived from the sort key itself, so
# it always moves with the rank, and form_display_marks.R asserts that.
#
# SB and PB come from the corpus rather than the model, so they are an outside
# reference rather than another view of the same rating.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D    <- here::here("citiusdata", "data")
TOPN <- as.integer(Sys.getenv("TOPN", "10"))

d <- setDT(read_parquet(file.path(D, "form_display_final.parquet")))
stopifnot("display table has no rank_mark - re-run form_display_marks.R" =
            "rank_mark" %in% names(d))
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family, orientation)]
# An inner join here drops any display row whose event_id drifted out of the
# registry, silently and partially - the emptiness check further down cannot see
# a partial loss. Count it.
.n_before <- nrow(d)
d <- merge(d, reg, by = "event_id")
if (nrow(d) < .n_before)
  stop(sprintf("%d of %d display rows have no registry entry - the event registry
and the display table disagree", .n_before - nrow(d), .n_before))
d[, athlete_id := as.character(athlete_id)]
ASOF <- max(d$last, na.rm = TRUE)

# season best and personal best, from the corpus
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id", "event_id", "mark", "date", "scoreable", "legal")))
c0[, athlete_id := as.character(athlete_id)]
# LEGAL marks only. Amusan's 12.06 (+2.5 wind, World Championship final) is a
# real run but is not her personal best, and showing it as one is wrong on a page
# that also prints the world record elsewhere.
# IMPOSSIBLE MARKS. A legality filter is not enough: Noah Lyles carries an 18.90
# for 200m dated 2020-07-09 with a 3.7 m/s HEADWIND, flagged legal = TRUE, no
# competition and no venue - the annulled Inspiration Games run where he started
# ~15m early. It beats the world record of 19.19, which is the only test that
# catches it. The engine drops these too; this repeats the check because SB and
# PB are read from the corpus directly, not from the model.
.wrf <- file.path(D, "world_records.csv")
if (file.exists(.wrf)) {
  .w <- setDT(utils::read.csv(.wrf, stringsAsFactors = FALSE))
  .w[, wr_mark := vapply(strsplit(as.character(mark), ":", fixed = TRUE), function(q) {
       v <- suppressWarnings(as.numeric(q))
       if (anyNA(v)) NA_real_ else Reduce(function(a, b) a * 60 + b, v)
     }, numeric(1))]
  .w <- merge(.w[is.finite(wr_mark), .(event_id, wr_mark)],
              reg[, .(event_id, orientation)], by = "event_id")
  # carry orientation from the record table under its own name: `c0` has not
  # been joined to the registry yet at this point, and reusing `orientation`
  # would collide with that later merge
  c0 <- merge(c0, .w[, .(event_id, wr_mark, wr_or = orientation)],
              by = "event_id", all.x = TRUE)
  .imp <- c0[is.finite(wr_mark) & fifelse(wr_or == -1, mark < wr_mark, mark > wr_mark)]
  if (nrow(.imp)) {
    cat(sprintf("dropping %d mark(s) better than their world record:\n", nrow(.imp)))
    print(.imp[, .(event_id, mark, wr_mark, date)])
    c0 <- c0[!(is.finite(wr_mark) & fifelse(wr_or == -1, mark < wr_mark, mark > wr_mark))]
  }
  c0[, c("wr_mark", "wr_or") := NULL]
}
n_all <- nrow(c0[is.finite(mark) & mark > 0 & scoreable == TRUE])
c0 <- c0[is.finite(mark) & mark > 0 & scoreable == TRUE & (is.na(legal) | legal == TRUE)]
cat(sprintf("SB/PB from legal marks only: %s of %s scoreable rows kept\n",
            format(nrow(c0), big.mark = ","), format(n_all, big.mark = ",")))
c0 <- merge(c0, reg[, .(event_id, orientation)], by = "event_id")
bestof <- function(x, o) if (o[1] == -1) min(x) else max(x)
pb <- c0[, .(pb = bestof(mark, orientation)), by = .(athlete_id, event_id)]
sb <- c0[date >= ASOF - 365, .(sb = bestof(mark, orientation)), by = .(athlete_id, event_id)]
d <- merge(d, pb, by = c("athlete_id", "event_id"), all.x = TRUE)
d <- merge(d, sb, by = c("athlete_id", "event_id"), all.x = TRUE)

wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]
wa[, sex := fifelse(grepl("^Men", event_group), "M", "W")]
wa[, disc := gsub(",", "", sub("^(Men|Women)'s ", "", event_group))]
wa[, disc := fcase(
  disc == "110mH", "110MetresHurdles", disc == "100mH", "100MetresHurdles",
  disc == "400mH", "400MetresHurdles", disc == "3000mSC", "3000MetresSteeplechase",
  disc == "20km Race Walking", "20KilometresRaceWalk",
  disc == "35km Race Walking", "35KilometresRaceWalk",
  grepl("^[0-9]+m$", disc), paste0(sub("m$", "", disc), "Metres"),
  default = gsub(" ", "", disc))]
wa[, event_id := paste0("AT-", disc, "-", sex)]
d <- merge(d, wa[, .(event_id, athlete_id = as.character(athlete_id), wa = wa_place)],
           by = c("event_id", "athlete_id"), all.x = TRUE)
cat(sprintf("display rows %s | events %d | WA covers %d events | as at %s\n",
            format(nrow(d), big.mark = ","), uniqueN(d$event_id),
            uniqueN(wa[event_id %chin% d$event_id, event_id]), ASOF))

# --- ordering the events the way a reader thinks about them ------------------
# Sorting alphabetically puts the 10,000m before the 100m and the Mile between
# the Marathon and the Pole Vault. A distance lets the page offer the ordering
# an athletics reader actually wants; field events have none, so they fall back
# to their name within their group.
dist_of <- function(disc) {
  # No regex backreference here on purpose: the first version used sub() with a
  # backreference and parsed only 2 of 40 track events, because the escape did
  # not survive being written into the file. regmatches needs no escaping.
  d <- tolower(gsub(",", "", disc))
  m <- regexpr("[0-9]+([.][0-9]+)?", d)
  n <- rep(NA_real_, length(d))
  hit <- m > 0
  n[hit] <- suppressWarnings(as.numeric(regmatches(d, m)))
  km <- grepl("kilometre|kilometer|km", d)
  out <- rep(NA_real_, length(d))
  out[grepl("metre|meter", d) & !km] <- n[grepl("metre|meter", d) & !km]
  out[km] <- n[km] * 1000
  out[grepl("mile", d)] <- 1609
  out[grepl("half marathon", d)] <- 21097
  out[grepl("marathon", d) & !grepl("half", d)] <- 42195
  out
}
d[, dist_m := dist_of(discipline)]
d[, grp := fcase(family %chin% c("sprint", "hurdles", "middle", "distance"), "track",
                 family %chin% c("jump", "throw"), "field",
                 family %chin% c("road", "walk"), "road",
                 default = "combined")]
evs <- unique(d[, .(event_id, discipline, grp, dist_m)])
cat("
events by group, and how many carry a distance:
")
print(evs[, .(events = .N, with_distance = sum(is.finite(dist_m))), by = grp][order(grp)])
# A guard that passes on 2 of 40 is not a guard. Track and road events all have
# a distance by definition, so require nearly all of them.
for (g in c("track", "road")) {
  got <- evs[grp == g, mean(is.finite(dist_m))]
  if (is.finite(got) && got < 0.9) {
    print(evs[grp == g & !is.finite(dist_m), .(discipline)])
    stop(sprintf("only %.0f%% of %s events parsed a distance - the parser is broken",
                 100 * got, g))
  }
}
cat(sprintf("distance parsed for %d of %d track and road events
",
            evs[grp %chin% c("track", "road"), sum(is.finite(dist_m))],
            evs[grp %chin% c("track", "road"), .N]))

fmt <- function(m, unit) {
  ifelse(is.na(m), NA_character_,
  # `unit` is "seconds"/"metres"/"points". Testing against "s" meant no time was
  # ever formatted: the 10,000m read 1625.07 rather than 27:05.07.
  ifelse(!(unit %chin% c("s", "seconds")), sprintf("%.2f", m),
  ifelse(m < 60, sprintf("%.2f", m),
  ifelse(m < 3600, sprintf("%d:%05.2f", floor(m / 60), m %% 60),
         sprintf("%d:%02d:%02.0f", floor(m / 3600), floor((m %% 3600) / 60), m %% 60)))))
}
top <- d[rk <= TOPN]
setorder(top, family, discipline, sex, rk)
# WHERE THE RATING COMES FROM. n_eff on its own cannot distinguish a rank built
# on an athlete's own racing from one inferred through correlated events, and for
# a thin record those are very different claims. own_races is this event's
# evidence; borrowed_pct is the share of the ranking key taken from elsewhere.
for (cc in c("xb_share", "xb_sibs", "ce_share"))
  if (!cc %chin% names(top)) top[, (cc) := 0]
out <- top[, .(event_id, discipline, sex, family, grp, dist_m, rk,
               athlete = athlete_name,
               ranked_on = fmt(rank_mark, unit),
               typical   = fmt(pred_mark, unit),
               sb = fmt(sb, unit), pb = fmt(pb, unit),
               own_races = round(n_eff, 1),
               borrowed_pct = round(100 * xb_share),
               sibs = as.integer(xb_sibs),
               sim_pct = round(100 * ce_share),
               wa, last, unit)]
stopifnot("no rows produced" = nrow(out) > 0,
          "ranked_on is missing on some rows" = !anyNA(out$ranked_on))

f <- file.path(D, "event_rankings_report.json")
writeLines(jsonlite::toJSON(out, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("wrote %s: %s rows over %d events\n", basename(f),
            format(nrow(out), big.mark = ","), uniqueN(out$event_id)))

for (EV in c("AT-1500Metres-M", "AT-10000Metres-M", "AT-100Metres-W")) {
  x <- out[event_id == EV]
  if (!nrow(x)) next
  cat(sprintf("\n== %s ==\n", sub("^AT-", "", EV)))
  print(x[, .(rk, athlete = substr(athlete, 1, 22), ranked_on, typical, sb, pb,
              own_races, borrowed_pct, wa)])
}
