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
d <- merge(d, reg, by = "event_id")
d[, athlete_id := as.character(athlete_id)]
ASOF <- max(d$last, na.rm = TRUE)

# season best and personal best, from the corpus
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id", "event_id", "mark", "date", "scoreable")))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[is.finite(mark) & mark > 0 & scoreable == TRUE]
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
  ifelse(unit != "s", sprintf("%.2f", m),
  ifelse(m < 60, sprintf("%.2f", m),
  ifelse(m < 3600, sprintf("%d:%05.2f", floor(m / 60), m %% 60),
         sprintf("%d:%02d:%02.0f", floor(m / 3600), floor((m %% 3600) / 60), m %% 60)))))
}
top <- d[rk <= TOPN]
setorder(top, family, discipline, sex, rk)
out <- top[, .(event_id, discipline, sex, family, grp, dist_m, rk,
               athlete = athlete_name,
               ranked_on = fmt(rank_mark, unit),
               typical   = fmt(pred_mark, unit),
               sb = fmt(sb, unit), pb = fmt(pb, unit),
               n_eff = round(n_eff, 1), wa, last, unit)]
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
  print(x[, .(rk, athlete = substr(athlete, 1, 22), ranked_on, typical, sb, pb, n_eff, wa)])
}
