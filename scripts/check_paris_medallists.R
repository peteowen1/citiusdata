# Every Paris 2024 medallist, and whether the model ranks them.
#
# THE TARGET. Pete's standard: 100%. This file defines what that means precisely,
# because the first three attempts at it all measured something else.
#
# DEFINING A MEDALLIST IS THE HARD PART, and getting it wrong flatters or damns
# the answer by tens of percent. Three traps, all hit before this was written:
#
#   place <= 3        `place` carries sentinels. 0 and -1 appear in the data for
#                     DNF / DNS / DQ, and both satisfy `<= 3`, so a filter
#                     written that way counts athletes who did not finish as
#                     medallists. Use place %in% 1:3.
#   any round         Top three in a HEAT is not a medal. Without a round filter
#                     the count came to 991 "medal performances" for a
#                     championship that awards about 144.
#   NA event_id       Some rows carry no event, and they duplicate an athlete
#                     who already appears with a real one.
#
# TWO STANDARDS, and the strict one is the real target. "Ranked somewhere" is
# satisfied by a decathlete who appears in the 100m; it does not mean the model
# has an opinion about the event they won. "Ranked in THAT event" is what a user
# would check.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("athlete_id","event_id","date","place","round",
                                        "comp_name","mark","scoreable","perf",
                                        "competition_id")))
c0[, `:=`(athlete_id = as.character(athlete_id),
          competition_id = as.character(competition_id))]
# The NAME is not enough. A June 2024 competition also carries "XXXIII Olympic"
# in its comp_name and contributed a whole extra set of 1-2-3 placings to the
# 100m. Pin the competition_id, chosen as the one holding the August dates, and
# assert the window rather than trusting the name.
oly_ids <- c0[grepl("XXXIII Olympic", comp_name) &
              date >= as.Date("2024-08-01") & date <= as.Date("2024-08-11"),
              .N, by = competition_id][order(-N)]
stopifnot("no Paris 2024 competition found in the August window" = nrow(oly_ids) > 0)
OLY <- oly_ids$competition_id[1]
oly <- c0[competition_id == OLY]
cat(sprintf("Paris 2024 competition %s: %s rows, %s to %s\n", OLY,
            format(nrow(oly), big.mark = ","), min(oly$date), max(oly$date)))
stopifnot("no Paris 2024 rows in the corpus" = nrow(oly) > 1000,
          "the chosen competition spans more than the Games fortnight" =
            as.numeric(max(oly$date) - min(oly$date)) <= 20)

# A COMBINED-EVENT MEDAL IS THE AGGREGATE, NOT A COMPONENT. Decathletes placing
# 1-3 within their decathlon 100m carry round "Combined - Group" and event_id
# AT-100Metres-M, and admitting that round wholesale awarded nine extra 100m
# medals. The decathlon medal lives on the AT-Decathlon-M row.
COMBINED <- grep("Decathlon|Heptathlon|Pentathlon", unique(oly$event_id), value = TRUE)
fin <- oly[place %in% 1:3 & !is.na(event_id) &
           (round == "Final" | event_id %chin% COMBINED)]
med <- unique(fin[, .(athlete_id, event_id, place)])
med <- med[, .SD[which.min(place)], by = .(athlete_id, event_id)]
cat(sprintf("Paris 2024: %d medals across %d events, %d athletes\n",
            nrow(med), uniqueN(med$event_id), uniqueN(med$athlete_id)))
by_ev <- med[, .N, by = event_id]
cat(sprintf("events awarding exactly 3: %d | more than 3 (ties/relays): %d | fewer: %d\n",
            sum(by_ev$N == 3), sum(by_ev$N > 3), sum(by_ev$N < 3)))
stopifnot("medal count is implausible for an Olympic athletics programme" =
            nrow(med) > 100 && nrow(med) < 200)

# FOLLOW FORM_TAG. This hardcoded the deployed display, so running it against an
# arm silently re-checked `final` and reported a pass about a file the arm had
# never touched - a verification step that could not fail for the thing it was
# being asked about. Caught on 2026-08-21 while verifying harvest1.
.tag <- Sys.getenv("FORM_TAG", "final")
.f <- file.path(D, sprintf("form_display_%s.parquet", .tag))
stopifnot("no display for that FORM_TAG - build it first with form_display_marks.R" =
            file.exists(.f))
cat(sprintf("checking %s
", basename(.f)))
d <- setDT(read_parquet(.f))
d[, athlete_id := as.character(athlete_id)]
med[, ranked_event := paste(athlete_id, event_id) %chin% paste(d$athlete_id, d$event_id)]
med[, ranked_any   := athlete_id %chin% d$athlete_id]
cat(sprintf("\nranked in the event they medalled in : %d of %d (%.1f%%)  <- THE TARGET\n",
            sum(med$ranked_event), nrow(med), 100*mean(med$ranked_event)))
cat(sprintf("ranked in any event at all           : %d of %d (%.1f%%)\n",
            sum(med$ranked_any), nrow(med), 100*mean(med$ranked_any)))

# --- why each gap exists, separating harvest from filter ----------------------
# The distinction that matters for what to DO: an athlete we have no recent
# results for needs harvesting, and one we have results for but did not rate
# needs a code fix. They look identical on the page.
st <- setDT(read_parquet(file.path(D, "seqv2_state_final.parquet")))
st[, athlete_id := as.character(athlete_id)]
sm <- st[, .(rated_last = max(last), rated_neff = max(n_eff)), by = .(athlete_id, event_id)]
cm <- c0[scoreable == TRUE & is.finite(perf),
         .(corpus_last = max(date), corpus_marks = .N), by = .(athlete_id, event_id)]
g <- merge(med[ranked_event == FALSE], cm, by = c("athlete_id","event_id"), all.x = TRUE)
g <- merge(g, sm, by = c("athlete_id","event_id"), all.x = TRUE)
ASOF <- max(d$last, na.rm = TRUE)
g[, since_corpus := as.numeric(ASOF - corpus_last)]
g[, since_rated  := as.numeric(ASOF - rated_last)]
# THE WINDOW SEES THE RATED DATE, NOT THE CORPUS DATE, and the gap between them
# is itself a category. Jemima Montag looked "unexplained" because her most
# recent 20km walk is 273 days old in the corpus - but it was contested at a WA
# category F meeting, which the engine skips by design, so her last RATED walk
# is 533 days old and the filter is behaving correctly. Classifying on
# corpus_last mislabels that as a mystery; classifying on rated_last names it.
g[, cause := fcase(
  is.na(corpus_last),      "no result at all in this event - HARVEST",
  is.na(rated_last),       "results exist but none ever rated - CODE",
  since_corpus <= 400 & since_rated > 400,
                           "raced recently, but only below the rating tier",
  since_rated > 400,       "no rated result in this event for 400d+",
  rated_neff < 1,          "rated but n_eff < 1 - evidence bar",
  default =                "unexplained - INVESTIGATE")]
cat(sprintf("\n=== why %d medallists are unranked in their own event ===\n", nrow(g)))
print(g[, .N, by = cause][order(-N)])

nm <- as.data.table(readRDS(file.path(D, "athlete_name_lookup.rds")))
nm[, athlete_id := as.character(athlete_id)]
g <- merge(g, unique(nm[, .(athlete_id, athlete_name)], by = "athlete_id"),
           by = "athlete_id", all.x = TRUE)
setorder(g, cause, since_corpus)
cat("\n=== the full list, so it can be worked through ===\n")
print(g[, .(athlete = substr(fifelse(is.na(athlete_name), "(no name)", athlete_name), 1, 22),
            event = sub("^AT-", "", event_id), place,
            corpus_last, since_corpus, since_rated,
            neff = round(rated_neff, 2), cause)], nrows = 100)

f <- file.path(D, "paris_medallist_coverage.json")
writeLines(jsonlite::toJSON(list(asof = as.character(ASOF), medals = nrow(med),
                                 ranked_event = sum(med$ranked_event),
                                 ranked_any = sum(med$ranked_any),
                                 gaps = g), dataframe = "rows",
                            auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
