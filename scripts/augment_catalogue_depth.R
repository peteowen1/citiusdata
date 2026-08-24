# Floor T3 -> T2 for meets that demonstrably held elite fields.
#
# RUNS AFTER augment_catalogue_wa_codes.R, and is the same shape: a ONE-WAY
# floor, never a demotion, never producing a T1.
#
# WHY THIS MATTERS MORE THAN A REWEIGHTING. form_ratings.R keeps only T1_elite
# and T2_strong and INNER-JOINS, so a T3 competition is not down-weighted, it is
# absent. Every race in a wrongly-T3 meet is invisible to the model, which is
# also why augment_catalogue_wa_codes.R exists.
#
# THE MEASURE IS DEPTH, NOT THE BEST MARK. "This T3 meet contains an elite mark"
# is usually one exceptional teenager in a weak field: of 1,408 T3 meets holding
# at least one mark past the 99th percentile of its event, 808 - 57.4% - got
# there on a SINGLE athlete. One athlete cannot manufacture depth, so this counts
# DISTINCT athletes clearing the bar, and requires breadth across events on top,
# because a strong field in one event is an invitational rather than a meet.
#
# PERCENTILES ARE TAKEN IN A RECENT WINDOW ONLY. Measuring against an all-time
# distribution is what once classified the Olympics as second tier: the sport
# gets faster, so every old meet scores low against a modern bar.
#
# ON OVERRIDING THE CLASS BLOCK. augment_catalogue_wa_codes.R refuses to lift
# meets whose CLASS names them as development, on the deliberate principle that
# knowledge from the name outranks a measurement on their results. That rule is
# kept, with one narrow exception argued from evidence rather than convenience:
# `age_group` conflates under-18 and under-20 meets, where the reasoning plainly
# holds, with UNDER-23, where it does not - a U23 international field is largely
# senior internationals. The European U23 Championships scores 92.0 and 87.3 on
# the catalogue's own strength measure against a median of 85.7 for T1 and 70.6
# for T2, while sitting in T3. So U23 specifically may be lifted, and only when
# its strength clears the T2 median. U18, U20, youth and junior stay blocked, as
# do ncaa_lower, team_champs_lower and club_meet - those name a LOWER DIVISION of
# a competition rather than an age band, and that is a different argument which
# this script does not attempt.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D      <- here::here("citiusdata", "data")
F_CAT  <- file.path(D, "competition_catalogue.parquet")
FROMY  <- .env_int("DEPTH_FROM_YEAR",  "2023")
QBAR   <- .env_num("DEPTH_ELITE_Q",    "0.99")
MINN   <- .env_int("DEPTH_MIN_EVENT_N","2000")
MIN_A  <- .env_int("DEPTH_MIN_ATHLETES","10")   # distinct athletes past the bar
MIN_E  <- .env_int("DEPTH_MIN_EVENTS",  "5")    # across this many events
stopifnot("run build_competition_catalogue.R and the augment chain first" = file.exists(F_CAT))

cg <- setDT(read_parquet(F_CAT))
cg[, competition_id := as.character(competition_id)]
n0 <- nrow(cg); t0 <- cg[, .N, by = meet_tier][order(meet_tier)]
cat(sprintf("catalogue before: %s competitions\n", format(n0, big.mark = ",")))
print(t0)
stopifnot("catalogue looks truncated - expected the full augmented table" = n0 > 20000,
          "catalogue has no meet_tier column" = "meet_tier" %chin% names(cg))

c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("competition_id","event_id","athlete_id",
                                        "perf","date","scoreable")))
c0[, competition_id := as.character(competition_id)]
c0 <- c0[scoreable == TRUE & is.finite(perf) & year(date) >= FROMY]
stopifnot("no marks in the window" = nrow(c0) > 10000)
keep <- c0[, .N, by = event_id][N >= MINN, event_id]
c0 <- c0[event_id %chin% keep]
c0[, elite := perf >= stats::quantile(perf, QBAR, na.rm = TRUE), by = event_id]
dep <- c0[elite == TRUE, .(elite_athletes = uniqueN(athlete_id),
                           elite_events = uniqueN(event_id)), by = competition_id]
cg <- merge(cg, dep, by = "competition_id", all.x = TRUE)
cg[is.na(elite_athletes), `:=`(elite_athletes = 0L, elite_events = 0L)]

T2MED <- cg[meet_tier == "T2_strong" & !is.na(strength), stats::median(strength)]
cat(sprintf("\nmedian strength of an existing T2 meet: %.1f\n", T2MED))
stopifnot("no T2 meet has a strength - cannot set the bar" = is.finite(T2MED))

BLOCKED <- c("age_group", "ncaa_lower", "team_champs_lower", "club_meet")
cg[, .u23 := class == "age_group" & grepl("U23", comp_name, fixed = TRUE) &
              !grepl("U18|U20|Youth|Junior", comp_name, ignore.case = TRUE) &
              !is.na(strength) & strength > T2MED]
cg[, .liftable := meet_tier == "T3_development" &
                  elite_athletes >= MIN_A & elite_events >= MIN_E &
                  (!(class %chin% BLOCKED) | .u23 == TRUE)]

lift <- cg[.liftable == TRUE]
cat(sprintf("\nT3 meets with %d+ elite athletes across %d+ events: %d\n", MIN_A, MIN_E,
            cg[meet_tier == "T3_development" & elite_athletes >= MIN_A & elite_events >= MIN_E, .N]))
cat(sprintf("of those, liftable: %d (U23 exception used on %d)\n", nrow(lift), lift[.u23 == TRUE, .N]))
cat(sprintf("still blocked by class: %d\n",
            cg[meet_tier == "T3_development" & elite_athletes >= MIN_A & elite_events >= MIN_E &
               class %chin% BLOCKED & .u23 != TRUE, .N]))
cat("\n=== everything lifted ===\n")
print(lift[order(-elite_athletes)][seq_len(min(15L, .N)),
      .(comp_name = substr(comp_name, 1, 40), year, class,
        strength = round(strength, 1), elite_athletes, elite_events)])
cat("\n=== still blocked, for the record ===\n")
print(cg[meet_tier == "T3_development" & elite_athletes >= MIN_A & elite_events >= MIN_E &
         class %chin% BLOCKED & .u23 != TRUE][order(-elite_athletes)][seq_len(min(8L, .N)),
      .(comp_name = substr(comp_name, 1, 40), year, class,
        strength = round(strength, 1), elite_athletes, elite_events)])

# COUNTED BEFORE THE LIFT. An anchor needs a value from before the change; the
# first version of this compared the post-lift count to itself and was TRUE by
# construction - a vacuous guard in the file that exists to assert things.
.youth <- function(dt) dt[class == "age_group" &
                          grepl("U18|U20|Youth|Junior", comp_name, ignore.case = TRUE) &
                          !grepl("U23", comp_name, fixed = TRUE) &
                          meet_tier != "T3_development", .N]
.youth_before <- .youth(cg)
cg[.liftable == TRUE, meet_tier := "T2_strong"]
.n_lift <- nrow(lift)
cg[, c(".liftable", ".u23", "elite_athletes", "elite_events") := NULL]

# ---- ANCHORS, written before the run ---------------------------------------
oly <- cg[class == "olympics"]; wch <- cg[class == "world_champs"]
stopifnot(
  "the depth floor lifted no meet at all - it is inert" = .n_lift > 0,
  "the floor changed the number of competitions" = nrow(cg) == n0,
  "the floor must never produce a T1 meet" =
    cg[, sum(meet_tier == "T1_elite")] == t0[meet_tier == "T1_elite", N],
  "a T2 meet was demoted - this floor is one-way" =
    cg[, sum(meet_tier == "T2_strong")] >= t0[meet_tier == "T2_strong", N],
  "an Olympics stopped being T1" = nrow(oly) == 0 || all(oly$meet_tier == "T1_elite"),
  "a World Championships stopped being T1" = nrow(wch) == 0 || all(wch$meet_tier == "T1_elite"),
  "a U18, U20, youth or junior meet was lifted - only U23 is exempt" =
    .youth(cg) == .youth_before)
.bak <- file.path(D, "competition_catalogue.before_depth.parquet")
if (!file.exists(.bak)) file.copy(F_CAT, .bak)
write_parquet(cg, F_CAT)
cat(sprintf("previous catalogue kept at %s\n", basename(.bak)))
cat(sprintf("\nlifted %s competitions T3 -> T2\n", format(.n_lift, big.mark = ",")))
print(cg[, .N, by = meet_tier][order(meet_tier)])
cat("\nANCHORS PASSED: Olympics and World Championships still T1, no demotions,\n")
cat("no new T1, and no U18/U20/youth meet lifted.\n")
