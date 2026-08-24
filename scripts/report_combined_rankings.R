# The combined-event top tens, side by side, under all three ranking keys.
#
#   total  - today's published rating, built from the points total as one event
#   sim    - the score simulated from ten component ratings (mean of 600 draws)
#   blend  - the simulation as a prior, updated by however many combined events
#            the athlete has actually contested
#
# With the outside references a reader needs to judge them: season best, personal
# best, and the World Athletics ranking. Precision@10 cannot separate these keys
# (20 WA slots across the only two combined events WA ranks), so the useful thing
# is to look at the names.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D   <- here::here("citiusdata", "data")
# DEFAULT TO THE DEPLOYED ARM. This read "base4" until 2026-08-20 - the arm that
# happened to be current the day it was written. Nothing failed when the engine
# moved on: the state file still existed, so this quietly scored a fresh
# simulation against a two-day-old state and called it one ranking. Five scripts
# shared the default and only one was noticed; the others were found by asking
# what ELSE reads this artefact.
TAG <- Sys.getenv("STATE_TAG", "final")
# read the DEPLOYED prior weight rather than restate it - a copied constant that
# drifts is how a report ends up describing a ranking nobody publishes
# Read the DEPLOYED prior weight rather than restate it - a copied constant that
# drifts is how a report ends up describing a ranking nobody publishes. And
# VALIDATE it: the first version of this parse silently produced NA, which made
# every blended athlete NA and left the "blend" top ten showing only the
# athletes the simulation cannot see. It looked like a ranking.
.deployed_num <- function(nm_) {
  src <- readLines(here::here("citiusdata", "scripts", "form_display_marks.R"), warn = FALSE)
  ln <- grep(sprintf("^%s[[:space:]]*<-", nm_), src, value = TRUE)[1]
  stopifnot("could not find the deployed constant" = length(ln) == 1 && !is.na(ln))
  v <- suppressWarnings(as.numeric(sub('.*"([0-9.]+)".*', "\\1", ln)))
  stopifnot("the deployed constant did not parse to a number" = is.finite(v))
  v
}
PW <- suppressWarnings(.env_num("CE_PRIOR_W", ""))
if (!is.finite(PW)) PW <- .deployed_num("CE_PW")
stopifnot("prior weight must be a positive number" = is.finite(PW) && PW > 0)
CE_EVENTS <- c("AT-Decathlon-M", "AT-Heptathlon-M", "AT-Heptathlon-W", "AT-Pentathlon-W")

st  <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
h   <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                          col_select = c("athlete_id", "event_id", "date")))
h[, athlete_id := as.character(athlete_id)]
sim <- setDT(read_parquet(file.path(D, "combined_simulated.parquet")))
# THE SIMULATION MUST COME FROM THE ARM WE ARE REPORTING. Same guard as
# form_display_marks.R: the simulation reads component ratings from
# seqv2_state_<STATE_TAG>, so a simulation built on one arm and a state read on
# another compares two different models and prints one table.
if ("state_tag" %chin% names(sim)) {
  .st <- unique(sim$state_tag)
  if (length(.st) != 1L || !identical(.st[1], TAG))
    stop("combined_simulated.parquet was built from state tag '",
         paste(.st, collapse = "/"), "' but this report is tag '", TAG,
         "'.\n  Rebuild:  STATE_TAG=", TAG,
         " Rscript citiusdata/scripts/build_combined_simulation.R")
} else {
  stop("combined_simulated.parquet predates the state_tag stamp, so the arm it\n",
       "  was built from cannot be established. Rebuild it:  STATE_TAG=", TAG,
       " Rscript citiusdata/scripts/build_combined_simulation.R")
}
sim[, athlete_id := as.character(athlete_id)]
comp <- setDT(read_parquet(file.path(D, "combined_components.parquet")))
comp[, athlete_id := as.character(athlete_id)]
d   <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
nm  <- unique(d[, .(athlete_id = as.character(athlete_id), athlete_name)])

.deployed <- function(nm_) {
  src <- readLines(here::here("citiusdata", "scripts", "form_display_marks.R"), warn = FALSE)
  ln <- grep(sprintf("^%s[[:space:]]*<-", nm_), src, value = TRUE)[1]
  v <- suppressWarnings(as.integer(sub('.*"([0-9]+)".*', "\\1", ln)))
  stopifnot("could not read the deployed filter" = is.finite(v)); v
}
ACT_A <- .deployed("ACT_ATHLETE"); ACT_E <- .deployed("ACT_EVENT")
la <- h[, .(last_any = max(date)), by = athlete_id]
st <- merge(st, la, by = "athlete_id", all.x = TRUE)
ASOF <- max(st$last, na.rm = TRUE)
act <- st[event_id %chin% CE_EVENTS & n_eff >= 1 &
          !is.na(last_any) & last_any >= ASOF - ACT_A &
          !is.na(last) & last >= ASOF - ACT_E]

# season best and personal best, from the performances themselves
perf <- unique(comp[, .(tid, ce, athlete_id, date = tdate, points = stored_points)])
sb <- perf[date >= ASOF - 365, .(sb = max(points)), by = .(ce, athlete_id)]
pb <- perf[, .(pb = max(points), perfs = .N), by = .(ce, athlete_id)]
act <- merge(act, sim[, .(ce, athlete_id, sim_mean, sim_sd)],
             by.x = c("event_id", "athlete_id"), by.y = c("ce", "athlete_id"), all.x = TRUE)
act <- merge(act, sb, by.x = c("event_id", "athlete_id"), by.y = c("ce", "athlete_id"), all.x = TRUE)
act <- merge(act, pb, by.x = c("event_id", "athlete_id"), by.y = c("ce", "athlete_id"), all.x = TRUE)
act[is.na(perfs), perfs := 0]

wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]
wa[, sex := fifelse(grepl("^Men", event_group), "M", "W")]
wa[, event_id := paste0("AT-", gsub(" ", "", sub("^(Men|Women)'s ", "", event_group)), "-", sex)]
act <- merge(act, wa[event_id %chin% CE_EVENTS,
                     .(event_id, athlete_id = as.character(athlete_id), wa = wa_place)],
             by = c("event_id", "athlete_id"), all.x = TRUE)
act <- merge(act, nm, by = "athlete_id", all.x = TRUE)

# the three keys, standardised within event so they are comparable
act[, z_total := (R_ceil - mean(R_ceil)) / stats::sd(R_ceil), by = event_id]
act[is.finite(sim_mean), z_sim := (sim_mean - mean(sim_mean)) / stats::sd(sim_mean), by = event_id]
act[, w_prior := fifelse(is.finite(z_sim), PW / (perfs + PW), 0)]
act[, z_blend := fifelse(is.finite(z_sim), (1 - w_prior) * z_total + w_prior * z_sim, z_total)]
# the total-based rating expressed back as points, so every column is in points
act[, est_total := round(exp(R_ceil))]

top_by <- function(EV, key, k = 10) {
  x <- act[event_id == EV & is.finite(get(key))][order(-get(key))][seq_len(min(k, .N))]
  x[, .(rank = seq_len(.N), athlete = athlete_name,
        rating_pts = est_total, sim = sim_mean, sd = sim_sd,
        sb, pb, perfs, wa, n_eff = round(n_eff, 1))]
}
LABS <- c(z_total = "points total (published)", z_sim = "simulated from components",
          z_blend = sprintf("blend, prior weight %.1f", PW))
out <- rbindlist(lapply(CE_EVENTS, function(EV) rbindlist(lapply(names(LABS), function(k) {
  x <- top_by(EV, k); if (!nrow(x)) return(NULL)
  cbind(event_id = EV, method = LABS[[k]], x)
}), fill = TRUE)), fill = TRUE)
stopifnot("no rankings produced" = nrow(out) > 0)

cat(sprintf("as at %s | filter %dd athlete / %dd event | prior weight %.1f\n",
            ASOF, ACT_A, ACT_E, PW))
for (EV in c("AT-Decathlon-M", "AT-Heptathlon-W")) {
  for (k in names(LABS)) {
    x <- out[event_id == EV & method == LABS[[k]]]
    if (!nrow(x)) next
    cat(sprintf("\n== %s -- %s ==\n", sub("^AT-", "", EV), LABS[[k]]))
    print(x[, .(rank, athlete = substr(athlete, 1, 22), rating_pts, sim, sd,
                sb, pb, perfs, wa)])
  }
}
f <- file.path(D, "combined_rankings_report.json")
writeLines(jsonlite::toJSON(out, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s (%d rows)\n", basename(f), nrow(out)))
