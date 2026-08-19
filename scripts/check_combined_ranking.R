# Does a SIMULATED combined-event score rank athletes better than the rating
# built from their points totals?
#
# The simulation is coherent (correlation 0.95 with real totals, 92-97% coverage
# on recent performances) but coherence is not skill. The question is whether it
# orders a field better, and the referee is the same outside check used
# everywhere else: of World Athletics' top ten, how many appear in ours.
#
# THREE CANDIDATES, because "replace" and "keep" are not the only options:
#   total   - today's rating, built from the points total as a single event
#   sim     - the score simulated from ten component ratings
#   blend   - the simulation as a PRIOR that the measured total updates, weighted
#             by how many combined events the athlete has actually contested
# The third is the one worth wanting: an athlete who has scored 8,500 twice needs
# no prior, and one with a single junior decathlon on file is almost all prior.
#
# COVERAGE IS A RESULT, NOT A FOOTNOTE. Only athletes with a rating in EVERY slot
# can be simulated. If the World Athletics top ten are not simulable, the
# simulation cannot rank them at all and a flattering precision figure computed
# over the remainder would be measuring the wrong thing.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("STATE_TAG", "base4")
CE_EVENTS <- c("AT-Decathlon-M", "AT-Heptathlon-M", "AT-Heptathlon-W", "AT-Pentathlon-W")

st <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("athlete_id", "event_id", "date")))
h[, athlete_id := as.character(athlete_id)]
sim <- setDT(read_parquet(file.path(D, "combined_simulated.parquet")))
sim[, athlete_id := as.character(athlete_id)]
comp <- setDT(read_parquet(file.path(D, "combined_components.parquet")))
comp[, athlete_id := as.character(athlete_id)]

# read the deployed active filter rather than copy it - a copied filter that
# drifts is how a harness ends up scoring a different population than the page
.deployed <- function(nm) {
  src <- readLines(here::here("citiusdata", "scripts", "form_display_marks.R"), warn = FALSE)
  ln <- grep(sprintf("^%s[[:space:]]*<-", nm), src, value = TRUE)[1]
  v <- suppressWarnings(as.integer(sub('.*"([0-9]+)".*', "\\1", ln)))
  stopifnot("could not read the deployed filter value" = is.finite(v)); v
}
ACT_A <- .deployed("ACT_ATHLETE"); ACT_E <- .deployed("ACT_EVENT")
la <- h[, .(last_any = max(date)), by = athlete_id]
st <- merge(st, la, by = "athlete_id", all.x = TRUE)
ASOF <- max(st$last, na.rm = TRUE)
act <- st[event_id %chin% CE_EVENTS & n_eff >= 1 &
          !is.na(last_any) & last_any >= ASOF - ACT_A &
          !is.na(last) & last >= ASOF - ACT_E]
cat(sprintf("as at %s | active combined-event athletes: %s (filter %dd / %dd)\n",
            ASOF, format(nrow(act), big.mark = ","), ACT_A, ACT_E))

# how many combined events has each athlete actually contested?
nperf <- unique(comp[complete == TRUE, .(tid, ce, athlete_id)])[
  , .(perfs = .N), by = .(ce, athlete_id)]
act <- merge(act, sim[, .(ce, athlete_id, sim_mean, sim_sd)],
             by.x = c("event_id", "athlete_id"), by.y = c("ce", "athlete_id"), all.x = TRUE)
act <- merge(act, nperf, by.x = c("event_id", "athlete_id"),
             by.y = c("ce", "athlete_id"), all.x = TRUE)
act[is.na(perfs), perfs := 0]
cat("\n=== coverage: who can be simulated at all? ===\n")
print(act[, .(active = .N, simulable = sum(is.finite(sim_mean)),
              pct = round(100 * mean(is.finite(sim_mean)), 1)), by = event_id][order(event_id)])

# --- the three ranking keys ---------------------------------------------------
# z within event so the total-based rating and a points score are comparable
act[, z_total := (R_ceil - mean(R_ceil)) / stats::sd(R_ceil), by = event_id]
act[is.finite(sim_mean), z_sim := (sim_mean - mean(sim_mean)) / stats::sd(sim_mean),
    by = event_id]
# prior weight falls with how many combined events the athlete has on file
PW <- as.numeric(Sys.getenv("CE_PRIOR_W", "2"))
act[, w_prior := fifelse(is.finite(z_sim), PW / (perfs + PW), 0)]
act[, z_blend := fifelse(is.finite(z_sim), (1 - w_prior) * z_total + w_prior * z_sim, z_total)]
cat(sprintf("\nprior weight (CE_PRIOR_W = %.1f): median %.2f over simulable athletes\n",
            PW, stats::median(act[is.finite(z_sim), w_prior])))

wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]
wa[, sex := fifelse(grepl("^Men", event_group), "M", "W")]
wa[, disc := gsub(" ", "", sub("^(Men|Women)'s ", "", event_group))]
wa[, event_id := paste0("AT-", disc, "-", sex)]
wa <- wa[event_id %chin% CE_EVENTS]
stopifnot("World Athletics rankings cover no combined event" = nrow(wa) > 0)
cat(sprintf("WA covers %d combined events, %d ranked athletes\n",
            uniqueN(wa$event_id), nrow(wa)))

cat("\n=== can the simulation even see the WA top ten? ===\n")
top <- wa[, .SD[order(wa_place)][seq_len(min(10, .N))], by = event_id]
top <- merge(top[, .(event_id, athlete_id = as.character(athlete_id), wa_place)],
             act[, .(event_id, athlete_id, simulable = is.finite(sim_mean), perfs)],
             by = c("event_id", "athlete_id"), all.x = TRUE)
print(top[, .(wa_top10 = .N, in_our_pool = sum(!is.na(simulable)),
              simulable = sum(simulable, na.rm = TRUE)), by = event_id][order(event_id)])

score <- function(col, lab) {
  s <- act[is.finite(get(col))]
  s <- s[order(event_id, -get(col))]
  s[, rk := seq_len(.N), by = event_id]
  ours <- s[rk <= 10, .(event_id, athlete_id)]
  res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
    w10 <- wa[event_id == EV][order(wa_place)][seq_len(min(10, .N))]
    o10 <- ours[event_id == EV]
    if (!nrow(o10) || !nrow(w10)) return(NULL)
    data.table(event_id = EV, wa_n = nrow(w10),
               hits = sum(as.character(w10$athlete_id) %chin% o10$athlete_id))
  }))
  if (!nrow(res)) return(NULL)
  data.table(ranked_on = lab, events = nrow(res), wa_slots = sum(res$wa_n),
             `precision@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1))
}
cat("\n=== agreement with World Athletics: top-ten overlap ===\n")
print(rbindlist(list(
  score("z_total", "points total (today)"),
  score("z_sim",   "simulated from components"),
  score("z_blend", sprintf("blend, prior weight %.1f", PW))), fill = TRUE))
cat("20 slots across the only two combined events WA ranks. One athlete moves\n")
cat("this by 5 points, so it cannot separate the keys - hence the next table.\n")

# RANK CORRELATION over every WA-ranked athlete, not just the top ten. Same
# referee, roughly five times the sample, and it uses the whole ordering rather
# than a membership test at an arbitrary cutoff.
waall <- merge(wa[, .(event_id, athlete_id = as.character(athlete_id), wa_place)],
               act, by = c("event_id", "athlete_id"))
cat(sprintf("\n=== rank correlation with the WA order (%d ranked athletes matched) ===\n",
            nrow(waall)))
rho <- function(col, lab) {
  x <- waall[is.finite(get(col))]
  if (nrow(x) < 10) return(NULL)
  per <- x[, .(rho = stats::cor(-get(col), wa_place, method = "spearman")), by = event_id]
  data.table(ranked_on = lab, athletes = nrow(x),
             spearman = round(stats::cor(-x[[col]], x$wa_place, method = "spearman"), 3),
             dec = round(per[event_id == "AT-Decathlon-M", rho], 3),
             hep = round(per[event_id == "AT-Heptathlon-W", rho], 3))
}
print(rbindlist(list(
  rho("z_total", "points total (today)"),
  rho("z_sim",   "simulated from components"),
  rho("z_blend", sprintf("blend, prior weight %.1f", PW))), fill = TRUE))
cat("Higher is better. Computed within event then pooled, over athletes present\n")
cat("in both orderings, so it is unaffected by whom WA chooses to rank.\n")

cat("\n=== sweeping the prior weight ===\n")
sw <- rbindlist(lapply(c(0.5, 1, 2, 4, 8, 20, 1e6), function(pw) {
  act[, w_p := fifelse(is.finite(z_sim), pw / (perfs + pw), 0)]
  act[, z_p := fifelse(is.finite(z_sim), (1 - w_p) * z_total + w_p * z_sim, z_total)]
  w2 <- merge(wa[, .(event_id, athlete_id = as.character(athlete_id), wa_place)],
              act, by = c("event_id", "athlete_id"))
  a <- score("z_p", "x")
  data.table(prior_w = ifelse(pw > 1e5, Inf, pw),
             median_w_on_sim = round(stats::median(act[is.finite(z_sim), w_p]), 2),
             `precision@10` = a$`precision@10`,
             spearman = round(stats::cor(-w2$z_p, w2$wa_place, method = "spearman"), 3))
}), fill = TRUE)
print(sw)
cat("prior_w = Inf is the pure simulation wherever it exists, falling back to the\n")
cat("measured total only where it does not - the simulation WITH full coverage,\n")
cat("which a pure-sim ranking cannot offer.\n")

cat("\n=== where they disagree most: top 10 by each key ===\n")
d <- setDT(read_parquet(file.path(D, "form_display_final.parquet")))
nm <- unique(d[, .(athlete_id = as.character(athlete_id), athlete_name)])
EV <- Sys.getenv("CE_SHOW", "AT-Decathlon-M")
x <- merge(act[event_id == EV], nm, by = "athlete_id", all.x = TRUE)
# seq_len, not 1:10. With fewer than ten simulable athletes this indexed past
# the end and the loop below then printed the NA rows as if they were
# athletes - a fabricated top ten, not a short one.
mk <- function(col) { y <- x[is.finite(get(col))][order(-get(col))]
                      y <- y[seq_len(min(10L, nrow(y)))]
                      paste(sprintf("%2d. %s", seq_len(nrow(y)),
                                    substr(y$athlete_name, 1, 22)), collapse = "\n") }
cat(sprintf("\n-- %s --\nBY POINTS TOTAL%sBY SIMULATION\n", EV, strrep(" ", 12)))
a <- strsplit(mk("z_total"), "\n")[[1]]; b <- strsplit(mk("z_sim"), "\n")[[1]]
for (i in seq_along(a)) cat(sprintf("%-27s %s\n", a[i], b[i]))
cat("\nsimulated scores for that top ten:\n")
print(x[is.finite(z_sim)][order(-z_sim)][1:10,
        .(athlete = substr(athlete_name, 1, 22), sim = sim_mean, sd = sim_sd,
          perfs, n_eff = round(n_eff, 1))])
