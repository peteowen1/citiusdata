# WHY DOESN'T KERR'S MILE FORM LIFT HIS 1500m?
#
# He has won the Mile and run 3:42.66; his 1500m rating sits at 3:36.55. The
# events correlate 0.873 - the strongest pair he has. So why is none of that
# reaching the 1500m?
#
# This walks the engine's own decision for one athlete-event and prints each
# gate in turn, so the answer is a list of specific blockers rather than an
# impression.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

AID <- Sys.getenv("SIB_ATHLETE", "14533464")     # Josh Kerr
EV  <- Sys.getenv("SIB_EVENT", "AT-1500Metres-M")
XB_MAXN <- 8; XB_MINCOR <- 0.80; XBLEND <- 1     # the deployed settings

st <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
me <- st[athlete_id == AID]
stopifnot("athlete not in state" = nrow(me) > 0)
secs <- function(p) exp(-p)
fmt <- function(s) sprintf("%d:%05.2f", floor(s / 60), s %% 60)

cat("=== his rating in every event ===\n")
print(me[, .(event_id, R = round(R, 4), mark = fmt(secs(R)),
             n_eff = round(n_eff, 2))][order(-n_eff)])

sim <- setDT(read_parquet(file.path(D, "event_similarity.parquet")))
sim[, `:=`(e1 = as.character(e1), e2 = as.character(e2))]
cors <- rbind(sim[e1 == EV, .(other = e2, cor)], sim[e2 == EV, .(other = e1, cor)])
sib <- merge(me[event_id != EV, .(other = event_id, R, n_eff)], cors, by = "other",
             all.x = TRUE)
setorder(sib, -cor)

# the event means the engine maps through
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("event_id", "perf")))
mu <- h[, .(mu = mean(perf)), by = event_id]
sib <- merge(sib, mu, by.x = "other", by.y = "event_id", all.x = TRUE)
mu_ev <- mu[event_id == EV, mu]
my_n <- me[event_id == EV, n_eff]; my_R <- me[event_id == EV, R]

cat(sprintf("\n=== %s: rating %s, n_eff %.2f ===\n", EV, fmt(secs(my_R)), my_n))
cat("Each sibling, what it implies for this event, and whether it is allowed:\n\n")
sib[, implied := R - mu + mu_ev]
sib[, implied_mark := fmt(secs(implied))]
sib[, eligible := is.finite(cor) & cor >= XB_MINCOR]
print(sib[, .(sibling = other, cor = round(cor, 3), n_eff = round(n_eff, 2),
              implies = implied_mark, eligible)])

cat("\n=== BLOCKER 1: the thin-record cutoff ===\n")
cat(sprintf("his n_eff here is %.2f; the blend is skipped at %.0f or above -> %s\n",
            my_n, XB_MAXN, if (my_n >= XB_MAXN) "SKIPPED ENTIRELY" else "would run"))

cat("\n=== BLOCKER 2: which sibling gets chosen ===\n")
el <- sib[eligible == TRUE]
if (nrow(el)) {
  by_n <- el[which.max(n_eff)]
  by_c <- el[which.max(cor)]
  cat(sprintf("engine rule picks MOST EVIDENCE : %s (cor %.3f, n_eff %.2f) -> implies %s\n",
              by_n$other, by_n$cor, by_n$n_eff, by_n$implied_mark))
  cat(sprintf("picking BEST CORRELATED instead : %s (cor %.3f, n_eff %.2f) -> implies %s\n",
              by_c$other, by_c$cor, by_c$n_eff, by_c$implied_mark))
  if (by_n$other != by_c$other)
    cat("  -> these DISAGREE: the engine borrows from the weaker relationship.\n")
} else cat("no sibling clears the correlation gate\n")

cat("\n=== BLOCKER 3: how much would it move him ===\n")
if (nrow(el)) {
  for (rule in c("most evidence", "best correlated")) {
    s <- if (rule == "most evidence") el[which.max(n_eff)] else el[which.max(cor)]
    for (xb in c(1, 3, 6)) {
      w <- xb / (my_n + xb)
      nv <- (1 - w) * my_R + w * s$implied
      cat(sprintf("  %-16s xb=%d  weight %5.1f%%  ->  %s (from %s)\n",
                  rule, xb, 100 * w, fmt(secs(nv)), fmt(secs(my_R))))
    }
  }
}

cat("\n=== BLOCKER 4: it would not reach the rankings anyway ===\n")
cat("The blend writes r_use, which orders a field inside a race. The published\n")
cat("table ranks on the persisted R. So even with 1-3 fixed, the ranking is\n")
cat("unchanged unless the blend is also applied at export.\n")
