# Worked traces for the model explainer, regenerated from the CURRENT history.
#
# The published explainer's traces came from a model without seeding, without
# the robust update and with SEEDHL 365. They describe a model that no longer
# exists. This regenerates them, and recomputes the update EXACTLY as
# form_ratings.R does - including the Huber cap, which the earlier version
# predated - so the arithmetic printed on the page is the arithmetic that ran.
#
# Emits a JS array ready to paste into the artifact, so the page's numbers and
# the engine's numbers cannot drift apart by hand-transcription.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- Sys.getenv("FORM_OUT", here::here("citiusdata", "data"))
TAG <- Sys.getenv("TRACE_TAG", "final")
# Defaults to a tempfile rather than a session scratchpad path. The hardcoded one
# stopped existing when that session ended, so the default was dead for everyone.
OUT <- Sys.getenv("TRACE_OUT", file.path(tempdir(), "traces.js"))

# knobs, mirroring the engine defaults
K0 <- 0.95; KAPPA <- 3; KFLOOR <- 0.32; CENS <- 0.3; HUBER <- 3

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
nm <- unique(d[, .(athlete_id, athlete_name)])
h[, athlete_id := as.character(athlete_id)]
nm[, athlete_id := as.character(athlete_id)]
h <- merge(h, nm, by = "athlete_id", all.x = TRUE)

# race shock, exactly as the engine: mean surprise of ESTABLISHED athletes,
# scaled by the share of the field they represent
h[, est := n_eff >= 2]
h[, S := { e <- est & is.finite(perf) & is.finite(r_pre)
           (if (sum(e) >= 3L) mean(perf[e] - r_pre[e]) else 0) * (sum(e)/.N) },
  by = race_key]
h[, surprise := (perf - r_pre) - S]
h[, k := pmax(K0 * KAPPA / (n_eff + KAPPA), KFLOOR)]
h[rc != "final" & surprise < 0, k := k * CENS]              # heat censoring
# robust update: a surprise beyond HUBER of the athlete's OWN sd has its step
# capped there. Applied AFTER censoring, same order as the engine.
h[, lim := HUBER * sqrt(v_pre)]
h[is.finite(lim) & lim > 0 & abs(surprise) > lim, k := k * (lim / abs(surprise))]
h[, r_post := fifelse(seen, r_pre + k * surprise, perf - S)]
h[, nf := .N, by = race_key]

emit <- function(pat, ev, from = "2025-01-01") {
  x <- h[grepl(pat, athlete_name) & event_id == ev & date >= as.Date(from)][order(date)]
  if (!nrow(x)) { cat(sprintf("-- NO ROWS for %s / %s\n", pat, ev)); return("") }
  rows <- vapply(seq_len(nrow(x)), function(i) sprintf(
    '["%s","%s",%.2f,%.2f,%.4f,%.4f,%.3f,%.2f]',
    as.character(x$date[i]), x$rc[i],
    exp(-x$r_pre[i]), exp(-x$perf[i]), x$S[i], x$surprise[i], x$k[i], exp(-x$r_post[i])),
    character(1))
  cat(sprintf("-- %s: %d races, rating %s -> %s\n", x$athlete_name[1], nrow(x),
      sprintf("%d:%05.2f", floor(exp(-x$r_pre[1])/60), exp(-x$r_pre[1]) %% 60),
      sprintf("%d:%05.2f", floor(exp(-x$r_post[nrow(x)])/60), exp(-x$r_post[nrow(x)]) %% 60)))
  paste0("[", paste(rows, collapse = ","), "]")
}
w <- emit("Werro", "AT-800Metres-W")
k <- emit("Hodgkinson", "AT-800Metres-W")
writeLines(c(paste0("const W=", w, ";"), paste0("const H=", k, ";")), OUT)
cat(sprintf("\nwrote %s\n", OUT))

# the fall that motivated SEQ_HUBER - report how much the cap changed it
f <- h[grepl("Werro", athlete_name) & event_id == "AT-800Metres-W" &
       date == as.Date("2025-03-09")]
if (nrow(f)) {
  k_unc <- max(K0*KAPPA/(f$n_eff[1]+KAPPA), KFLOOR)
  cat(sprintf("\n2025-03-09 fall: surprise %.4f, own sd %.4f (%.1f sigma)\n",
      f$surprise[1], sqrt(f$v_pre[1]), abs(f$surprise[1])/sqrt(f$v_pre[1])))
  cat(sprintf("  k uncapped %.3f -> capped %.3f | rating %s -> %s (was %s uncapped)\n",
      k_unc, f$k[1],
      sprintf("%d:%05.2f", floor(exp(-f$r_pre[1])/60), exp(-f$r_pre[1]) %% 60),
      sprintf("%d:%05.2f", floor(exp(-f$r_post[1])/60), exp(-f$r_post[1]) %% 60),
      sprintf("%d:%05.2f", floor(exp(-(f$r_pre[1]+k_unc*f$surprise[1]))/60),
              exp(-(f$r_pre[1]+k_unc*f$surprise[1])) %% 60)))
}
