# Which active-athlete filter shows the RIGHT people?
#
# `form_display_marks.R` filters `n_eff >= 3 & last >= 2026-01-01`, per
# athlete-EVENT. Josh Kerr therefore has no 1500m ranking despite 35 races and a
# 3:27.79 Olympic final, because his last 1500m was 2025-09-17 - while he raced a
# mile in July 2026. Same mechanism empties the 20km race walk, where walkers
# contest the distance twice a year.
#
# Until now there was no way to judge a filter except by eye. The profile
# harvest gives one: World Athletics publish their OWN ranking per event, built
# from a different method entirely. Agreement with it is an external referee -
# not a target to fit, but a check that the people we show are the people who
# matter.
#
# Metric: precision@10 - of WA's top 10 in an event, how many appear in ours.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

st <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
h  <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                         col_select = c("athlete_id","event_id","date")))
h[, athlete_id := as.character(athlete_id)]
wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]

# --- map WA event groups onto our event ids ---------------------------------
map_group <- function(g) {
  sex <- fifelse(grepl("^Men", g), "M", "W")
  ev  <- sub("^(Men|Women)'s ", "", g)
  ev  <- gsub(",", "", ev)
  disc <- fcase(
    ev == "110mH", "110MetresHurdles", ev == "100mH", "100MetresHurdles",
    ev == "400mH", "400MetresHurdles", ev == "3000mSC", "3000MetresSteeplechase",
    ev == "20km Race Walking", "20KilometresRaceWalk",
    ev == "35km Race Walking", "35KilometresRaceWalk",
    grepl("^[0-9]+m$", ev), paste0(sub("m$", "", ev), "Metres"),
    default = gsub(" ", "", ev))
  paste0("AT-", disc, "-", sex)
}
wa[, event_id := map_group(event_group)]
wa <- wa[event_id %chin% unique(st$event_id)]
cat(sprintf("WA rankings mapped to %d of our events, %s athlete rows\n",
            uniqueN(wa$event_id), format(nrow(wa), big.mark = ",")))

# last time each athlete raced ANYTHING, and each athlete-event
last_any <- h[, .(last_any = max(date)), by = athlete_id]
st <- merge(st, last_any, by = "athlete_id", all.x = TRUE)

# --- the variants ------------------------------------------------------------
# Each returns the subset of `st` a page would display.
VAR <- list(
  "A current (event recency, 2026)" = function(x)
    x[n_eff >= 3 & last >= as.Date("2026-01-01")],
  "B athlete active anywhere 2026"  = function(x)
    x[n_eff >= 3 & last_any >= as.Date("2026-01-01")],
  "C event recency widened to 2025" = function(x)
    x[n_eff >= 3 & last >= as.Date("2025-01-01")],
  "D athlete active + event <=24mo" = function(x)
    x[n_eff >= 3 & last_any >= as.Date("2026-01-01") &
      last >= as.Date("2024-08-01")],
  "E no recency at all"             = function(x) x[n_eff >= 3],
  "F athlete active, n_eff >= 2"    = function(x)
    x[n_eff >= 2 & last_any >= as.Date("2026-01-01")],
  # F was the best value tested AND the lowest n_eff tested, which is not the
  # same as an optimum. Bracket it downward until the metric turns.
  "G athlete active, n_eff >= 1.5"  = function(x)
    x[n_eff >= 1.5 & last_any >= as.Date("2026-01-01")],
  "H athlete active, n_eff >= 1"    = function(x)
    x[n_eff >= 1 & last_any >= as.Date("2026-01-01")],
  "I athlete active, no n_eff bar"  = function(x)
    x[last_any >= as.Date("2026-01-01")],
  "J like F + event within 24mo"    = function(x)
    x[n_eff >= 2 & last_any >= as.Date("2026-01-01") & last >= as.Date("2024-08-01")],
  "K n_eff >= 2, no activity rule"  = function(x) x[n_eff >= 2],
  # H won on precision but the eyeball check found what precision@10 cannot see:
  # it admits ratings built on one race two years ago (Mohammed Ahmed last raced
  # 2024-08-02; Wanyonyi ranked 3rd in the 1500m off n_eff 1.5). The right
  # people arrived, carrying stale evidence. So keep the low evidence bar - rare
  # events need it - but require the EVENT itself to be within a generous window.
  "L active, n>=1, event >= 2025"   = function(x)
    x[n_eff >= 1 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-01-01")],
  "M active, n>=1, event >= 2025-06"= function(x)
    x[n_eff >= 1 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-06-01")],
  "N active, n>=1.5, event >= 2025" = function(x)
    x[n_eff >= 1.5 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-01-01")],
  "O active, n>=2, event >= 2025"   = function(x)
    x[n_eff >= 2 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-01-01")],
  # M was best AND had the tightest window tested. Bracket it.
  "P active, n>=1, event >= 2025-09"= function(x)
    x[n_eff >= 1 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-09-01")],
  "Q active, n>=1, event >= 2025-12"= function(x)
    x[n_eff >= 1 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-12-01")],
  "R active, n>=1, event >= 2025-03"= function(x)
    x[n_eff >= 1 & last_any >= as.Date("2026-01-01") & last >= as.Date("2025-03-01")]
)

# --- EVENT-FREQUENCY-SCALED WINDOW ------------------------------------------
# A fixed window cannot serve both a sprinter racing weekly and a 10,000m runner
# racing annually. At 330 days the 10,000m loses Mehary (26:43) and Kiplangat
# (26:52) because they last raced 388 days ago - while the same window is far
# too loose for the 100m. So scale it by how often the event is actually
# contested: median days between an athlete's consecutive races IN THAT EVENT.
gaps <- h[order(athlete_id, event_id, date)]
gaps[, gap := as.numeric(date - shift(date)), by = .(athlete_id, event_id)]
freq <- gaps[is.finite(gap) & gap > 0, .(med_gap = stats::median(gap)), by = event_id]
cat("
event race frequency (median days between an athlete's races in it):
")
print(freq[order(-med_gap)][1:6]); print(freq[order(med_gap)][1:4])
st <- merge(st, freq, by = "event_id", all.x = TRUE)
st[is.na(med_gap), med_gap := stats::median(freq$med_gap)]
ASOF <- max(st$last, na.rm = TRUE)
for (K in c(2, 3, 4, 6)) {
  VAR[[sprintf("S window = %dx event gap", K)]] <- local({
    KK <- K
    # bounded: an event contested twice a decade should not get a ten-year
    # window, and a weekly one still needs a few months of slack
    function(x) x[n_eff >= 1 & last_any >= as.Date("2026-01-01") &
                  last >= ASOF - pmax(pmin(KK * med_gap, 900), 200)]
  })
}

score <- function(sel, lab) {
  sel <- copy(sel)
  setorder(sel, event_id, -R)
  sel[, rk := seq_len(.N), by = event_id]
  ours <- sel[rk <= 10, .(event_id, athlete_id, rk)]
  res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
    w10 <- wa[event_id == EV][order(wa_place)][seq_len(min(10, .N))]
    o10 <- ours[event_id == EV]
    if (!nrow(o10) || !nrow(w10)) return(NULL)
    data.table(event_id = EV, wa_n = nrow(w10), our_n = nrow(o10),
               hits = sum(w10$athlete_id %chin% o10$athlete_id),
               # is WA's number one shown at all?
               wa1_shown = w10$athlete_id[1] %chin% sel[event_id == EV, athlete_id])
  }))
  data.table(filter = lab,
             events = nrow(res),
             shown = sel[, .N],
             `precision@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1),
             `WA #1 shown` = round(100 * mean(res$wa1_shown), 1),
             `events with 10` = sum(res$our_n >= 10))
}
cat("\n=== agreement with the World Athletics ranking ===\n")
out <- rbindlist(lapply(names(VAR), function(k) score(VAR[[k]](st), k)))
print(out)
cat("\nprecision@10: of WA's top ten, how many we show in ours.\n")
cat("Not a target to fit - a check that the right people are on the page.\n")

cat("\n=== the anchor case: does Josh Kerr appear in the 1500m? ===\n")
jk <- st[athlete_id == "14533464" & event_id == "AT-1500Metres-M"]
if (nrow(jk)) {
  cat(sprintf("  his state: n_eff %.1f, last 1500m %s, last raced anywhere %s\n",
              jk$n_eff, jk$last, jk$last_any))
  for (k in names(VAR)) {
    s <- VAR[[k]](st)
    inn <- nrow(s[athlete_id == "14533464" & event_id == "AT-1500Metres-M"]) > 0
    r <- NA_integer_
    if (inn) {
      e <- s[event_id == "AT-1500Metres-M"][order(-R)]
      r <- which(e$athlete_id == "14533464")
    }
    cat(sprintf("  %-34s %s\n", k, if (inn) sprintf("shown, rank %d", r) else "ABSENT"))
  }
}
