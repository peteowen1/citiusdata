# What did retiring the 210-day athlete window actually cost?
#
# The number quoted when it shipped was "precision@10 falls about 1.8 pp", taken
# from a stale comment. p@10 is the wrong referee for this and we knew it: 440
# slots across 44 events, one athlete moves it 0.2 pp. Re-measured on the set
# that has power - p@16, Spearman over every matched athlete, and Spearman
# restricted to World Athletics' top 30.
#
# THE COMPARISON HAS TO HOLD THE POPULATION FIXED. Widening a window ADDS
# athletes to the page, so it also adds them to the matched set - n_top30 goes
# 988 -> 1,155 across the event sweep. The added athletes are by construction the
# ones with the stalest evidence, so a correlation computed on the wider set is
# scored on a harder population than the narrower one. Comparing those two
# numbers answers "is this population harder to rank", not "did the ranking get
# worse", and they are different questions with different answers.
#
# So every arm is scored TWICE:
#   as-shown  - each arm on its own population. What a user sees.
#   common    - every arm restricted to the athletes ALL arms show. Isolates
#               whether the ORDERING changed, with composition held constant.
# A drop in as-shown with no drop in common means the window admitted athletes
# who are hard to rank, not that it degraded the rating of anyone already there.
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


st <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
h  <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                         col_select = c("athlete_id","date","seen")))
h[, athlete_id := as.character(athlete_id)]
la <- h[seen == TRUE, .(last_any = max(date)), by = athlete_id]
st <- merge(st, la, by = "athlete_id", all.x = TRUE)
st <- st[!is.na(last) & !is.na(last_any)]
ASOF <- max(st$last, na.rm = TRUE)

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
wa[, athlete_id := as.character(athlete_id)]
wa <- wa[event_id %chin% unique(st$event_id)]
stopifnot("WA mapping produced nothing usable" = uniqueN(wa$event_id) > 10)

# the arms: the athlete window as it was, as it is now, and the event window
# alternatives that were on the table
ARMS <- list(
  "athlete 210 / event 400 (before)" = c(ath = 210, evt = 400),
  "athlete 730 / event 400 (now)"    = c(ath = 730, evt = 400),
  "athlete 730 / event 550"          = c(ath = 730, evt = 550),
  "athlete 730 / event 730"          = c(ath = 730, evt = 730))
sel_of <- function(a) st[n_eff >= 1 & last_any >= ASOF - a[["ath"]] &
                         last >= ASOF - a[["evt"]]]
sels <- lapply(ARMS, sel_of)
keys <- lapply(sels, function(s) paste(s$event_id, s$athlete_id))
common <- Reduce(intersect, keys)
cat(sprintf("arms: %d | smallest shows %s athlete-events | common to all: %s\n",
            length(ARMS), format(min(vapply(sels, nrow, 1L)), big.mark = ","),
            format(length(common), big.mark = ",")))
stopifnot("the arms share almost nothing - a common comparison is meaningless" =
            length(common) > 0.5 * min(vapply(sels, nrow, 1L)))

metrics <- function(sel, restrict_to = NULL) {
  s <- copy(sel)
  if (!is.null(restrict_to)) s <- s[paste(event_id, athlete_id) %chin% restrict_to]
  if (!nrow(s)) return(NULL)
  setorder(s, event_id, -R)
  s[, rk := seq_len(.N), by = event_id]
  prec <- function(N) {
    res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
      wN <- wa[event_id == EV][order(wa_place)][seq_len(min(N, .N))]
      oN <- s[event_id == EV & rk <= N, .(athlete_id)]
      if (!nrow(oN) || !nrow(wN)) return(NULL)
      data.table(wa_n = nrow(wN), hits = sum(wN$athlete_id %chin% oN$athlete_id))
    }))
    if (!nrow(res)) return(NA_real_)
    round(100 * sum(res$hits) / sum(res$wa_n), 1)
  }
  m  <- merge(wa[, .(event_id, athlete_id, wa_place)],
              s[, .(event_id, athlete_id, rk)], by = c("event_id", "athlete_id"))
  mt <- m[wa_place <= 30]
  data.table(shown = nrow(s), `p@10` = prec(10), `p@16` = prec(16),
             spearman = round(stats::cor(m$rk, m$wa_place, method = "spearman"), 4),
             sp_top30 = round(stats::cor(mt$rk, mt$wa_place, method = "spearman"), 4),
             n_all = nrow(m), n_top30 = nrow(mt))
}
as_shown <- rbindlist(lapply(names(ARMS), function(k)
  cbind(arm = k, metrics(sels[[k]]))))
on_common <- rbindlist(lapply(names(ARMS), function(k)
  cbind(arm = k, metrics(sels[[k]], restrict_to = common))))

cat("\n=== AS SHOWN: each arm on its own population ===\n")
print(as_shown)
cat("\n=== ON THE COMMON POPULATION: composition held fixed ===\n")
print(on_common)
stopifnot("the common-population arms differ in size - the restriction failed" =
            uniqueN(on_common$shown) == 1)
cat(sprintf("\nAll four arms rank the SAME %s athlete-events above.\n",
            format(on_common$shown[1], big.mark = ",")))

base <- "athlete 210 / event 400 (before)"
now  <- "athlete 730 / event 400 (now)"
d_shown  <- on_common[arm == now, sp_top30] - on_common[arm == base, sp_top30]
a_shown  <- as_shown[arm == now, sp_top30]  - as_shown[arm == base, sp_top30]
cat(sprintf("\nRetiring the 210-day athlete window, top-30 Spearman:\n"))
cat(sprintf("  as shown        %+.4f  (populations differ: %d vs %d ranked athletes)\n",
            a_shown, as_shown[arm == base, n_top30], as_shown[arm == now, n_top30]))
cat(sprintf("  common population %+.4f  (same athletes both sides)\n", d_shown))
cat("\nIf the common figure is ~0, the window changed WHO is shown and not how\n")
cat("well anyone is ranked - which is the outcome we wanted, since the reason\n")
cat("for widening it was that the right people were missing entirely.\n")

f <- file.path(D, "window_common_comparison.json")
writeLines(jsonlite::toJSON(list(asof = as.character(ASOF), common_n = length(common),
                                 as_shown = as_shown, on_common = on_common),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
