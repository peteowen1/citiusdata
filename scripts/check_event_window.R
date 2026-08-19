# Should the EVENT-recency window go to two years as well?
#
# The 210-day athlete window was retired on 2026-08-19 because it deleted the
# people who race least often - it cost us the Olympic marathon champion. The
# natural follow-up is whether the EVENT window (400 days) should widen too.
#
# THE REASON THE OLD SWEEP CANNOT ANSWER IT. form_display_marks.R records
# 400d -> 70.0 precision@10 and 730d -> 69.3, which looks decisive. But that
# sweep was run with athlete-recency held at 210 days, and that constraint is
# now gone. The event window used to be the second of two gates; it is now the
# ONLY gate, so it is doing a different job and the old numbers do not describe
# this configuration. Re-measured here rather than quoted.
#
# TWO REFEREES, because they fail in opposite directions and one alone would
# pick the wrong window:
#   precision@10  - of World Athletics' top ten, how many are in ours. Punishes a
#                   window so wide that stale ratings crowd the page.
#   WA #1 shown   - is the actual world number one on the page AT ALL. Punishes a
#                   window so tight that the person who matters most is missing.
#   medallists    - share of Paris 2024 medal-round performers who have a
#                   ranking. This is the failure that was actually reported, so
#                   it is measured rather than assumed to track the others.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
ATH <- as.integer(Sys.getenv("SWEEP_ACT_ATHLETE_D", "730"))   # as now deployed

st <- setDT(read_parquet(file.path(D, "seqv2_state_final.parquet")))
st[, athlete_id := as.character(athlete_id)]
h  <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet"),
                         col_select = c("athlete_id","event_id","date","seen")))
h[, athlete_id := as.character(athlete_id)]
la <- h[seen == TRUE, .(last_any = max(date)), by = athlete_id]
st <- merge(st, la, by = "athlete_id", all.x = TRUE)
st <- st[!is.na(last) & !is.na(last_any)]
ASOF <- max(st$last, na.rm = TRUE)
stopifnot("state table is empty" = nrow(st) > 10000)
cat(sprintf("state: %s athlete-events | ASOF %s | athlete window held at %dd\n",
            format(nrow(st), big.mark = ","), ASOF, ATH))

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

am <- setDT(read_parquet(file.path(D, "adjusted_marks.parquet"),
                         col_select = c("athlete_id","comp_name","place")))
am[, athlete_id := as.character(athlete_id)]
med <- am[grepl("XXXIII Olympic", comp_name) & is.finite(place) & place <= 3]
stopifnot("no Paris 2024 medal rows found" = nrow(med) > 100)

score <- function(evt_d) {
  sel <- st[n_eff >= 1 & last_any >= ASOF - ATH & last >= ASOF - evt_d]
  if (!nrow(sel)) return(NULL)
  setorder(sel, event_id, -R)
  sel[, rk := seq_len(.N), by = event_id]
  ours <- sel[rk <= 10, .(event_id, athlete_id)]
  res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
    w <- wa[event_id == EV][order(wa_place)]
    w10 <- w[seq_len(min(10L, .N))]
    o10 <- ours[event_id == EV]
    if (!nrow(o10) || !nrow(w10)) return(NULL)
    data.table(wa_n = nrow(w10),
               hits = sum(w10$athlete_id %chin% o10$athlete_id),
               # "shown at all" is deliberately checked against the WHOLE
               # selection, not the top ten: the question is whether the page
               # contains the world number one, not where it ranks them.
               top1 = as.integer(w$athlete_id[1] %chin% sel[event_id == EV, athlete_id]))
  }))
  if (!nrow(res)) return(NULL)
  data.table(event_d = evt_d,
             shown = nrow(sel),
             `p@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1),
             `WA#1 shown` = round(100 * mean(res$top1), 1),
             medallists = round(100 * mean(med$athlete_id %chin% sel$athlete_id), 1))
}
out <- rbindlist(lapply(c(300, 365, 400, 450, 550, 730, 900, 1100), score))
cat("\n=== sweeping the EVENT window with the athlete window retired ===\n")
print(out)
cat("\nshown      = athlete-events on the page\n")
cat("p@10       = of World Athletics' top ten, how many are in ours\n")
cat("WA#1 shown = is the world number one on the page at all\n")
cat("medallists = share of Paris 2024 medal-round performers with a ranking\n")

best_p <- out[which.max(`p@10`)]
cat(sprintf("\nbest precision at %d d (%.1f). At 730 d it is %.1f, costing %.1f pp",
            best_p$event_d, best_p$`p@10`, out[event_d == 730, `p@10`],
            best_p$`p@10` - out[event_d == 730, `p@10`]))
cat(sprintf(" and buying %+.1f pp of medallist coverage.\n",
            out[event_d == 730, medallists] - out[event_d == best_p$event_d, medallists]))
cat("Judge the trade directly: a missing world number one or Olympic champion is\n")
cat("a worse failure than one extra wrong name in a top ten, but only if the\n")
cat("wider window is actually recovering those people rather than adding tails.\n")

f <- file.path(D, "event_window_sweep.json")
writeLines(jsonlite::toJSON(list(athlete_window_d = ATH, asof = as.character(ASOF),
                                 sweep = out),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
