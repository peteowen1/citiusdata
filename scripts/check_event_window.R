# How long since an athlete last contested an event should they still be ranked?
#
# The filter uses 330 days. Pete's question: is that too strict, given decay?
#
# THE ANSWER TO THE DECAY PART IS NO, AND IT MATTERS. Staleness decays `n_eff`,
# the evidence count - it never decays `R`. A rating sits where the athlete's
# last race left it indefinitely, so a 2019 rating ranks at full strength. The
# recency window is the only thing holding a stale athlete down, which is why
# widening it is a real risk rather than a free option.
#
# But 330 days is tight enough to have just excluded Josh Kerr by THREE days,
# and the original sweep had 14 months tying 11 months (65.2 each) with 17
# months only slightly worse at 64.8. That was measured on the old corpus,
# before R_ceil ranking. Re-test on current data.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
st <- setDT(read_parquet(file.path(D, "seqv2_state_final.parquet")))
st[, athlete_id := as.character(athlete_id)]
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet"),
                        col_select = c("athlete_id", "date")))
h[, athlete_id := as.character(athlete_id)]
st <- merge(st, h[, .(last_any = max(date)), by = athlete_id], by = "athlete_id", all.x = TRUE)
ASOF <- max(st$last, na.rm = TRUE)
wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]
map_group <- function(g) {
  sex <- fifelse(grepl("^Men", g), "M", "W"); ev <- sub("^(Men|Women)'s ", "", g)
  ev <- gsub(",", "", ev)
  disc <- fcase(ev == "110mH","110MetresHurdles", ev == "100mH","100MetresHurdles",
    ev == "400mH","400MetresHurdles", ev == "3000mSC","3000MetresSteeplechase",
    ev == "20km Race Walking","20KilometresRaceWalk",
    ev == "35km Race Walking","35KilometresRaceWalk",
    grepl("^[0-9]+m$", ev), paste0(sub("m$","",ev),"Metres"), default = gsub(" ","",ev))
  paste0("AT-", disc, "-", sex)
}
wa[, event_id := map_group(event_group)]

score <- function(EVENT_D, ATH_D = 210) {
  a <- st[n_eff >= 1 & !is.na(last_any) & last_any >= ASOF - ATH_D &
          !is.na(last) & last >= ASOF - EVENT_D]
  setorder(a, event_id, -R_ceil); a[, rk := seq_len(.N), by = event_id]
  ours <- a[rk <= 10, .(event_id, athlete_id)]
  res <- rbindlist(lapply(intersect(unique(wa$event_id), unique(a$event_id)), function(EV) {
    w10 <- wa[event_id == EV][order(wa_place)][seq_len(min(10, .N))]
    o10 <- ours[event_id == EV]
    if (!nrow(o10) || !nrow(w10)) return(NULL)
    data.table(wa_n = nrow(w10), hits = sum(w10$athlete_id %chin% o10$athlete_id),
               wa1 = w10$athlete_id[1] %chin% a[event_id == EV, athlete_id])
  }))
  kerr <- a[athlete_id == "14533464" & event_id == "AT-1500Metres-M"]
  data.table(event_window_days = EVENT_D,
             months = round(EVENT_D / 30.4, 1),
             shown = nrow(a),
             `precision@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1),
             `WA #1 shown` = round(100 * mean(res$wa1), 1),
             kerr_1500 = if (nrow(kerr)) sprintf("rank %d", kerr$rk) else "absent")
}
cat(sprintf("as at %s | athlete-recency held at 210 days throughout\n\n", ASOF))
print(rbindlist(lapply(c(330, 345, 360, 375, 390, 400, 420, 450), score)))
cat("\nWider windows admit more people; precision falls when the extra people are\n")
cat("stale rather than merely infrequent. Look for where it turns, and note that\n")
cat("R does not decay - a rating from 2019 competes at full strength.\n")
