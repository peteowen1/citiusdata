# Should the ceiling blend be allowed to use a PRE-CORPUS best?
#
# The engine seeds each athlete-event from results before the corpus opens
# (2020-01-01) and that seed sets the career best. R_ceil then puts 30% weight
# on it permanently, and no scored race can ever contradict it. Measured:
# 13.0% of athlete-events carry a best better than anything they have done in a
# scored race, and 61 of 780 published top-10 rows do.
#
# The names are the argument: Rohler (javelin, 26 scored races, best 11.3%
# better than any of them), Kolak, Wlodarczyk, Echevarria - all pre-2020
# champions ranked partly on a peak the corpus cannot see. The blend exists to
# reward a peak an athlete can still reach, not one from 2016.
#
# Scored against World Athletics, same harness as check_ceil_ranking.R.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
st <- setDT(read_parquet(file.path(D, "seqv2_state_final.parquet")))
st[, athlete_id := as.character(athlete_id)]
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet"),
                        col_select = c("athlete_id","event_id","date","perf")))
h[, athlete_id := as.character(athlete_id)]
wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]
map_group <- function(g) {
  sex <- fifelse(grepl("^Men", g), "M", "W")
  ev <- sub("^(Men|Women)'s ", "", g); ev <- gsub(",", "", ev)
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

seen <- h[, .(best_seen = max(perf)), by = .(athlete_id, event_id)]
st <- merge(st, seen, by = c("athlete_id", "event_id"), all.x = TRUE)
last_any <- h[, .(last_any = max(date)), by = athlete_id]
st <- merge(st, last_any, by = "athlete_id", all.x = TRUE)
ASOF <- max(st$last, na.rm = TRUE)
act <- st[n_eff >= 1 & !is.na(last_any) & last_any >= ASOF - 210 &
          !is.na(last) & last >= ASOF - 330]

CEIL <- 0.30
# variant B: the best may not exceed what the athlete has been seen to do in a
# scored race. Athletes with no scored best keep the seeded one.
act[, best_capped := fifelse(is.finite(best_seen) & is.finite(best) & best > best_seen,
                             best_seen, best)]
act[, R_ceil_capped := fifelse(is.na(best_capped), R, (1 - CEIL) * R + CEIL * best_capped)]

# variants C+: cap only where there is ENOUGH scored evidence for the absence of
# a matching mark to mean something. Two cases are otherwise conflated:
#   Rohler   - 26 scored races, none within 11% of his seeded best. With that
#              much evidence, never reaching it says he cannot any more.
#   a thin distance athlete with 1-2 races - the seed is legitimately the best
#              evidence about them, and capping discards it.
# That distinction is the likely reason the flat cap helps jumps and throws and
# hurts distance.
nr <- h[, .(n_races = .N), by = .(athlete_id, event_id)]
act <- merge(act, nr, by = c("athlete_id", "event_id"), all.x = TRUE)
act[is.na(n_races), n_races := 0L]
for (K in c(3L, 5L, 10L)) {
  bc <- fifelse(act$n_races >= K & is.finite(act$best_seen) & is.finite(act$best) &
                  act$best > act$best_seen, act$best_seen, act$best)
  set(act, j = sprintf("R_ceil_cap%d", K),
      value = fifelse(is.na(bc), act$R, (1 - CEIL) * act$R + CEIL * bc))
}

score <- function(sel, col, lab) {
  sel <- copy(sel); sel[, .key := get(col)]
  setorder(sel, event_id, -.key); sel[, rk := seq_len(.N), by = event_id]
  ours <- sel[rk <= 10, .(event_id, athlete_id)]
  res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
    w10 <- wa[event_id == EV][order(wa_place)][1:min(10, .N)]
    o10 <- ours[event_id == EV]
    if (!nrow(o10) || !nrow(w10)) return(NULL)
    data.table(event_id = EV, wa_n = nrow(w10),
               hits = sum(w10$athlete_id %chin% o10$athlete_id))
  }))
  list(tab = data.table(ranked_on = lab, events = nrow(res),
                        `precision@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1)),
       per = res)
}
a <- score(act, "R_ceil", "R_ceil, seeded best allowed (shipped)")
b <- score(act, "R_ceil_capped", "capped always")
c3 <- score(act, "R_ceil_cap3", "capped when >= 3 scored races")
c5 <- score(act, "R_ceil_cap5", "capped when >= 5 scored races")
c10 <- score(act, "R_ceil_cap10", "capped when >= 10 scored races")
cat("\n=== agreement with World Athletics ===\n")
print(rbindlist(list(a$tab, b$tab, c3$tab, c5$tab, c10$tab)))

reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
f <- merge(merge(merge(a$per[, .(event_id, h_a = hits, wa_n)],
                       b$per[, .(event_id, h_b = hits)], by = "event_id"),
                 c5$per[, .(event_id, h_c = hits)], by = "event_id"),
           reg, by = "event_id")
ft <- f[, .(events = .N, shipped = round(100 * sum(h_a) / sum(wa_n), 1),
            capped_always = round(100 * sum(h_b) / sum(wa_n), 1),
            capped_ge5 = round(100 * sum(h_c) / sum(wa_n), 1)), by = family]
ft[, delta := round(capped_ge5 - shipped, 1)]
setorder(ft, -delta)
cat("\n=== by family ===\n"); print(ft)
cat(sprintf("\ntop-10 rows whose best is seed-only: %d of %d\n",
            sum(act[, .SD[order(-R_ceil)][seq_len(min(10, .N))], by = event_id][
              , best > best_seen], na.rm = TRUE),
            nrow(act[, .SD[order(-R_ceil)][seq_len(min(10, .N))], by = event_id])))
