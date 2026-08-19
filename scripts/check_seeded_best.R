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
# parameterised 2026-08-19 so it can be run against the CURRENT arm; it was
# pinned to "final", which is no longer what the display is built from
TAG <- Sys.getenv("STATE_TAG", "base4")
st <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
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
# READ the deployed filter rather than copy it. This script was pinned to 330
# days while form_display_marks.R publishes at 400, so it scored 42 events where
# the page has 44 - the same drift that hid Barega from another harness.
.deployed_filter <- function() {
  src <- readLines(here::here("citiusdata", "scripts", "form_display_marks.R"), warn = FALSE)
  g <- function(nm) {
    ln <- grep(sprintf("^%s[[:space:]]*<-", nm), src, value = TRUE)[1]
    v <- suppressWarnings(as.integer(sub('.*"([0-9]+)".*', "\\1", ln)))
    stopifnot("could not read the deployed value" = is.finite(v)); v
  }
  list(athlete = g("ACT_ATHLETE"), event = g("ACT_EVENT"))
}
DEP <- .deployed_filter()
act <- st[n_eff >= 1 & !is.na(last_any) & last_any >= ASOF - DEP$athlete &
          !is.na(last) & last >= ASOF - DEP$event]

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
    w10 <- wa[event_id == EV][order(wa_place)][seq_len(min(10, .N))]
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

# --- the test that has power ---------------------------------------------------
# Top-ten overlap dilutes this change across every athlete, and only 60 of 788
# published top-10 rows carry a seed-only best. Measure it on the band it
# targets: among WA-RANKED athletes whose best the corpus never saw, does
# capping move our rank toward WA's or away from it?
cat("\n=== AFFECTED ATHLETES ONLY: those whose best is seed-only ===\n")
hb <- h[, .(hist_best = max(perf)), by = .(athlete_id, event_id)]
aa <- merge(act, hb, by = c("athlete_id", "event_id"), all.x = TRUE)
aa[, seed_only := is.finite(best) & (!is.finite(hist_best) | best > hist_best + 1e-9)]
aa[, R_cap := fifelse(seed_only, (1 - 0.30) * R + 0.30 * pmin(best, hist_best, na.rm = TRUE),
                      R_ceil)]
aa[!is.finite(R_cap), R_cap := R_ceil]
rank_on <- function(col) {
  x <- copy(aa); x[, .k := get(col)]
  setorder(x, event_id, -.k); x[, rk := seq_len(.N), by = event_id]
  x[, .(event_id, athlete_id, rk)]
}
r1 <- rank_on("R_ceil"); setnames(r1, "rk", "rk_ship")
r2 <- rank_on("R_cap");  setnames(r2, "rk", "rk_cap")
cmp <- merge(merge(r1, r2, by = c("event_id", "athlete_id")),
             wa[, .(event_id, athlete_id = as.character(athlete_id), wa_place)],
             by = c("event_id", "athlete_id"))
cmp <- merge(cmp, aa[, .(event_id, athlete_id, seed_only)],
             by = c("event_id", "athlete_id"))
cat(sprintf("WA-ranked athletes matched: %s | of those, seed-only best: %s\n",
            format(nrow(cmp), big.mark = ","), format(sum(cmp$seed_only), big.mark = ",")))
print(cmp[, .(athletes = .N,
              mean_abs_err_shipped = round(mean(abs(rk_ship - wa_place)), 2),
              mean_abs_err_capped  = round(mean(abs(rk_cap  - wa_place)), 2),
              # BOTH are ranks where 1 is best, so agreement is a POSITIVE
              # correlation - negating one flips the sign and reads as -0.93
              spearman_shipped = round(stats::cor(rk_ship, wa_place, method = "spearman"), 4),
              spearman_capped  = round(stats::cor(rk_cap,  wa_place, method = "spearman"), 4)),
          by = seed_only][order(-seed_only)])
cat("\nThe seed_only = TRUE row is the whole question. A LOWER mean absolute rank\n")
cat("error under capping means the seeded best is genuinely misplacing athletes;\n")
cat("no change means the referee cannot see it even where it acts.\n")
