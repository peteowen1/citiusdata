# Does ranking on the ceiling-blended rating still beat ranking on the raw one?
#
# WHY THIS EXISTS. The switch to R_ceil was adopted on 2026-08-18 on the strength
# of precision@10 67.7 -> 68.2, measured BEFORE a month of results (including two
# T1 championships) was harvested. That corpus no longer exists, so the evidence
# for something already published is stale. This re-runs the comparison on the
# current data.
#
# Same referee and the same metric as check_active_filter.R: of World Athletics'
# top ten in an event, how many appear in ours. An outside check, not a target.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
st <- setDT(read_parquet(file.path(D, "seqv2_state_final.parquet")))
st[, athlete_id := as.character(athlete_id)]
stopifnot("state has no R_ceil - re-run form_ratings.R" = "R_ceil" %in% names(st))
h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet"),
                        col_select = c("athlete_id", "event_id", "date")))
h[, athlete_id := as.character(athlete_id)]
wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]

map_group <- function(g) {
  sex <- fifelse(grepl("^Men", g), "M", "W")
  ev  <- sub("^(Men|Women)'s ", "", g); ev <- gsub(",", "", ev)
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

# the DEPLOYED filter, copied from form_display_marks.R so this measures what is
# actually published rather than a convenient approximation
# READ the deployed filter rather than copy it. Both this script and
# check_ceil_ranking.R hardcoded `last >= ASOF - 330` while form_display_marks.R
# had moved to 400 - so the harness scored a different population than the page
# publishes, and Barega (last 10,000m 336 days back) was missing from the check
# entirely while sitting 3rd on the live table. A copied guard that drifts is
# worse than no guard: it reports confidently on the wrong thing.
.deployed_filter <- function() {
  f <- here::here("citiusdata", "scripts", "form_display_marks.R")
  src <- readLines(f, warn = FALSE)
  g <- function(nm) {
    ln <- grep(sprintf("^%s\\s*<-", nm), src, value = TRUE)[1]
    v <- suppressWarnings(as.integer(sub('.*"([0-9]+)".*', "\\1", ln)))
    stopifnot("could not read the deployed value" = length(ln) == 1 && is.finite(v))
    v
  }
  list(athlete = g("ACT_ATHLETE"), event = g("ACT_EVENT"))
}
DEP <- .deployed_filter()
cat(sprintf("deployed active filter: athlete %d days, event %d days\n",
            DEP$athlete, DEP$event))
last_any <- h[, .(last_any = max(date)), by = athlete_id]
st <- merge(st, last_any, by = "athlete_id", all.x = TRUE)
ASOF <- max(st$last, na.rm = TRUE)
act <- st[n_eff >= 1 & !is.na(last_any) & last_any >= ASOF - DEP$athlete &
          !is.na(last) & last >= ASOF - DEP$event]
cat(sprintf("as at %s | active %s of %s athlete-events | WA covers %d events\n",
            ASOF, format(nrow(act), big.mark = ","),
            format(nrow(st), big.mark = ","), uniqueN(wa$event_id)))

score <- function(sel, col, lab) {
  sel <- copy(sel); sel[, .key := get(col)]
  setorder(sel, event_id, -.key)
  sel[, rk := seq_len(.N), by = event_id]
  ours <- sel[rk <= 10, .(event_id, athlete_id)]
  res <- rbindlist(lapply(unique(wa$event_id), function(EV) {
    w10 <- wa[event_id == EV][order(wa_place)][seq_len(min(10, .N))]
    o10 <- ours[event_id == EV]
    if (!nrow(o10) || !nrow(w10)) return(NULL)
    data.table(event_id = EV, wa_n = nrow(w10),
               hits = sum(w10$athlete_id %chin% o10$athlete_id),
               wa1 = w10$athlete_id[1] %chin% sel[event_id == EV, athlete_id])
  }))
  list(tab = data.table(ranked_on = lab, events = nrow(res),
                        `precision@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1),
                        `WA #1 shown` = round(100 * mean(res$wa1), 1)),
       per_event = res)
}
a <- score(act, "R", "raw R (the old rule)")
b <- score(act, "R_ceil", "R_ceil (shipped 2026-08-18)")
cat("\n=== agreement with World Athletics, current corpus ===\n")
print(rbindlist(list(a$tab, b$tab)))

reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
fam <- merge(merge(a$per_event[, .(event_id, hits_R = hits, wa_n)],
                   b$per_event[, .(event_id, hits_C = hits)], by = "event_id"),
             reg, by = "event_id", all.x = TRUE)
cat("\n=== by family (where it helps and where it hurts) ===\n")
ft <- fam[, .(events = .N,
              raw = round(100 * sum(hits_R) / sum(wa_n), 1),
              ceil = round(100 * sum(hits_C) / sum(wa_n), 1)), by = family]
ft[, delta := round(ceil - raw, 1)]
setorder(ft, -delta)
print(ft)
cat("\nIf the pooled delta is negative on this corpus, the change was adopted on\n")
cat("evidence that no longer holds and should be reconsidered, not defended.\n")
