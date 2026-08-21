# Should the PUBLISHED ranking borrow from correlated events?
#
# WHY THIS IS A DIFFERENT QUESTION FROM SEQ_XBLEND. The engine's cross-event
# blend modifies r_use, which is loop-local and used only to order a field while
# scoring. The state table computes R_ceil from R and the best mark alone, so no
# similarity setting - no gate, no NSIB, no matrix - can move a published rank.
# Verified 2026-08-18: Barega's 10,000m rating was byte-identical at gates 0.80,
# 0.50 and 0.30. His half marathon and 10km road CANNOT reach his 10,000m rank
# through that mechanism, however good the similarity matrix gets.
#
# So test the other mechanism: blend at display time, on the key the page ranks.
#
# WHAT WAS ALREADY REJECTED, AND WHY IT DOES NOT SETTLE THIS. An export-side
# blend was tried earlier on 2026-08-18 and rejected - against the 200-pair
# similarity matrix, which contained NO road or walk events at all. It was never
# given a matrix that could express "his half marathon says something about his
# 10,000m". event_similarity_all.parquet (668 pairs, all 85 events) can.
#
# THE REFEREE is the same as check_ceil_ranking.R and check_active_filter.R: of
# World Athletics' top ten in an event, how many appear in ours. An outside
# check, not a target.
#
# THE SCALE PROBLEM. R is in log-mark space per event, so a 10,000m rating and a
# marathon rating are not comparable numbers. Ratings are z-scored WITHIN event
# over the active pool before blending, which is exactly how the similarity
# matrix was measured, and ranking only needs the z - nothing has to be mapped
# back to seconds.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D    <- here::here("citiusdata", "data")
# DEFAULT TO THE DEPLOYED ARM. This read "base4" until 2026-08-20 - the arm that
# happened to be current the day it was written. Nothing failed when the engine
# moved on: the state file still existed, so this quietly scored against a
# two-day-old state. Five scripts shared the default; only one was noticed.
TAG  <- Sys.getenv("STATE_TAG", "final")
SIMF <- Sys.getenv("SIM_FILE", "event_similarity_all.parquet")

st <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
st[, athlete_id := as.character(athlete_id)]
stopifnot("state has no R_ceil" = "R_ceil" %in% names(st))
h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("athlete_id", "event_id", "date")))
h[, athlete_id := as.character(athlete_id)]
wa <- setDT(read_parquet(file.path(D, "athlete_wa_rankings.parquet")))
wa <- wa[is.finite(wa_place) & !grepl("Overall", event_group)]

# mirrors check_ceil_ranking.R - same mapping, same filter, so the numbers are
# comparable to the ones R_ceil was adopted on
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
stopifnot("WA mapping produced no usable events" = uniqueN(wa$event_id) > 10)

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
cat(sprintf("state %s | as at %s | active %s of %s athlete-events | WA covers %d events\n",
            TAG, ASOF, format(nrow(act), big.mark = ","),
            format(nrow(st), big.mark = ","), uniqueN(wa$event_id)))

# z within event over the ACTIVE pool - the same population the page ranks
act[, z := (R_ceil - mean(R_ceil)) / stats::sd(R_ceil), by = event_id]
act <- act[is.finite(z)]

sim <- setDT(read_parquet(file.path(D, SIMF)))
scol <- if ("cor_use" %chin% names(sim)) "cor_use" else "cor"
sim[, corv := as.numeric(get(scol))]
cat(sprintf("similarity: %s | %d pairs | column %s\n", SIMF, nrow(sim), scol))
# make it symmetric so a single join finds both directions
sim2 <- rbindlist(list(sim[, .(ev = e1, sv = e2, corv)],
                       sim[, .(ev = e2, sv = e1, corv)]))

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
  data.table(config = lab, events = nrow(res),
             `precision@10` = round(100 * sum(res$hits) / sum(res$wa_n), 1),
             `WA #1 shown` = round(100 * mean(res$wa1), 1))
}

# MAXN caps WHO may borrow. The blend costs precision overall while clearly
# helping thin records (Almgren, n_eff 3.38, moved 16th -> 9th on his 5000m
# form), so the suspicion is that it helps the few and taxes the many. An
# athlete with a deep record in the event has nothing to learn from a sibling.
blend <- function(MINCOR, XB, NSIB, MAXN = Inf) {
  s <- sim2[corv >= MINCOR]
  if (!nrow(s)) return(NULL)
  # sibling z for every athlete, weighted by cor^2 x the evidence behind it
  j <- merge(act[, .(athlete_id, ev = event_id, n_eff, z)],
             s, by = "ev", allow.cartesian = TRUE)
  j <- merge(j, act[, .(athlete_id, sv = event_id, z_sib = z, ne_sib = n_eff)],
             by = c("athlete_id", "sv"))
  if (!nrow(j)) return(NULL)
  setorder(j, athlete_id, ev, -corv)
  j <- j[, head(.SD, NSIB), by = .(athlete_id, ev)]
  j[, wt := corv^2 * ne_sib]
  agg <- j[, .(z_borrow = sum(wt * z_sib) / sum(wt), sibs = .N), by = .(athlete_id, ev)]
  out <- merge(act, agg, by.x = c("athlete_id", "event_id"),
               by.y = c("athlete_id", "ev"), all.x = TRUE)
  # weight on the borrowed value falls with the athlete's OWN evidence
  out[, w := fifelse(is.finite(z_borrow) & n_eff <= MAXN, XB / (n_eff + XB), 0)]
  out[!is.finite(z_borrow), z_borrow := 0]
  out[, z_blend := (1 - w) * z + w * z_borrow]
  out
}

cat("\n=== agreement with World Athletics ===\n")
rows <- list(score(act, "z", sprintf("no blend (R_ceil, %s)", TAG)))
grid <- CJ(MINCOR = c(0.30, 0.50), XB = c(0.5, 1, 2), NSIB = 6L,
           MAXN = c(Inf, 8, 5, 3))
for (i in seq_len(nrow(grid))) {
  b <- blend(grid$MINCOR[i], grid$XB[i], grid$NSIB[i], grid$MAXN[i])
  if (is.null(b)) next
  rows[[length(rows) + 1L]] <- score(b, "z_blend",
    sprintf("mincor %.2f | xb %.1f | maxn %s", grid$MINCOR[i], grid$XB[i],
            ifelse(is.finite(grid$MAXN[i]), as.character(grid$MAXN[i]), "all")))
}
print(rbindlist(rows))
cat("\nThe first row is what is published today. A configuration only matters if\n")
cat("it beats that, on a referee that took no part in choosing it.\n")

# The named cases this was built for.
cat("\n=== the athletes that prompted this ===\n")
# the configuration that ties the published precision@10 while still borrowing
# for thin records: mincor 0.30, xb 1.0, and only athletes with n_eff <= 8
b <- blend(0.30, 1, 6L, 8)
d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
nm <- unique(d[, .(athlete_id = as.character(athlete_id), athlete_name)])
for (tab in list(list(t = act, k = "z", lab = "published"),
                 list(t = b,   k = "z_blend", lab = "blended 0.30/1/6, maxn 8"))) {
  x <- copy(tab$t); x[, .key := get(tab$k)]
  setorder(x, event_id, -.key); x[, rk := seq_len(.N), by = event_id]
  x <- merge(x, nm, by = "athlete_id")
  cat(sprintf("\n-- %s --\n", tab$lab))
  print(x[athlete_name %like% "Barega|Almgren" &
          event_id %chin% c("AT-10000Metres-M", "AT-5000Metres-M"),
          .(athlete_name, event_id, rank = rk, n_eff = round(n_eff, 2),
            z = round(.key, 3))][order(event_id, rank)])
}
