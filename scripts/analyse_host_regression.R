# Host effect at SPORT level, regressed on a continuous subjectivity score.
#
# The three-bucket version of this asked "is the judged bucket different from
# the measured bucket". That throws away most of the information and cannot
# place football, where nobody scores the performance but a referee decides
# matches. Here every host-edition x sport is one observation, the predictor is
# continuous, and the two components of the score enter separately so a reader
# can reject one without rejecting the analysis.
#
# Three things are controlled for, and each is there because it would otherwise
# be confounded with subjectivity:
#
#  1. SPORT SIZE. Judged sports are small. A share out of 14 golds moves 7
#     points per gold; out of 59 it moves 1.7. That inflates variance, and if
#     the effect were driven by variance alone we would see it load on size
#     rather than on subjectivity.
#  2. TEAM SPORTS. One team per nation in a field of twelve is a completely
#     different lottery from three athletes in a field of forty, and team sports
#     also have high officiating scope -- so without this term the two are
#     entangled.
#  3. BASELINE SHARE. A nation already taking 40% of a sport's golds has less
#     room to gain than one taking 4%.

library(data.table)
DATA <- here::here("citiusdata", "data")
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
source(here::here("citiusdata", "scripts", "games_reference.R"))

# Use the table WITH team-sport podiums folded in. Team sports have no NOC
# medal table on Wikipedia, so without this only two of them reach the
# regression and the team coefficient rests on almost nothing.
SPORT_FILE <- if (file.exists(file.path(DATA, "sport_medal_tables_with_podiums.rds"))) {
  "sport_medal_tables_with_podiums.rds"
} else "sport_medal_tables.rds"
cat("sport table:", SPORT_FILE, "\n")
sp  <- as.data.table(readRDS(file.path(DATA, SPORT_FILE)))
med <- as.data.table(readRDS(file.path(DATA, "multisport_medal_tables.rds")))
sp[, canon := canonical_nation(nation)]
med[, canon := canonical_nation(nation)]
sp[, sport_fam := sport_family(sport)]

# Group to families so a renaming between editions is not read as a sport
# appearing and disappearing (Cycling -> Track cycling, Gymnastics -> Artistic).
spf <- sp[, .(gold = sum(gold)), by = .(games, year, sport = sport_fam, canon)]
tot <- spf[, .(sport_golds = sum(gold)), by = .(games, year, sport)]
shr <- merge(spf, tot, by = c("games", "year", "sport"))
shr[, share := 100 * gold / sport_golds]

present <- unique(med[, .(games, year, canon)])
present[, was_there := TRUE]

hosts <- unique(data.table(games = sp$games, year = sp$year))
hosts[, host := unlist(lapply(paste0(games, "_", year),
   function(k) if (is.null(hosts_map[[k]])) NA_character_ else hosts_map[[k]]))]
hosts[, host_nat := host_nation(host)]
hosts <- hosts[!is.na(host_nat)]

# ---------------------------------------------------------------------------
# Build one row per host-edition x sport
# ---------------------------------------------------------------------------
rows <- list()
for (i in seq_len(nrow(hosts))) {
  g <- hosts$games[i]; y <- hosts$year[i]; n <- hosts$host_nat[i]
  eds <- sort(unique(shr[games == g, year])); pos <- match(y, eds)
  if (is.na(pos)) next
  nb <- eds[c(pos - 1, pos + 1)]; nb <- nb[!is.na(nb)]
  if (!length(nb)) next

  for (s in shr[games == g & year == y, unique(sport)]) {
    sg <- tot[games == g & year == y & sport == s, sport_golds]
    # No minimum. A `sport_golds >= 4` floor looks like prudence and is in fact
    # a filter on TEAM SPORTS: netball has one gold, football and hockey two.
    # It removed exactly the observations the podium harvest was built to add,
    # so the team coefficient stayed pinned to two sports.
    #
    # A one-gold sport gives a share of 0 or 100 and is very noisy -- that is
    # handled by weighting each row by its gold count, which is the right
    # precision weight, not by dropping the row. Model 7 below re-fits on
    # `sport_golds >= 4` to show the result does not depend on the small ones.
    if (!length(sg) || sg < 1) next
    at <- function(yy) {
      if (!nrow(tot[games == g & year == yy & sport == s])) return(NA_real_)
      if (!nrow(present[games == g & year == yy & canon == n])) return(NA_real_)
      v <- shr[games == g & year == yy & sport == s & canon == n, share]
      if (length(v)) v[1] else 0
    }
    h <- at(y); bs <- vapply(nb, at, numeric(1)); bs <- bs[!is.na(bs)]
    if (is.na(h) || !length(bs)) next
    rows[[length(rows) + 1]] <- data.table(
      games = g, year = y, nation = n, sport = s,
      sport_golds = sg, host_share = h, base_share = mean(bs),
      n_neighbours = length(bs), effect_pp = h - mean(bs))
  }
}
d <- rbindlist(rows)

subj <- sport_subjectivity()
d <- merge(d, subj[, .(sport, assessment_share, officiating_scope,
                       subjectivity, team_sport)], by = "sport", all.x = TRUE)

# --- entry lift, where participation data exists at host AND neighbour -------
# Hosts field bigger teams at home: 1.3x their usual share of the field from
# 1960 on. If the gold gain is just that, it should vanish once entry lift is
# in the model. This is the test that separates access from everything else.
part <- as.data.table(readRDS(file.path(DATA, "sport_participation.rds")))
ptot <- fread(file.path(DATA, "sport_participation_totals.csv"))
pq <- part[, .(n_listed = .N, n_counted = sum(!is.na(competitors)),
               athletes = sum(competitors, na.rm = TRUE)),
           by = .(games, year, sport)]
pq <- merge(pq, ptot[, .(games, year, sport, infobox_nations)],
            by = c("games", "year", "sport"), all.x = TRUE)
pq <- pq[n_counted == n_listed & athletes > 0 &
         !is.na(infobox_nations) & n_listed == infobox_nations]
pshare <- merge(part, pq[, .(games, year, sport, athletes)],
                by = c("games", "year", "sport"))
pshare[, sport := sport_family(sport)]
pshare <- pshare[, .(competitors = sum(competitors), athletes = sum(athletes)),
                 by = .(games, year, sport, canon)]
pshare[, entry_share := 100 * competitors / athletes]

entry_at <- function(g, y, s, n) {
  v <- pshare[games == g & year == y & sport == s & canon == n, entry_share]
  if (length(v)) v[1] else if (nrow(pshare[games == g & year == y & sport == s])) 0 else NA_real_
}
d[, entry_home := NA_real_][, entry_away := NA_real_]
for (i in seq_len(nrow(d))) {
  g <- d$games[i]; y <- d$year[i]; s <- d$sport[i]; n <- d$nation[i]
  eds <- sort(unique(pshare[games == g & sport == s, year])); pos <- match(y, eds)
  if (is.na(pos)) next
  nb <- eds[c(pos - 1, pos + 1)]; nb <- nb[!is.na(nb)]
  if (!length(nb)) next
  h <- entry_at(g, y, s, n)
  bs <- vapply(nb, function(yy) entry_at(g, yy, s, n), numeric(1))
  bs <- bs[!is.na(bs) & bs > 0]          # a zero away share means absent
  if (is.na(h) || !length(bs)) next
  d$entry_home[i] <- h; d$entry_away[i] <- mean(bs)
}
d[, entry_lift_pp := entry_home - entry_away]
cat(sprintf("\nrows with an entry lift measured: %d of %d (%d host editions)\n",
            sum(!is.na(d$entry_lift_pp)), nrow(d),
            uniqueN(d[!is.na(entry_lift_pp), .(games, year)])))

cat("=== sample ===\n")
cat(sprintf("host-edition x sport rows: %d\n", nrow(d)))
cat(sprintf("  with a subjectivity score: %d\n", sum(!is.na(d$subjectivity))))
cat("  unscored sports:\n")
print(d[is.na(subjectivity), .(rows = .N, golds = sum(sport_golds)), by = sport][order(-golds)])
d <- d[!is.na(subjectivity)]
cat(sprintf("host editions: %d, distinct sports: %d, series: %s\n",
            uniqueN(d[, .(games, year)]), uniqueN(d$sport),
            paste(unique(d$games), collapse = ", ")))

cat("\n=== raw picture: mean host gain by subjectivity band ===\n")
d[, band := cut(subjectivity, c(-Inf, 0.10, 0.20, 0.35, 0.60, Inf),
                labels = c("0.00-0.10", "0.10-0.20", "0.20-0.35",
                           "0.35-0.60", "0.60-1.00"))]
print(d[, .(sports = uniqueN(sport), n = .N,
            mean_golds = round(mean(sport_golds), 1),
            base = round(mean(base_share), 1),
            home = round(mean(host_share), 1),
            gain_pp = round(mean(effect_pp), 2)), by = band][order(band)])

cat("\n=== team vs individual, which Pete asked about directly ===\n")
print(d[, .(sports = uniqueN(sport), n = .N,
            median_golds = as.double(median(sport_golds)),
            mean_subjectivity = round(mean(subjectivity), 2),
            base = round(mean(base_share), 1),
            home = round(mean(host_share), 1),
            gain_pp = round(mean(effect_pp), 2)), by = team_sport][order(team_sport)])

# ---------------------------------------------------------------------------
# Regression, with SEs clustered on host edition
# ---------------------------------------------------------------------------
# Sports within one host edition share that nation's form, its funding cycle and
# its boycott status, so their residuals are not independent. Clustering on the
# host edition is the minimum needed for the standard errors to mean anything.
cluster_se <- function(fit, cl) {
  X <- model.matrix(fit)
  u <- residuals(fit)
  w <- weights(fit); if (is.null(w)) w <- rep(1, length(u))
  bread <- solve(t(X) %*% (X * w))
  meat <- matrix(0, ncol(X), ncol(X))
  for (k in unique(cl)) {
    idx <- which(cl == k)
    sk <- t(X[idx, , drop = FALSE]) %*% (u[idx] * w[idx])
    meat <- meat + sk %*% t(sk)
  }
  G <- length(unique(cl)); N <- nrow(X); K <- ncol(X)
  adj <- (G / (G - 1)) * ((N - 1) / (N - K))
  sqrt(diag(bread %*% meat %*% bread) * adj)
}

report <- function(label, form, data, wts = NULL) {
  # The weights vector goes in as a COLUMN. `lm(weights = wts)` looks `wts` up
  # in `data` before the calling frame, so a local vector of that name is not
  # found and the call fails.
  dd <- copy(data)
  fit <- if (is.null(wts)) {
    lm(form, data = dd)
  } else {
    dd[, .wt := wts]
    lm(form, data = dd, weights = .wt)
  }
  cl <- paste(dd$games, dd$year)
  co <- coef(fit)
  # A term with no variation in this subset gives an NA coefficient and makes
  # the cluster sandwich singular. Drop it and refit rather than returning a
  # table of NAs that looks like a result.
  if (anyNA(co)) {
    drop <- names(co)[is.na(co)]
    cat(sprintf("  [dropped, no variation in this subset: %s]\n",
                paste(drop, collapse = ", ")))
    # Use the TERM LABELS, not all.vars(): all.vars strips the transformation,
    # so `log(sport_golds)` would silently come back as `sport_golds` and the
    # refitted model would not be the one described.
    labs <- attr(terms(form), "term.labels")
    keep <- labs[!vapply(labs, function(l)
      any(grepl(l, gsub("TRUE$|FALSE$", "", drop), fixed = TRUE)), logical(1))]
    form <- reformulate(keep, response = all.vars(form)[1])
    fit <- if (is.null(wts)) lm(form, data = dd) else lm(form, data = dd, weights = .wt)
    co <- coef(fit)
  }
  se <- tryCatch(cluster_se(fit, cl), error = function(e) rep(NA_real_, length(co)))
  t <- co / se
  p <- 2 * pt(abs(t), df = length(unique(cl)) - 1, lower.tail = FALSE)
  cat("\n---", label, "---\n")
  print(data.table(term = names(co), est = round(co, 2), se = round(se, 2),
                   t = round(t, 2), p = signif(p, 3),
                   lo = round(co - 1.96 * se, 2), hi = round(co + 1.96 * se, 2)))
  cat(sprintf("n = %d, clusters = %d, adj R2 = %.3f\n",
              nrow(data), length(unique(cl)), summary(fit)$adj.r.squared))
  invisible(fit)
}

cat("\n\n================ REGRESSION ================\n")
cat("Outcome: host nation's gain in percentage points of that sport's golds.\n")
cat("Weighted by the sport's gold count -- a share out of 40 is far better\n")
cat("measured than a share out of 4. SEs clustered on host edition.\n")

report("1. subjectivity alone", effect_pp ~ subjectivity, d, d$sport_golds)
report("2. + sport size and baseline",
       effect_pp ~ subjectivity + log(sport_golds) + base_share, d, d$sport_golds)
report("3. + team sport (the full model)",
       effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share,
       d, d$sport_golds)
report("4. the two components entered separately",
       effect_pp ~ assessment_share + officiating_scope + team_sport +
         log(sport_golds) + base_share, d, d$sport_golds)
report("5. unweighted, as a robustness check",
       effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share, d)

cat("\n\n================ DOES ENTRY EXPLAIN IT? ================\n")
cat("Hosts field bigger teams at home. If the gold gain is only that, the\n")
cat("subjectivity coefficient should collapse once entry lift is controlled.\n")
de <- d[!is.na(entry_lift_pp)]
if (nrow(de) >= 30 && uniqueN(de[, .(games, year)]) >= 6) {
  cat(sprintf("\nsub-sample: %d rows, %d host editions\n",
              nrow(de), uniqueN(de[, .(games, year)])))
  report("A. same model on the entry sub-sample (no entry term)",
         effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share,
         de, de$sport_golds)
  report("B. entry lift added",
         effect_pp ~ subjectivity + entry_lift_pp + team_sport +
           log(sport_golds) + base_share, de, de$sport_golds)
  report("C. entry lift alone",
         effect_pp ~ entry_lift_pp + log(sport_golds) + base_share,
         de, de$sport_golds)
  cat("\nCompare the subjectivity coefficient in A and B: the share of it that\n")
  cat("survives is the part entry does NOT explain.\n")
} else {
  cat(sprintf("\nOnly %d rows across %d host editions have entry data at both the\n",
              nrow(de), uniqueN(de[, .(games, year)])))
  cat("host edition and a neighbour -- too thin to control for. Reported as a\n")
  cat("gap, not fudged.\n")
}

report("7. restricted to sports with >= 4 golds",
       effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share,
       d[sport_golds >= 4], d[sport_golds >= 4, sport_golds])

cat("\n\n================ TEAM SPORTS, DIRECTLY ================\n")
cat("Team sports have one or two golds, so a share is a blunt instrument. The\n")
cat("plainer question -- how often does the host actually WIN the thing at home\n")
cat("versus away -- needs no share at all.\n")
tm <- d[team_sport == TRUE]
if (nrow(tm) >= 10) {
  cat(sprintf("\n%d host-edition x team-sport rows, %d sports, %d host editions\n",
              nrow(tm), uniqueN(tm$sport), uniqueN(tm[, .(games, year)])))
  cat(sprintf("host's share of that sport's golds: %.1f%% at home vs %.1f%% away (%+.1f pp)\n",
              mean(tm$host_share), mean(tm$base_share),
              mean(tm$host_share) - mean(tm$base_share)))
  tt <- t.test(tm$host_share, tm$base_share, paired = TRUE)
  cat(sprintf("paired t: %+.2f pp (95%% CI %+.2f to %+.2f, p = %.4f)\n",
              mean(tm$effect_pp), tt$conf.int[1], tt$conf.int[2], tt$p.value))
  cat("\nby sport:\n")
  print(tm[, .(n = .N, golds = as.double(median(sport_golds)),
               away = round(mean(base_share), 1),
               home = round(mean(host_share), 1),
               gain = round(mean(effect_pp), 1)), by = sport][order(-n)])
  cat("\nSame comparison for individual sports, for contrast:\n")
  iv <- d[team_sport == FALSE]
  cat(sprintf("  individual: %.1f%% away -> %.1f%% home (%+.1f pp), n = %d\n",
              mean(iv$base_share), mean(iv$host_share),
              mean(iv$effect_pp), nrow(iv)))
  ttd <- t.test(tm$effect_pp, iv$effect_pp)
  cat(sprintf("  team minus individual: %+.2f pp (95%% CI %+.2f to %+.2f, p = %.4f)\n",
              mean(tm$effect_pp) - mean(iv$effect_pp),
              ttd$conf.int[1], ttd$conf.int[2], ttd$p.value))
}

cat("\n--- rank of subjectivity instead of its value ---\n")
d[, subj_rank := frank(subjectivity, ties.method = "dense")]
d[, subj_rank := subj_rank / max(subj_rank)]
report("6. subjectivity replaced by its normalised rank",
       effect_pp ~ subj_rank + team_sport + log(sport_golds) + base_share,
       d, d$sport_golds)

cat("\n--- leave-one-sport-out on the full model ---\n")
loo <- rbindlist(lapply(sort(unique(d$sport)), function(s) {
  dd <- d[sport != s]
  if (uniqueN(dd[, .(games, year)]) < 8) return(NULL)
  dd <- copy(dd); dd[, .wt := sport_golds]
  f <- lm(effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share,
          data = dd, weights = .wt)
  se <- tryCatch(cluster_se(f, paste(dd$games, dd$year)),
                 error = function(e) rep(NA_real_, length(coef(f))))
  # Index the SE by NAME. Positional indexing breaks silently the moment a term
  # is dropped for having no variation in the subset. The error handler returns
  # a full-length vector for the same reason: a scalar NA would make this
  # assignment throw and kill the whole script, which is how a singular fit in
  # one leave-one-out iteration would have taken the run down.
  if (length(se) != length(coef(f))) se <- rep(NA_real_, length(coef(f)))
  names(se) <- names(coef(f))
  data.table(dropped = s, coef = round(unname(coef(f)["subjectivity"]), 2),
             se = round(unname(se["subjectivity"]), 2))
}))
loo[, t := round(coef / se, 2)]
print(loo[order(coef)])
cat(sprintf("\ncoefficient range across leave-one-out: %.2f to %.2f\n",
            min(loo$coef, na.rm = TRUE), max(loo$coef, na.rm = TRUE)))

cat("\n--- by series ---\n")
for (g in unique(d$games)) {
  dd <- d[games == g]
  if (uniqueN(dd[, .(games, year)]) < 5) next
  report(paste("series:", g),
         effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share,
         dd, dd$sport_golds)
}

saveRDS(d, file.path(DATA, "host_sport_panel.rds"))
# Parquet twin: temp-then-rename, LOUD on failure -- see the note in
# harvest_athlete_histories.R; a quiet skip leaves two vintages on disk.
pq <- file.path(DATA, "host_sport_panel.parquet")
if (requireNamespace("arrow", quietly = TRUE)) {
  tryCatch({
    arrow::write_parquet(d, paste0(pq, ".tmp"))
    if (!file.rename(paste0(pq, ".tmp"), pq)) stop("rename over target failed")
  }, error = function(e) cli::cli_alert_danger(
    "parquet twin write FAILED ({conditionMessage(e)}); {basename(pq)} is STALE relative to the .rds"))
} else {
  cli::cli_alert_danger("arrow not installed -- {basename(pq)} NOT refreshed; it is stale relative to the .rds")
}
cat("\nSaved host_sport_panel.rds\n")
