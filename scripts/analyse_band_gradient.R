# Is the subjectivity gradient real, or is it the size gradient wearing a hat?
#
# The four bands give a perfectly monotone rise in gold gained per 100
# available: measured 4.11, refereed 4.98, mixed 5.81, judged 7.81. That looks
# compelling and two things need checking before it is believed.
#
#   1. HOW MUCH INFORMATION IS IN "MONOTONE"? With four bands there are 24
#      orderings and 2 of them are monotone, so a random ordering comes out
#      monotone 8.3% of the time. A clean gradient across four points is worth
#      about p = 0.08 on its own -- close to what the FULL regression on the
#      continuous score reports (subjectivity + team + log size + baseline).
#      That comparison is computed live below rather than quoted, because an
#      earlier version of this comment hardcoded a p-value that went stale.
#      Note it is the full model that lands near 0.08; subjectivity ALONE is
#      p = 0.003, and the difference between those two is the whole point.
#
#   2. IS IT SIZE? Small sports gain more per gold available, and the two
#      gradients are collinear. The test that separates them is whether the
#      subjectivity gradient survives WITHIN size strata.
#
#      Mean sport size per sport-edition, printed below: measured is the LARGE
#      band (~17 golds), judged is mid (~10), and refereed is the smallest
#      (~5). An earlier version of this comment said "judged 3.7, measured 8.0"
#      -- those divided a per-edition gold total by a sport count and are not
#      mean sizes. The real ordering matters: the smallest band has nearly the
#      lowest gain rate, which cuts against a pure size explanation.

library(data.table)
DATA <- "C:/dev/citiusverse/citiusdata/data"
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))

panel <- as.data.table(readRDS(file.path(DATA, "host_sport_panel.rds")))
panel[, golds_gained    := (host_share - base_share) / 100 * sport_golds]
panel[, subj_band := cut(subjectivity, c(-Inf, 0.10, 0.25, 0.50, Inf),
        labels = c("measured", "refereed", "mixed", "judged"))]
panel[, size_band := cut(sport_golds, c(0, 6, 14, Inf),
        labels = c("small (1-6)", "medium (7-14)", "large (15+)"))]
panel[, edition := paste(games, year)]

rate <- function(d) 100 * sum(d$golds_gained) / sum(d$sport_golds)

cat("================ THE GRADIENT ================\n")
obs <- panel[, .(sports = uniqueN(sport), available = round(sum(sport_golds)),
                 per100 = round(rate(.SD), 2)), by = subj_band,
             .SDcols = c("golds_gained", "sport_golds")]
setorder(obs, subj_band)
print(obs)

# --- how much is "monotone" worth on its own? ------------------------------
cat("\n--- what a monotone ordering of four bands is worth ---\n")
cat(sprintf("  orderings of 4 bands: %d; monotone ones: 2; by chance: %.1f%%\n",
            factorial(4), 100 * 2 / factorial(4)))
# Derived, not quoted. The regression is refitted here on the same panel so the
# comparison cannot drift out of date the way a hardcoded p-value did.
cluster_se <- function(fit, cl) {
  X <- model.matrix(fit); u <- residuals(fit)
  w <- weights(fit); if (is.null(w)) w <- rep(1, length(u))
  bread <- solve(t(X) %*% (X * w)); meat <- matrix(0, ncol(X), ncol(X))
  for (k in unique(cl)) {
    i <- which(cl == k); s <- t(X[i, , drop = FALSE]) %*% (u[i] * w[i])
    meat <- meat + s %*% t(s)
  }
  G <- length(unique(cl)); N <- nrow(X); K <- ncol(X)
  sqrt(diag(bread %*% meat %*% bread) * (G / (G - 1)) * ((N - 1) / (N - K)))
}
pval <- function(form) {
  dd <- copy(panel); dd[, .wt := sport_golds]
  f <- lm(form, data = dd, weights = .wt)
  cl <- dd$edition; se <- cluster_se(f, cl)
  names(se) <- names(coef(f))
  t <- coef(f)[["subjectivity"]] / se[["subjectivity"]]
  2 * pt(abs(t), df = length(unique(cl)) - 1, lower.tail = FALSE)
}
p_full  <- pval(effect_pp ~ subjectivity + team_sport + log(sport_golds) + base_share)
p_alone <- pval(effect_pp ~ subjectivity)
cat(sprintf("  the shape alone carries p ~ %.3f\n", 2 / factorial(4)))
cat(sprintf("  full regression on the continuous score: p = %.4f\n", p_full))
cat(sprintf("  subjectivity ALONE, no controls:         p = %.4f\n", p_alone))
cat("  The shape adds nothing the full model did not already say.\n")

# --- bootstrap the rates, resampling HOST EDITIONS -------------------------
# Sports within an edition share that nation's form, so the edition is the
# independent unit, not the sport.
set.seed(20260803)
eds <- unique(panel$edition)
B <- 4000
boot <- matrix(NA_real_, B, nlevels(panel$subj_band),
               dimnames = list(NULL, levels(panel$subj_band)))
mono <- logical(B)
for (b in seq_len(B)) {
  pick <- sample(eds, length(eds), replace = TRUE)
  d <- panel[.(pick), on = "edition", allow.cartesian = TRUE]
  r <- d[, .(v = rate(.SD)), by = subj_band, .SDcols = c("golds_gained","sport_golds")]
  setorder(r, subj_band)
  boot[b, as.character(r$subj_band)] <- r$v
  v <- boot[b, ]
  mono[b] <- !anyNA(v) && all(diff(v) > 0)
}
cat("\n--- bootstrap over host editions (4000 resamples) ---\n")
ci <- t(apply(boot, 2, quantile, c(0.025, 0.5, 0.975), na.rm = TRUE))
print(round(ci, 2))
cat(sprintf("\n  the gradient stays perfectly monotone in %.0f%% of resamples\n",
            100 * mean(mono, na.rm = TRUE)))
gap <- boot[, "judged"] - boot[, "measured"]
cat(sprintf("  judged minus measured: %.2f (95%% CI %.2f to %.2f), > 0 in %.0f%%\n",
            mean(gap, na.rm = TRUE), quantile(gap, 0.025, na.rm = TRUE),
            quantile(gap, 0.975, na.rm = TRUE), 100 * mean(gap > 0, na.rm = TRUE)))

# --- does it survive within size strata? -----------------------------------
cat("\n\n================ IS IT JUST SIZE? ================\n")
cat("\nAverage sport size by subjectivity band (golds per sport-edition):\n")
print(panel[, .(mean_size = round(mean(sport_golds), 1),
                median_size = as.double(median(sport_golds))), by = subj_band][order(subj_band)])

cat("\nGain per 100 available, subjectivity band WITHIN size stratum:\n")
tab <- dcast(panel[, .(v = round(rate(.SD), 2)), by = .(size_band, subj_band),
                   .SDcols = c("golds_gained", "sport_golds")],
             size_band ~ subj_band, value.var = "v")
print(tab)
cat("\nSame cells, golds available (blank cells are combinations that barely exist):\n")
print(dcast(panel[, .(g = round(sum(sport_golds) / uniqueN(panel$edition), 1)),
                  by = .(size_band, subj_band)], size_band ~ subj_band, value.var = "g"))

cat("\nMonotone within each size stratum?\n")
for (s in levels(panel$size_band)) {
  v <- unlist(tab[size_band == s, -1])
  v <- v[!is.na(v)]
  cat(sprintf("  %-14s %s  -> %s\n", s,
              paste(sprintf("%.2f", v), collapse = " "),
              if (length(v) >= 3 && all(diff(v) > 0)) "monotone" else "NOT monotone"))
}

# --- and the size gradient for comparison ----------------------------------
cat("\nThe size gradient, for comparison:\n")
print(panel[, .(sports = uniqueN(sport), available = round(sum(sport_golds)),
                per100 = round(rate(.SD), 2)), by = size_band,
            .SDcols = c("golds_gained","sport_golds")][order(size_band)])

# --- drop the two sports carrying the judged band --------------------------
cat("\n--- the judged band without boxing and gymnastics ---\n")
nb <- panel[!(sport %in% c("Boxing", "Gymnastics"))]
print(nb[, .(sports = uniqueN(sport), available = round(sum(sport_golds)),
             per100 = round(rate(.SD), 2)), by = subj_band,
         .SDcols = c("golds_gained","sport_golds")][order(subj_band)])

saveRDS(list(observed = obs, boot_ci = ci, mono_frac = mean(mono, na.rm = TRUE),
             within_size = tab),
        file.path(DATA, "band_gradient.rds"))
cat("\nSaved band_gradient.rds\n")
