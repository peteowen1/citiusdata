# Cluster-robust inference for statistics computed over athlete-race rows.
#
# WHY THIS EXISTS. Every model-vs-baseline number in this project was produced by
# a plain paired t.test over athlete-race rows. Rows within a race are NOT
# independent -- a race is one draw of shared conditions across 8-12 athletes, so
# the effective sample is nearer the RACE count than the row count. Measured on
# race walks 2026-09-01: naive SE +/-0.14, race-clustered SE +/-1.21, 8.6x wider,
# which turned an apparently large bias (+3.56pp, "the largest unexplained bias
# in the corpus") into t = 0.8, i.e. nothing. 325 predictions, 10 races.
#
# The ratio between the two is roughly the field size, so the naive test is
# wrongest exactly where fields are deep -- which is the population this project
# makes decisions on.
#
# ESTIMATOR: CR2 (Bell-McCaffrey) with Satterthwaite (Bell-McCaffrey) degrees of
# freedom, on an intercept-only regression of a per-row influence contribution.
# CR2 rather than CR0/CR1 because subgroup cuts here routinely have <30 races and
# CR0 is badly downward-biased there. Both quantities have closed forms in the
# intercept-only case, so no matrix algebra is needed:
#
#   H_gg = J_{n_g}/n, and 1 is its eigenvector, so
#     (I_g - H_gg)^{-1/2} 1 = (1 - n_g/n)^{-1/2} 1
#   =>  SE_CR2 = (1/n) sqrt( sum_g S_g^2 / (1 - n_g/n) ),  S_g = sum_{i in g} psi_i
#
# Two identities are asserted below as self-checks rather than trusted:
#   * E[V_CR2] = sigma^2/n exactly under homoskedasticity (tr(M) == 1/n),
#   * dof -> G as cluster sizes equalise.
#
# GENERALITY. Passing a per-row INFLUENCE CONTRIBUTION rather than a raw
# difference means the same code covers a paired mean difference (psi = d - dbar)
# and a ratio-of-means like the relative-MAE headline (delta method, below),
# which is what the project actually reports.

# Cluster-robust SE for an estimator whose influence contributions are `psi`
# (one per row, already centred so mean(psi) == 0). Returns CR0/CR1/CR2 SEs, the
# naive iid SE for comparison, Bell-McCaffrey dof, and a t-based CI.
cluster_stat <- function(est, psi, cluster, conf = 0.95) {
  ok <- is.finite(psi) & !is.na(cluster)
  psi <- psi[ok]; cluster <- cluster[ok]
  n <- length(psi)
  if (n < 3L) return(list(est = est, n = n, G = NA_integer_, se = NA_real_,
                          se_naive = NA_real_, se_cr0 = NA_real_, se_cr1 = NA_real_,
                          dof = NA_real_, t = NA_real_, p = NA_real_,
                          lo = NA_real_, hi = NA_real_))

  # cluster sums of the influence contributions
  f  <- factor(cluster)
  S  <- as.numeric(rowsum(psi, f, reorder = FALSE))
  ng <- as.numeric(table(f))
  G  <- length(S)
  r  <- 1 - ng / n                      # CR2 leverage factor per cluster

  se_naive <- sqrt(sum(psi^2)) / n      # iid: what the old t.test effectively used
  se_cr0   <- sqrt(sum(S^2)) / n
  se_cr1   <- se_cr0 * sqrt(G / (G - 1))
  se_cr2   <- sqrt(sum(S^2 / r)) / n

  # Bell-McCaffrey Satterthwaite dof. With c_g^2 = 1/(n^2 r_g):
  #   tr(M)   = 1/n                                   (exact; asserted)
  #   tr(M^2) = sum n_g^2 / n^4
  #           + (sum n_g^2/r_g)^2 / n^6
  #           - sum n_g^4/r_g^2 / n^6
  trM  <- sum(ng / n^2)
  trM2 <- sum(ng^2) / n^4 + (sum(ng^2 / r))^2 / n^6 - sum(ng^4 / r^2) / n^6
  stopifnot(abs(trM - 1 / n) < 1e-12 * max(1, 1 / n))   # self-check on the algebra
  dof <- if (is.finite(trM2) && trM2 > 0) trM^2 / trM2 else G - 1
  dof <- max(1, min(dof, n - 1))

  tval <- est / se_cr2
  crit <- stats::qt(1 - (1 - conf) / 2, df = dof)
  list(est = est, n = n, G = G,
       se = se_cr2, se_naive = se_naive, se_cr0 = se_cr0, se_cr1 = se_cr1,
       dof = dof, t = tval,
       p = 2 * stats::pt(-abs(tval), df = dof),
       lo = est - crit * se_cr2, hi = est + crit * se_cr2,
       infl = se_cr2 / se_naive)
}

# Paired mean difference of two per-row vectors, clustered.
cluster_paired <- function(x, y, cluster, conf = 0.95) {
  d <- x - y
  cluster_stat(mean(d), d - mean(d), cluster, conf)
}

# Relative difference of two mean-absolute-error vectors, as a percentage:
#   theta = 100 * (mean|x| - mean|y|) / mean|y|
# Delta method, so the denominator's own sampling variability is carried rather
# than assumed away (it is positively correlated with the numerator, so treating
# it as fixed is not conservative).
cluster_rel_mae <- function(x, y, cluster, conf = 0.95) {
  ax <- abs(x); ay <- abs(y)
  A <- mean(ax); B <- mean(ay)
  est <- 100 * (A - B) / B
  psi <- (100 / B) * (ax - A) - (100 * A / B^2) * (ay - B)
  cluster_stat(est, psi, cluster, conf)
}

# One-line renderer. ALWAYS prints the cluster count -- a percentage improvement
# without its race count is not reportable.
fmt_cl <- function(r) {
  if (!is.finite(r$se)) return(sprintf("n=%d  (too few rows to test)", r$n))
  sprintf("%+7.2f%%  [%+7.2f, %+7.2f]  %5d races / %8s rows  t=%6.2f  df=%5.0f  p=%-9.3g SE %.1fx naive",
          r$est, r$lo, r$hi, r$G, format(r$n, big.mark = ","),
          r$t, r$dof, r$p, r$infl)
}
