# Gold and medal LOG-LOSS, paired per race, between two arm caches -- the metric
# the 2026-09-06 sigma-scale rejection stood on (Brier tied, log-loss +1.49% on
# T1 medals). quick_compare.R prints Brier only, so a spread change that wins
# on Brier can still be the same failure. Lower is better; negative diff = B
# better.
#   CITIUS_LL_A=bt_cache_ss_ctrl CITIUS_LL_B=bt_cache_ss_08 Rscript citiusdata/scripts/diagnostics/compare_logloss_arms.R
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
A <- Sys.getenv("CITIUS_LL_A", "bt_cache_ss_ctrl"); B <- Sys.getenv("CITIUS_LL_B", "bt_cache_ss_08")
load_rows <- function(cache) {
  fs <- setdiff(list.files(file.path(OUT, cache), pattern = "\\.rds$"), "_arm.rds")
  rbindlist(lapply(fs, function(f) {
    o <- readRDS(file.path(OUT, cache, f)); if (!length(o)) return(NULL)
    rbindlist(lapply(o, function(r) {
      if (is.null(r$pred) || is.null(r$outc)) return(NULL)
      p <- as.data.table(r$pred)[, .(race_id, athlete_id = as.character(athlete_id), p_gold, p_medal)]
      oc <- as.data.table(r$outc)[, .(race_id, athlete_id = as.character(athlete_id), hit, hit_medal)]
      merge(p, oc, by = c("race_id", "athlete_id"))
    }), fill = TRUE)
  }), fill = TRUE)
}
a <- load_rows(A); b <- load_rows(B)
common <- intersect(unique(a$race_id), unique(b$race_id))
a <- a[race_id %in% common]; b <- b[race_id %in% common]
cat(sprintf("%s vs %s: %d races in common (%s / %s athlete-rows)\n", A, B, length(common), format(nrow(a), big.mark = ","), format(nrow(b), big.mark = ",")))
ll <- function(p, y) -(y * log(pmax(p, 1e-9)) + (1 - y) * log(pmax(1 - p, 1e-9)))
per_race <- function(d, pcol, ycol) d[, .(ll = mean(ll(get(pcol), get(ycol)))), by = race_id]
br <- function(p, y) (p - y)^2
per_race <- function(d, pcol, ycol) d[, .(ll = mean(ll(get(pcol), get(ycol))), br = mean(br(get(pcol), get(ycol)))), by = race_id]
# family via the race key (the cache rows carry no event_id)
ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))[, .(race_id = as.character(race_key), event_id)]
ch <- unique(ch, by = "race_id")
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
fam <- as.data.table(citius_events())[, .(event_id, family)]
ch <- merge(ch, fam, by = "event_id", all.x = TRUE)
line <- function(lbl, m, col) {
  d <- m[[paste0(col, "_b")]] - m[[paste0(col, "_a")]]; base <- mean(m[[paste0(col, "_a")]])
  tt <- if (length(d) > 2) t.test(d) else list(statistic = NA, p.value = NA)
  sprintf("%-22s %-8s A %.5f  B %.5f | %+.2f%%  t = %+.2f  p = %.3g  n = %d",
          lbl, col, base, mean(m[[paste0(col, "_b")]]), 100 * mean(d) / base, tt$statistic, tt$p.value, nrow(m))
}
cat("\nLower is better for both; negative % = B better. t-test is paired per race.\n")
for (w in list(c("p_gold", "hit", "gold"), c("p_medal", "hit_medal", "medal"))) {
  ra <- per_race(a, w[1], w[2]); rb <- per_race(b, w[1], w[2])
  m <- merge(ra, rb, by = "race_id", suffixes = c("_a", "_b"))
  m <- merge(m, ch[, .(race_id, family)], by = "race_id", all.x = TRUE)
  cat(sprintf("\n== %s ==\n", toupper(w[3])))
  cat(line("all races", m, "br"), "\n"); cat(line("all races", m, "ll"), "\n")
  for (f in m[, .N, by = family][order(-N)]$family) {
    mf <- m[family %in% f]; if (nrow(mf) < 8) next
    cat(line(sprintf("  %s", f), mf, "br"), "\n"); cat(line(sprintf("  %s", f), mf, "ll"), "\n")
  }
}
