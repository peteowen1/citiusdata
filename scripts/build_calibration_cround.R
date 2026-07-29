# Calibration for the `cround` arm: corpus + wind (both confirmed) + per-family
# context offsets under the FITTED shrinkage.
#
# This isolates what `cstack` could not. cstack bundled wind, indoor and
# per-family round AND tier offsets, then lost to wind-only -- so the failure
# could not be attributed. The fitted shrinkage now sends tier offsets to pooled
# on its own (k=Inf, 16-20% better out of sample) and keeps round offsets raw
# (k=0), leaving per-family ROUND as the single untested change against cwind.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
OUT <- here::here("citiusdata", "data")

cal <- readRDS(file.path(OUT, "calibration_corpus_w.rds"))
x <- flag_implausible(setDT(readRDS(file.path(OUT, "athletics_corpus.rds"))))

ctx <- estimate_context_effects(x)
cal$round_family <- ctx$round_family
cal$tier_family  <- ctx$tier_family

cat(sprintf("round shrink k : %s\n", format(ctx$round_family$shrink_k[1])))
cat(sprintf("tier  shrink k : %s\n\n", format(ctx$tier_family$shrink_k[1])))

show <- function(tbl, cls, col, pooled) {
  t <- as.data.table(tbl)[get(col) == cls]
  if (!nrow(t)) return(invisible())
  t[, `:=`(w = round(n / (n + shrink_k), 3),
           raw_pct = round(100 * (exp(raw) - 1), 3),
           used_pct = round(100 * (exp(offset) - 1), 3))]
  cat(sprintf("--- %s = %s (pooled %.3f%%) ---\n", col, cls, 100 * (exp(pooled) - 1)))
  print(t[, .(family, n, w, raw_pct, used_pct)][order(raw_pct)])
}
show(ctx$round_family, "heat", "round_class", ctx$round["heat"])
cat("\n")
show(ctx$tier_family, "low", "tier_class", ctx$tier["low"])

saveRDS(cal, file.path(OUT, "calibration_corpus_cround.rds"))
cat("\nwrote calibration_corpus_cround.rds\n")
