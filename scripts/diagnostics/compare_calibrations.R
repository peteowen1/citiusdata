# How different are two calibrations, channel by channel? Written for the
# no-leak chain (2026-09-20): the deployed calibration against the same
# composition refitted with the scored competitions removed. If every channel
# is within noise here, an arm difference is sampling; if a channel moved,
# that is where the leak lived.
#   CITIUS_CC_A=<deployed.rds> CITIUS_CC_B=<noleak.rds> Rscript citiusdata/scripts/diagnostics/compare_calibrations.R
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
A <- Sys.getenv("CITIUS_CC_A", "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad.rds")
B <- Sys.getenv("CITIUS_CC_B", "calibration_corpus_wac_coast_0904_full2_altitude_banded_noroad_noleak.rds")
a <- readRDS(file.path(OUT, A)); b <- readRDS(file.path(OUT, B))
cat(sprintf("A = %s\nB = %s\n", A, B))
pr <- function(lbl, x, y) cat(sprintf("  %-28s A %10.5f  B %10.5f  diff %+9.5f (%+.2f%%)\n", lbl, x, y, y - x, 100 * (y - x) / x))

cat("\n== provenance ==\n")
cat(sprintf("  meets  A %d  B %d | excluded_scored: %s\n", a$provenance$n_meets, b$provenance$n_meets,
            if (is.null(b$provenance$excluded_scored)) "NONE (not a no-leak fit!)" else sprintf("%d competitions from %s", b$provenance$excluded_scored$competitions, b$provenance$excluded_scored$from)))

cat("\n== tier / round offsets (log scale) ==\n")
ta <- as.data.table(a$tier); tb <- as.data.table(b$tier)
m <- merge(ta, tb, by = "tier_class", suffixes = c("_a", "_b")); for (i in seq_len(nrow(m))) pr(paste("tier", m$tier_class[i]), m$offset_a[i], m$offset_b[i])
ra <- as.data.table(a$round); rb <- as.data.table(b$round)
m <- merge(ra, rb, by = "round_class", suffixes = c("_a", "_b")); for (i in seq_len(nrow(m))) pr(paste("round", m$round_class[i]), m$offset_a[i], m$offset_b[i])

cat("\n== events: sigma_within and condition_sd (median over events, and max |rel diff|) ==\n")
ea <- as.data.table(a$events)[, .(event_id, sw_a = sigma_within, cs_a = condition_sd)]
eb <- as.data.table(b$events)[, .(event_id, sw_b = sigma_within, cs_b = condition_sd)]
e <- merge(ea, eb, by = "event_id")
pr("sigma_within (median)", median(e$sw_a), median(e$sw_b)); pr("condition_sd (median)", median(e$cs_a), median(e$cs_b))
e[, `:=`(rsw = abs(sw_b / sw_a - 1), rcs = abs(cs_b / cs_a - 1))]
cat(sprintf("  max |rel diff| sigma_within %.4f (%s), condition_sd %.4f (%s)\n", max(e$rsw), e$event_id[which.max(e$rsw)], max(e$rcs), e$event_id[which.max(e$rcs)]))

cat("\n== sigma_context ratio by family ==\n")
sa <- as.data.table(a$sigma_context); sb <- as.data.table(b$sigma_context)
m <- merge(sa, sb, by = "family", suffixes = c("_a", "_b")); for (i in seq_len(nrow(m))) pr(m$family[i], m$ratio_a[i], m$ratio_b[i])

cat("\n== spread scales by family (k_shared / k_indiv) ==\n")
ka <- as.data.table(a$spread_scales); kb <- as.data.table(b$spread_scales)
m <- merge(ka, kb, by = "family", suffixes = c("_a", "_b"))
for (i in seq_len(nrow(m))) { pr(paste(m$family[i], "k_shared"), m$k_shared_a[i], m$k_shared_b[i]); pr(paste(m$family[i], "k_indiv"), m$k_indiv_a[i], m$k_indiv_b[i]) }

cat("\n== race shock ==\n")
pr("beta (overall)", a$race_shock$beta, b$race_shock$beta)
bt <- merge(as.data.table(a$race_shock$by_tier), as.data.table(b$race_shock$by_tier), by = "tier_class", suffixes = c("_a", "_b"))
for (i in seq_len(nrow(bt))) pr(paste("beta", bt$tier_class[i]), bt$beta_a[i], bt$beta_b[i])
cat(sprintf("  families A: %s | B: %s\n", paste(a$race_shock$families, collapse = ","), paste(b$race_shock$families, collapse = ",")))
cat(sprintf("  race table rows A %s  B %s (B should be fewer by the scored races)\n", format(nrow(a$race), big.mark = ","), format(nrow(b$race), big.mark = ",")))
cat(sprintf("  tail_df A %s B %s | altitude rows A %d B %d\n", a$tail_df, b$tail_df, NROW(a$altitude), NROW(b$altitude)))
