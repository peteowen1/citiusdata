# Fit per-family SCALE factors for the two spread terms from hold-out forecast
# residuals, and attach them to a calibration.
#
# WHY (2026-09-06). With the context-conditional condition_sd in place the 50%
# interval still covers 56% of T1-final marks pooled, and by family the
# picture is a mix: throws over-corrected (24%), middle distance untouched
# (76%), jumps too narrow (43%). The fitted race effects and the forecast
# residuals disagree, per family, on how much a final shares and how much an
# athlete varies. The forecast residual is the quantity the simulation has to
# match, so the last step is a per-family multiplier on each term fitted from
# those residuals -- on a FIT window, validated on two others.
#
#   k_shared^2 = (var of race-mean residual - individual noise / field) / mean(cond_sd^2)
#   k_indiv^2  = (var of within-race residual - ability_se^2 - form_sd^2) / mean(sigma^2 * t-inflation)
#
# Both come from pit_coverage_check.R's per-row output (CITIUS_PIT_COND_CONTEXT=1
# so cond_sd is the context cell). Clamped to [0.5, 2] and shrunk toward 1
# with a pseudo-count of races, because a family with 20 finals in a
# half-season cannot support a free factor.
#
# Usage:
#   Rscript citiusdata/scripts/fit_spread_scales.R
# Env:
#   CITIUS_SCALES_ROWS  rows CSV from the fit-window PIT run
#                       (default pit_coverage_rows_finals_2023_ctx.csv)
#   CITIUS_SCALES_SRC   calibration to attach to (default ..._ctxsd.rds)
#   CITIUS_SCALES_OUT   output calibration (default ..._ctxsd_scaled.rds)
#   CITIUS_SCALES_PSEUDO  pseudo-races for shrinkage toward 1 (default 20)
suppressMessages(library(data.table))
OUT  <- here::here("citiusdata", "data")
ROWS <- Sys.getenv("CITIUS_SCALES_ROWS", "pit_coverage_rows_finals_2022_ctx.csv,pit_coverage_rows_finals_2023_ctx.csv,pit_coverage_rows_finals_2025_ctx.csv")
SRC  <- Sys.getenv("CITIUS_SCALES_SRC", "calibration_corpus_wac_coast_0904_ctxsd.rds")
DST  <- Sys.getenv("CITIUS_SCALES_OUT", "calibration_corpus_wac_coast_0904_ctxsd_scaled.rds")
M    <- as.numeric(Sys.getenv("CITIUS_SCALES_PSEUDO", "20"))
say <- function(...) cat(sprintf(...), "\n", sep = "")
stopifnot(DST != SRC)

# Several fit-window files may be given, comma-separated, and are pooled.
d <- rbindlist(lapply(trimws(strsplit(ROWS, ",")[[1]]), function(r) fread(file.path(OUT, r))), fill = TRUE)
req <- c("family", "race_key", "resid", "race_mean_resid", "indiv_resid", "sigma", "ability_se", "cond_sd", "tail_df", "form_sd")
stopifnot(all(req %in% names(d)))
d <- d[is.finite(resid) & is.finite(sigma) & is.finite(cond_sd)]
d[, n_field := .N, by = race_key]
tvar <- function(df) ifelse(is.finite(df) & df > 2, df / (df - 2), 1)
# The mark distribution is drawn from sigma_marks when the ability table carries
# it (2026-09-06 package change), so that is the term k_indiv must scale.
if ("sigma_marks" %in% names(d)) d[is.finite(sigma_marks), sigma := sigma_marks]
d[, sigma2_t := sigma^2 * tvar(tail_df)]
d[, se2 := ifelse(is.finite(ability_se), ability_se^2, 0)]
d[, form2 := ifelse(is.finite(form_sd), form_sd^2, 0)]
say("%s rows, %s races, %d families from %s", format(nrow(d), big.mark = ","), format(uniqueN(d$race_key), big.mark = ","), uniqueN(d$family), ROWS)

# Individual term first: its variance leaks into every race mean as var/n.
ind <- d[, .(n = .N, obs_indiv_var = var(indiv_resid), sigma2_t = mean(sigma2_t), se2 = mean(se2), form2 = mean(form2)),
         by = family]
ind[, k_indiv2_raw := (obs_indiv_var - se2 - form2) / sigma2_t]
races <- unique(d[, .(race_key, family, race_mean_resid, n_field, indiv_pred = sigma2_t + se2 + form2)], by = "race_key")
sh <- merge(races[, .(n_races = .N, obs_shared_var = var(race_mean_resid), leak = mean(indiv_pred / n_field)), by = family],
            d[, .(cond2 = mean(cond_sd^2)), by = family], by = "family")
sh[, k_shared2_raw := pmax(obs_shared_var - leak, 0) / cond2]

sc <- merge(ind[, .(family, n, k_indiv2_raw)], sh[, .(family, n_races, k_shared2_raw, obs_shared_var, leak, cond2)], by = "family")
clamp <- function(x) pmin(pmax(x, 0.1), 4)      # on the variance scale: sd factor in [0.32, 2]
sc[, k_indiv2  := (n_races * clamp(k_indiv2_raw)  + M * 1) / (n_races + M)]
sc[, k_shared2 := (n_races * clamp(k_shared2_raw) + M * 1) / (n_races + M)]
sc[, `:=`(k_indiv = sqrt(k_indiv2), k_shared = sqrt(k_shared2))]
setorder(sc, -n)
cat("\n=== per-family spread scales (sd multipliers; 1 = the term is already right) ===\n")
print(sc[, .(family, n, n_races, k_shared_raw = round(sqrt(clamp(k_shared2_raw)), 3), k_shared = round(k_shared, 3),
             k_indiv_raw = round(sqrt(clamp(k_indiv2_raw)), 3), k_indiv = round(k_indiv, 3))])

cal <- readRDS(file.path(OUT, SRC))
stopifnot(inherits(cal, "citius_calibration"))
cal$spread_scales <- sc[, .(family, k_shared, k_indiv, n_races, n_rows = n)]
cal$provenance$spread_scales <- list(rows = ROWS, src = SRC, pseudo_races = M, built = Sys.time())
saveRDS(cal, file.path(OUT, DST))
fwrite(cal$spread_scales, file.path(OUT, "spread_scales.csv"))
say("wrote %s (spread_scales on %d families) and spread_scales.csv", DST, nrow(sc))
