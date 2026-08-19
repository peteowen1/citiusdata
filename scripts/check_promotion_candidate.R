# What actually changes if you promote a calibration?
#
# `_deployed.R` points at the calibration BY FILENAME, so promotion is a
# one-line edit with nothing checking that the new artefact is the shape the
# package expects or that it was fitted on the corpus now deployed. This says
# what would change, and refuses to be reassuring about things it cannot see.
#
# Usage, from the verse root:
#   Rscript citiusdata/scripts/check_promotion_candidate.R <candidate.rds>
# defaults to the keyfix chain.
suppressMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
OUT <- here::here("citiusdata", "data")
CAND <- if (length(args)) args[1] else "calibration_corpus_csigma_coast_keyfix.rds"

# The currently deployed pointer, read from _deployed.R rather than retyped.
dep_src <- readLines(here::here("citiusdata", "scripts", "_deployed.R"), warn = FALSE)
cur <- sub('.*calibration\\s*=\\s*"([^"]+)".*', "\\1",
           grep('^\\s*calibration\\s*=', dep_src, value = TRUE)[1])
stamp <- sub('.*stamp\\s*=\\s*"([^"]+)".*', "\\1",
             grep('^\\s*stamp\\s*=', dep_src, value = TRUE)[1])
cat(sprintf("deployed stamp : %s\ndeployed cal   : %s\ncandidate      : %s\n\n",
            stamp, cur, CAND))
a <- readRDS(file.path(OUT, cur)); b <- readRDS(file.path(OUT, CAND))

# 1. Does the candidate have every layer the deployed one has? A missing layer
#    is silently dropped by estimate_ability(), which is how six adjustments
#    once sat inert.
miss <- setdiff(names(a), names(b)); extra <- setdiff(names(b), names(a))
cat("== LAYERS ==\n")
cat(sprintf("  in deployed but NOT in candidate: %s\n",
            if (length(miss)) paste(miss, collapse = ", ") else "none"))
cat(sprintf("  new in candidate                : %s\n",
            if (length(extra)) paste(extra, collapse = ", ") else "none"))
if (length(miss))
  cat("  *** a dropped layer degrades SILENTLY -- estimate_ability() skips an\n",
      "      adjustment whose column is absent, without complaint ***\n", sep = "")

# 2. What actually differs, layer by layer.
cat("\n== WHAT CHANGES ==\n")
for (k in intersect(names(a), names(b))) {
  x <- a[[k]]; y <- b[[k]]
  if (identical(x, y)) next
  if (is.numeric(x) && is.numeric(y) && length(x) == 1 && length(y) == 1) {
    cat(sprintf("  %-16s %s -> %s\n", k, format(x), format(y)))
  } else if (is.data.frame(x) && is.data.frame(y)) {
    cat(sprintf("  %-16s table %d x %d -> %d x %d\n", k,
                nrow(x), ncol(x), nrow(y), ncol(y)))
  } else {
    cat(sprintf("  %-16s differs (%s)\n", k, class(x)[1]))
  }
}

# 3. The sensitivity product, because CLAUDE.md says quote the product and
#    never the spread -- the two are identified only jointly.
sens <- function(c0) {
  s <- c0$athlete
  # The column is `sensitivity`; an earlier guess of `s` returned NA silently,
  # which is exactly the failure this script is meant to catch elsewhere.
  if (is.null(s) || !"sensitivity" %in% names(s)) return(NA_real_)
  stats::sd(s$sensitivity, na.rm = TRUE)
}
sa <- sens(a); sb <- sens(b)
cat(sprintf("\n== SENSITIVITY ==\n  sd(s_i) %.4f -> %.4f\n", sa, sb))
if (!is.finite(sa) || !is.finite(sb))
  cat("  *** could not read the sensitivity column -- do not report this as 'no change' ***\n")
cat("  Quote the PRODUCT sd(s_i) x condition_sd, never the spread alone: the two\n")
cat("  are identified only jointly, so the spread on its own says nothing.\n")

# 4. What this check CANNOT tell you.
cat("\n== NOT CHECKED, and it matters ==\n")
cat("  * Whether the candidate predicts BETTER. That needs a backtest arm;\n")
cat("    a layer diff is not evidence.\n")
cat("  * Whether the shipping path can express what the backtest measured.\n")
cat("    deployed_ability() passes only (past, as_of, half_life, calibration),\n")
cat("    so adjust_race, peak_gamma, robust_location, decouple_peak,\n")
cat("    sigma_parts and sigma_mode take PACKAGE DEFAULTS on every shipped\n")
cat("    number regardless of what an arm used.\n")
cat("  * Whether artefacts built under the old calibration were re-scored.\n")
cat("    Birmingham's card was; its score report would otherwise describe two\n")
cat("    configurations at once.\n")
