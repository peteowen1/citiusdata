# Regression for form_ratings.R: the engine must reproduce a known arm exactly.
#
# The model is sequential and deterministic, so identical is the right bar — any
# change to processing order or arithmetic moves these digits. This is what
# licensed every optimisation this engine has had (split() -> boundary scan,
# CJ() -> integer vectors, unique() hoisted): each was required to leave the
# output untouched, and each did.
#
# Usage:  Rscript citiusdata/scripts/verify_form_engine.R
# from the VERSE ROOT (here::here anchors at the nearest git repo).
suppressMessages(library(data.table))
ROOT <- here::here()
OUT  <- file.path(ROOT, "citiusdata", "data")
TMP  <- file.path(tempdir(), "form_verify")
dir.create(TMP, showWarnings = FALSE, recursive = TRUE)

# The reference arm: engine DEFAULTS at cap 12, which is the shipping config.
# Update these two numbers deliberately, never to make a run pass.
EXPECT <- c(conc25 = 69.127, conc26 = 69.387)
TOL <- 1e-3   # the reference is quoted to 3dp, so compare at that precision

e <- new.env()
Sys.setenv(SEQ_TAG = "verify", FORM_OUT = TMP)
sys.source(file.path(ROOT, "citiusdata", "scripts", "form_ratings.R"), envir = e)

got <- c(conc25 = e$res$conc25, conc26 = e$res$conc26)
cat("\n================ REGRESSION ================\n")
for (n in names(EXPECT))
  cat(sprintf("%-7s expected %.3f  got %.3f  delta %+.4f\n",
              n, EXPECT[[n]], got[[n]], got[[n]] - EXPECT[[n]]))
if (max(abs(got - EXPECT)) < TOL) {
  cat("PASS - the engine reproduces the reference arm\n")
} else {
  cat("FAIL - the engine no longer reproduces the reference arm.\n")
  cat("       If the change was intentional, update EXPECT and say why in the\n")
  cat("       commit. If it was not, this is the regression it exists to catch.\n")
  quit(status = 1)
}
