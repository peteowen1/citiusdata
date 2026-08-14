# Regression: the optimised engine must reproduce k0_095 EXACTLY.
# The model is sequential and deterministic, so identical is the right bar --
# any change to processing order or arithmetic would move these digits.
Sys.setenv(SEQ_CENS = "0.3", SEQ_AGE = "1", SEQ_STALE = "1",
           SEQ_K0 = "0.95", SEQ_KAPPA = "3", SEQ_KFLOOR = "0.18",
           SEQ_TAG = "optverify",
           FORM_OUT = "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-citiusverse/5a095d02-ce82-4b96-a703-6b96c0eb9c26/scratchpad/opt/out")
dir.create(Sys.getenv("FORM_OUT"), showWarnings = FALSE, recursive = TRUE)
source("C:/Users/peteo/AppData/Local/Temp/claude/C--dev-citiusverse/5a095d02-ce82-4b96-a703-6b96c0eb9c26/../scripts/form_ratings.R", echo = FALSE)

BASE <- c(conc25 = 68.5528929337102, conc26 = 68.0020364202814)
got  <- c(conc25 = res$conc25, conc26 = res$conc26)
cat("\n================ REGRESSION ================\n")
for (n in names(BASE))
  cat(sprintf("%-7s baseline %.13f  optimised %.13f  delta %.2e\n",
              n, BASE[[n]], got[[n]], got[[n]] - BASE[[n]]))
if (isTRUE(all.equal(unname(BASE), unname(got), tolerance = 1e-10))) {
  cat("PASS - identical to written precision\n")
} else {
  cat("FAIL - the optimisation changed the result; do not ship it\n")
}
