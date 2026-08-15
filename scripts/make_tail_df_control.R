# Build the tail_df control arm WITHOUT a rebuild.
#
# tail_df is a scalar computed at the END of calibrate() (calibrate.R:489) from
# the decomposition residuals, and read ONLY at prediction time (rounds.R:82,
# simulate.R:169). Nothing reads it while the other layers are being fitted, so
# overriding it post-hoc is equivalent to having fitted it to that value — and
# it gives a PERFECT single-variable arm: same corpus, same every other layer,
# tail_df alone differs.
#
# That removes step 1 of tail-df-refit-route.md (a full rebaseline_chain.R run)
# from the critical path.
OUT <- "C:/dev/citiusverse/citiusdata/data"
src <- file.path(OUT, "calibration_corpus_csigma_coast_keyfix.rds")
dst <- file.path(OUT, "calibration_corpus_csigma_coast_keyfix_tail30.rds")
c0 <- readRDS(src)
stopifnot(is.numeric(c0$tail_df), length(c0$tail_df) == 1L, c0$tail_df == 6)
c1 <- c0
c1$tail_df <- 30
# provenance, so this can never be mistaken for a fitted calibration
c1$provenance <- c(c0$provenance,
  tail_df_override = paste0("tail_df forced 6 -> 30 from ",
                            basename(src), " on 2026-08-15 as the A/B control; ",
                            "every other layer is byte-identical to the source"))
saveRDS(c1, dst)

# Prove the ONLY difference is tail_df.
a <- readRDS(src); b <- readRDS(dst)
a$tail_df <- NULL; b$tail_df <- NULL
a$provenance <- NULL; b$provenance <- NULL
cat(sprintf("layers identical apart from tail_df/provenance: %s\n",
            identical(a, b)))
cat(sprintf("source tail_df %.0f -> control tail_df %.0f\n",
            c0$tail_df, c1$tail_df))
cat(sprintf("wrote %s\n", basename(dst)))
