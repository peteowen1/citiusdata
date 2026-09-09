# Scale the family-pool offsets by a constant, and write them under a new name.
#
# WHY. Offsets fitted on [2016, 2020) and applied to 2020+ OVER-correct:
# measured 2026-09-05, every family flipped from over-optimistic to pessimistic
# after the debias (walk -1.31, road -0.90, hurdles -0.51, sprint -0.41,
# throw -0.36, jump -0.23). The model's bias was larger in the fit era than in
# the test era, so a correction sized on the old era is too big for the new one.
#
# This does NOT refit anything -- it rescales an existing, honestly out-of-sample
# fit. The scale is the only free parameter and it is swept, not chosen.
#
# Usage:
#   CITIUS_OFFSET_SCALE=0.8 CITIUS_OFFSET_OUT=family_pool_offsets_s08.rds \
#     Rscript citiusdata/scripts/scale_family_pool_offsets.R
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
SRC   <- Sys.getenv("CITIUS_OFFSET_SRC", "family_pool_offsets.rds")
SCALE <- as.numeric(Sys.getenv("CITIUS_OFFSET_SCALE", "0.8"))
DEST  <- Sys.getenv("CITIUS_OFFSET_OUT", "")
stopifnot("scale must be finite and in (0, 2]" = is.finite(SCALE) && SCALE > 0 && SCALE <= 2,
          "CITIUS_OFFSET_OUT is required" = nzchar(DEST))

x <- readRDS(file.path(OUT, SRC))
for (f in c("mu0", "fs_map", "ev_map")) x[[f]] <- x[[f]] * SCALE
# Provenance travels with the file: an offsets table that cannot say what it is
# is exactly how a scaled arm gets read back as an unscaled one.
x$scaled_from <- SRC
x$scale <- SCALE
saveRDS(x, file.path(OUT, DEST))
cat(sprintf("wrote %s: %s scaled by %.2f (mu0 %.3f, %d family cells, %d events)\n",
            DEST, SRC, SCALE, x$mu0, length(x$fs_map), length(x$ev_map)))
