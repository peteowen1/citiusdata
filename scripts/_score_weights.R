# WAC-CLASS SCORING WEIGHTS: what a race is worth when we judge the model.
#
# The project forecasts LA 2028. Getting an Olympic final wrong costs more than
# getting a category F club meet wrong, and an unweighted mean says they are
# worth the same. Every marks number measured before 2026-09-07 used that
# unweighted mean, which is why the headline read -8.38% while the championship
# races it exists to predict were nearer -6%.
#
# This is the single definition. Sourced by the scorer and by the fitters, so
# "what better means" is stated once rather than re-derived per script.
#
# DISTINGUISH IT FROM THE OTHER TWO TIER LEVERS, which sound alike:
#   w_static                how much a historical MARK counts when estimating
#                           an athlete's ability (swept as ^lambda; 0 won)
#   CITIUS_LAB_TIER_WEIGHT  how much a test row counts when FITTING, so the 27x
#                           more numerous T2 races cannot dominate
#   THIS FILE               how much a race counts when SCORING
#
# The default follows Pete's steer -- top tier worth 10x a club meet:
#
#   OW  Olympic Games / World Championships     10
#   GL  Diamond League                          10
#   GW  Continental Tour Gold / World-tier      10
#   DF  Diamond League Final                    10
#   A   category A meet                          3
#   B   category B meet                          3
#   C, D, E, F   everything below                1
#
# An unknown or missing code gets 1. That is deliberate: an unrecognised code
# should not silently inherit championship weight, and a zero would silently
# drop races. Codes are listed explicitly so a new one shows up as a 1 and gets
# noticed rather than guessed at.
#
# Override wholesale with CITIUS_SCORE_WEIGHTS, e.g.
#   CITIUS_SCORE_WEIGHTS="OW=1,GL=1,GW=1,DF=1,A=0,B=0,C=0,D=0,E=0,F=0"
# scores championships only. Set every weight to 1 to recover the old
# unweighted numbers for comparison with anything measured before today.

.WAC_SCORE_DEFAULT <- c(OW = 10, GL = 10, GW = 10, DF = 10,
                        A = 3, B = 3, C = 1, D = 1, E = 1, F = 1)

wac_score_weights <- function() {
  spec <- Sys.getenv("CITIUS_SCORE_WEIGHTS", "")
  if (!nzchar(spec)) return(.WAC_SCORE_DEFAULT)
  kv <- strsplit(trimws(strsplit(spec, ",")[[1]]), "=")
  bad <- vapply(kv, length, integer(1)) != 2L
  if (any(bad)) stop("CITIUS_SCORE_WEIGHTS wants name=value pairs, comma separated.")
  v <- stats::setNames(as.numeric(vapply(kv, `[`, "", 2)), vapply(kv, `[`, "", 1))
  if (any(!is.finite(v) | v < 0)) stop("CITIUS_SCORE_WEIGHTS values must be finite and >= 0.")
  v
}

# Cached race_key -> WAC tier. championship_results.rds is 4.5M rows and every
# scorer needs the same two columns from it, so it is read once and kept.
.race_tier_lookup <- function(out_dir) {
  f <- file.path(out_dir, "race_tier.rds")
  if (file.exists(f)) return(readRDS(f))
  ch <- data.table::setDT(readRDS(file.path(out_dir, "championship_results.rds")))
  lk <- unique(ch[, .(race_key, tier)], by = "race_key")
  rm(ch); invisible(gc())
  saveRDS(lk, f)
  lk
}

# Attach `sw`, the scoring weight, to a table carrying `race_key`.
# Reports the weight actually applied, because a scoring weight that silently
# defaulted to 1 everywhere would make a weighted run look identical to an
# unweighted one with nothing to say which had happened.
attach_score_weight <- function(d, out_dir, quiet = FALSE) {
  w <- wac_score_weights()
  if (!"tier" %in% names(d)) {
    d <- merge(d, .race_tier_lookup(out_dir), by = "race_key", all.x = TRUE)
  }
  d[is.na(tier), tier := "unknown"]
  j <- match(d$tier, names(w))
  d[, sw := data.table::fifelse(is.na(j), 1, unname(w[j]))]
  # NORMALISED TO MEAN 1, which matters more than it looks.
  #
  # A weighted mean is invariant to scaling, so the scorer does not care. The
  # FITTERS do: they shrink each event toward its family by an evidence count,
  # and the kappas were tuned against unweighted counts near 1,200. Left raw,
  # weights up to 10 took the effective count from 25,077 to 183,095 and the
  # shrinkage all but vanished -- family ranges for context_scale widened from
  # 0.467-0.598 to 0.321-1.118 with nothing having been decided.
  #
  # That is a side effect of a scoring change reaching into a fitting parameter,
  # silently and in the wrong direction. Normalising keeps "how much evidence is
  # there" on the same scale as before, so the weights change WHICH rows matter
  # without changing HOW MUCH the model is allowed to move.
  mw <- mean(d$sw)
  if (is.finite(mw) && mw > 0) d[, sw := sw / mw]
  if (!quiet) {
    s <- d[, .(races = data.table::uniqueN(race_key), weight = data.table::first(sw)), by = tier]
    data.table::setorder(s, -weight, -races)
    cat("scoring weights in force:\n")
    print(s)
    cat(sprintf("  effective race count %.0f from %d actual races\n",
                sum(unique(d[, .(race_key, sw)])$sw), data.table::uniqueN(d$race_key)))
  }
  d[]
}
