# An EMPIRICAL event-similarity matrix: which events actually transfer?
#
# Today the engine answers that with a taxonomy. form_ratings.R:708 restricts a
# thin athlete's cross-event siblings to the SAME FAMILY, which means the 1500m
# cannot learn from the 3000m - they sit in `middle` and `distance` - even though
# those are two of the events an athlete most obviously shares fitness across.
# Family is a hand-drawn approximation of similarity. This measures it instead.
#
# THE MEASUREMENT. For every pair of events, take the athletes who have a rating
# in both, standardise ratings WITHIN each event (so an event's overall level and
# spread drop out), and correlate. Events whose athletes keep their relative
# standing across the pair are events that transfer. That is exactly the quantity
# the blend needs and nothing about it is assumed from the event names.
#
# Built from the blend-OFF arm on purpose: measuring similarity on a corpus where
# blending has already pulled sibling ratings together would find the similarity
# the blend itself created.
#
# ANCHOR CHECK, and it is the point of the script rather than a formality: the
# 1500m's nearest neighbours must come out as the Mile, 800m and 3000m, and the
# 100m's as the 200m and 60m. Any metric that misses those is broken, and it is
# far better to learn that here than after it is wired into the engine.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
TAG    <- Sys.getenv("STATE_TAG", "xbh_0")
# COVER EVERY PAIR. These floors used to be 3 and 30, which silently excluded
# every ROAD and WALK event from the matrix: a marathoner races twice a year and
# never reaches n_eff 3, so no road pair could reach 30 shared athletes, and the
# 10,000m ended up with exactly ONE sibling in the whole table (the 5000m).
# Barega's 2:05:00 marathon, 27:37 10km and 1:01:22 half therefore contributed
# nothing at all to his 10,000m rating.
#
# The fix is not a looser gate, it is no gate: compute the correlation for every
# pair and let the correlation itself decide influence. 100m against marathon
# comes out near zero and contributes nothing on its own. The real risk of a low
# floor is SPURIOUS correlation from a handful of shared athletes, and that is
# handled by shrinking each correlation by its own standard error below, rather
# than by refusing to look.
MIN_NE <- as.numeric(Sys.getenv("MIN_NEFF", "1"))   # thin ratings shrink, not excluded
MIN_SH <- as.integer(Sys.getenv("MIN_SHARED", "5")) # enough to estimate anything at all
OUT    <- Sys.getenv("SIM_OUT", file.path(D, "event_similarity.parquet"))

f <- file.path(D, sprintf("seqv2_state_%s.parquet", TAG))
stopifnot("no state file for that tag" = file.exists(f))
s <- setDT(read_parquet(f))
cat(sprintf("state rows: %s | columns: %s\n", format(nrow(s), big.mark = ","),
            paste(names(s), collapse = ", ")))
rcol <- intersect(c("R", "r", "rating"), names(s))[1]
ncol_ <- intersect(c("n_eff", "neff", "n"), names(s))[1]
stopifnot("cannot find the rating column" = !is.na(rcol),
          "cannot find the n_eff column" = !is.na(ncol_))
setnames(s, c(rcol, ncol_), c("R", "n_eff"))
s <- s[is.finite(R) & is.finite(n_eff) & n_eff >= MIN_NE]
cat(sprintf("after n_eff >= %.1f: %s athlete-events, %s athletes, %d events\n",
            MIN_NE, format(nrow(s), big.mark = ","),
            format(uniqueN(s$athlete_id), big.mark = ","), uniqueN(s$event_id)))
stopifnot("nothing survived the n_eff filter" = nrow(s) > 0)

# standardise within event: an event's level and spread must not drive the
# correlation, only the ORDERING of athletes within it
# DROP ATHLETE-EVENTS CONTESTED MOSTLY THROUGH A COMBINED EVENT. Measured
# 2026-08-18 (check_similarity_ce_split.R): a pair's correlation is systematically
# higher among athletes a decathlon FORCED into both events than among
# specialists - 0.490 vs 0.414 overall, and worse across family boundaries
# (+0.087) than within them (+0.047). Several cross-family pairs do not merely
# shrink, they change sign: Javelin/Long Jump 0.422 -> -0.155, 110mH/Javelin
# 0.349 -> -0.160. Even Discus/Shot Put, the least controversial pair in the
# sport, runs 0.805 among decathletes against 0.479 among 1,200 throwers.
#
# The matrix is applied per athlete, and most athletes are specialists, so a
# correlation carried by decathletes describes the wrong population. Excluding
# those athlete-events measures transfer between events rather than "combined-
# event athletes are good at everything".
#
# CE_EXCLUDE=0 restores the old behaviour for comparison.
if (Sys.getenv("CE_EXCLUDE", "1") != "0") {
  cc <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                           col_select = c("event_id", "athlete_id", "round", "mark")))
  cc[, athlete_id := as.character(athlete_id)]
  cc <- cc[is.finite(mark) & mark > 0]
  cc[, is_ce := grepl("^(CE[0-9]*|Combined.*)$", round)]
  shr <- cc[, .(ce_frac = mean(is_ce, na.rm = TRUE)), by = .(athlete_id, event_id)]
  rm(cc); invisible(gc())
  s[, athlete_id := as.character(athlete_id)]
  n0 <- nrow(s)
  s <- merge(s, shr, by = c("athlete_id", "event_id"), all.x = TRUE)
  s <- s[is.na(ce_frac) | ce_frac <= 0.5]
  cat(sprintf("combined-event exclusion: dropped %s of %s athlete-events (%.1f%%)\n",
              format(n0 - nrow(s), big.mark = ","), format(n0, big.mark = ","),
              100 * (n0 - nrow(s)) / n0))
}
s[, z := (R - mean(R)) / stats::sd(R), by = event_id]
s <- s[is.finite(z)]

# --- pairwise correlation over shared athletes --------------------------------
evs <- sort(unique(s$event_id))
sm <- merge(s[, .(athlete_id, e1 = event_id, z1 = z)],
            s[, .(athlete_id, e2 = event_id, z2 = z)],
            by = "athlete_id", allow.cartesian = TRUE)
sm <- sm[e1 < e2]
sim <- sm[, .(shared = .N,
              cor = if (.N >= MIN_SH) stats::cor(z1, z2) else NA_real_),
          by = .(e1, e2)]
sim <- sim[is.finite(cor)]
# A correlation from 6 athletes and one from 600 are not the same evidence.
# Shrink toward zero by the effect against its own sampling error:
#     w = cor^2 / (cor^2 + se^2),   se^2 = (1 - cor^2)^2 / (n - 3)
# A real 0.85 over hundreds of athletes barely moves; a spurious 0.6 over six
# collapses. Noise used as a WEIGHT, never as a gate.
# Shrink on SAMPLE SIZE in Fisher-z space. The first attempt shrank on the
# effect against se^2 = (1 - r^2)^2 / (n - 3), which fails exactly where it
# matters: as r approaches 1 that se collapses, so 100m against 3000m
# steeplechase - FIVE shared athletes, r = 0.979 - came through at 0.978
# untouched. Five sprinters who once ran a steeplechase is not a measurement of
# the pair, however significant it tests.
#     w = (n - 3) / ((n - 3) + SIM_KAPPA),  applied to z = atanh(r)
# n=5 -> w 0.09 (that pair falls to ~0.20 and is inert); n=167 -> 0.89;
# n=421 -> 0.95; n=2699 -> 0.99. Coverage is kept, influence is earned.
SIM_K <- as.numeric(Sys.getenv("SIM_KAPPA", "20"))
sim[, se := 1 / sqrt(pmax(shared - 3, 1))]           # sd of Fisher z
sim[, w_n := pmax(shared - 3, 0) / (pmax(shared - 3, 0) + SIM_K)]
sim[, cor_shrunk := tanh(w_n * atanh(pmin(pmax(cor, -0.999), 0.999)))]
# a negative weight would push a rating the WRONG way and no pair here has a
# mechanism for that, so floor it at zero
sim[, cor_use := pmax(0, cor_shrunk)]
cat(sprintf("\nevent pairs with >= %d shared athletes: %s\n",
            MIN_SH, format(nrow(sim), big.mark = ",")))
stopifnot("no event pair cleared the shared-athlete floor" = nrow(sim) > 0)

reg <- as.data.table(citius::citius_events())[
  , .(event_id, discipline, sex, family, tactical, technical)]
lab <- function(x) reg$discipline[match(x, reg$event_id)]
fam <- function(x) reg$family[match(x, reg$event_id)]
sx  <- function(x) reg$sex[match(x, reg$event_id)]
sim[, `:=`(d1 = lab(e1), d2 = lab(e2), f1 = fam(e1), f2 = fam(e2),
           s1 = sx(e1), s2 = sx(e2))]
# same-sex pairs only: a man's 1500m rating and a woman's are on separate scales
# and share no athletes, so a cross-sex correlation is meaningless here
sim <- sim[s1 == s2]
sim[, same_family := f1 == f2]

cat("\n================ ANCHOR CHECK ================\n")
cat("If these are wrong, the metric is wrong. Nothing else in this file matters.\n")
missing_probe <- character(0)
for (probe in c("1500 Metres", "100 Metres", "Shot Put", "Marathon")) {
  for (sexi in c("M", "W")) {
    id <- reg[discipline == probe & sex == sexi, event_id]
    if (!length(id)) {
      # the "no neighbours" branch below is loud; this one - the probe not
      # existing in the registry at all - used to be silent, which is the same
      # bug one step earlier
      cat(sprintf("\n*** %s (%s): NOT IN THE REGISTRY, so it was never probed.\n",
                  probe, sexi))
      missing_probe <- c(missing_probe, sprintf("%s (%s) [absent]", probe, sexi))
      next
    }
    nb <- sim[e1 == id | e2 == id]
    # A probe with NO neighbours is the loudest possible result, and this used
    # to `next` past it in silence - exactly how road events sat outside the
    # matrix unnoticed while the Marathon probe printed nothing at all.
    if (!nrow(nb)) {
      cat(sprintf("\n*** %s (%s): NO NEIGHBOURS AT ALL - cannot borrow from anything.\n",
                  probe, sexi))
      missing_probe <- c(missing_probe, sprintf("%s (%s)", probe, sexi))
      next
    }
    nb[, other := fifelse(e1 == id, e2, e1)]
    setorder(nb, -cor)
    cat(sprintf("\n%s (%s) nearest by correlation:\n", probe, sexi))
    print(head(nb[, .(neighbour = lab(other), family = fam(other), shared,
                      cor = round(cor, 3), shrunk = round(cor_use, 3))], 8))
  }
}

if (length(missing_probe))
  cat(sprintf("\n*** %d anchor probes have NO neighbours: %s\n",
              length(missing_probe), paste(missing_probe, collapse = ", ")))

# A family entirely absent from the matrix is a silent hole in the blend.
cat("\n================ FAMILY COVERAGE ================\n")
present <- unique(c(fam(sim$e1), fam(sim$e2)))
# Compare against the families actually PRESENT IN THIS STATE FILE, not the whole
# registry: the registry also holds 36 swimming events, which an athletics run
# has no rows for and whose absence is not a hole.
scope   <- sort(unique(fam(unique(s$event_id))))
absent  <- setdiff(scope, present)
cat(sprintf("families with at least one pair: %s\n", paste(sort(present), collapse = ", ")))
if (length(absent))
  cat(sprintf("*** ABSENT ENTIRELY: %s\n", paste(absent, collapse = ", ")))
cat(sprintf("events appearing at all: %d of %d rated in this state file\n",
            uniqueN(c(sim$e1, sim$e2)), uniqueN(s$event_id)))
stopifnot("a whole family is missing from the similarity matrix" = length(absent) == 0)

cat("\n================ DOES CORRELATION AGREE WITH FAMILY? ================\n")
print(sim[, .(pairs = .N, mean_cor = round(mean(cor), 3),
              median_cor = round(median(cor), 3)), by = same_family])
cat("\ncross-family pairs that correlate MORE than the median same-family pair\n")
cat("- these are the transfers the current family gate throws away:\n")
thr <- sim[same_family == TRUE, median(cor)]
x <- sim[same_family == FALSE & cor > thr]
setorder(x, -cor)
print(head(x[, .(a = d1, fa = f1, b = d2, fb = f2, sex = s1,
                 shared, cor = round(cor, 3))], 20))
cat(sprintf("\n%d cross-family pairs beat the median same-family correlation (%.3f)\n",
            nrow(x), thr))

write_parquet(sim[, .(e1, e2, shared, cor, se, w_n, cor_shrunk, cor_use, same_family)], OUT)
cat(sprintf("\nwrote %s (%s pairs)\n", basename(OUT), format(nrow(sim), big.mark = ",")))
