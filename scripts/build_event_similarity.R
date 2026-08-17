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
MIN_NE <- as.numeric(Sys.getenv("MIN_NEFF", "3"))   # an athlete needs real evidence
MIN_SH <- as.integer(Sys.getenv("MIN_SHARED", "30"))# a pair needs enough athletes
OUT    <- file.path(D, "event_similarity.parquet")

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
for (probe in c("1500 Metres", "100 Metres", "Shot Put", "Marathon")) {
  for (sexi in c("M", "W")) {
    id <- reg[discipline == probe & sex == sexi, event_id]
    if (!length(id)) next
    nb <- sim[e1 == id | e2 == id]
    if (!nrow(nb)) next
    nb[, other := fifelse(e1 == id, e2, e1)]
    setorder(nb, -cor)
    cat(sprintf("\n%s (%s) nearest by correlation:\n", probe, sexi))
    print(head(nb[, .(neighbour = lab(other), family = fam(other),
                      shared, cor = round(cor, 3))], 6))
  }
}

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

write_parquet(sim[, .(e1, e2, shared, cor, same_family)], OUT)
cat(sprintf("\nwrote %s (%s pairs)\n", basename(OUT), format(nrow(sim), big.mark = ",")))
