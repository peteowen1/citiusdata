# Which per-family figures are actually measurable, and which are not?
#
# The per-event noise column answers "could ONE event show this by chance". The
# family figure pools its events, so it gets a smaller floor - but "smaller" is
# not "zero", and two families here are so small that their headline numbers sit
# underneath it. That applies to a gain I have been quoting as a result, not
# only to the walk loss I was worried about.
#
# The floor below is binomial on the pair count, which ASSUMES pairs are
# independent. They are not - pairs from one race share a race effect - so the
# true floor is LARGER than this. Every "inside the floor" verdict is therefore
# conservative in the safe direction.
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
ld <- function(f, lab) {
  j <- jsonlite::fromJSON(file.path(D, f))
  e <- as.data.table(j$by_event[[1]]); f2 <- as.data.table(j$by_family[[1]])
  merge(e[, .(pairs = sum(pairs)), by = family], f2[, .(family, pooled)],
        by = "family")[, change := lab][]
}
x <- rbind(ld("fam_shock.json", "Race-shock weighting"),
           ld("fam_adjusted.json", "Adjusted marks"))
stopifnot("nothing loaded" = nrow(x) > 8)
x[, floor := 100 * sqrt(0.75 * 0.25 / pairs)]
x[, verdict := fifelse(abs(pooled) > 2 * floor, "measurable",
                fifelse(abs(pooled) > floor, "marginal", "INSIDE NOISE"))]
setorder(x, change, -pairs)
for (ch in unique(x$change)) {
  cat(sprintf("\n=== %s ===\n", ch))
  print(x[change == ch, .(family, pairs, pooled = round(pooled, 3),
                          floor = round(floor, 3),
                          ratio = round(abs(pooled) / floor, 1), verdict)])
}
