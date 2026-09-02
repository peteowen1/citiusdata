# Does the seed half-life matter for ROAD events, where the aggregate cannot see?
#
# Pete: "I don't believe that not a single event can get better with a different
# decay parameter." Correct, and the SEEDHLPOW sweep did not test it. That sweep
# asked whether the optimal half-life SCALES WITH RACE FREQUENCY (it does not),
# and scored the answer on a metric that is ~98% track - the same blindness that
# had cross-event pooling refuted twice.
#
# This scores two half-lives on ROAD AND WALK EVENTS ONLY, where a 45-day window
# is most obviously wrong: those athletes' previous race is a year old, so every
# seed weight underflows and they arrive with a value but no evidence.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
ROAD <- reg[family %chin% c("road", "walk"), event_id]

score <- function(tag) {
  f <- file.path(D, sprintf("seqv3_history_%s.parquet", tag))
  if (!file.exists(f)) return(NULL)
  h <- setDT(read_parquet(f))
  h <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12 &
         year(date) %in% c(2025, 2026)]
  one <- function(x, lab) {
    if (!nrow(x)) return(NULL)
    x <- copy(x)[, rid := .GRP, by = race_key]
    a <- x[, .(rid, i = seq_len(.N), place, r = r_use)]
    m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
    m <- m[i.x < i.y & place.x != place.y]
    d <- m$r.x - m$r.y
    cw <- as.numeric((d > 0) == (m$place.x < m$place.y)); cw[d == 0] <- 0.5
    data.table(arm = tag, scope = lab, pairs = nrow(m),
               concordance = round(100 * mean(cw), 3))
  }
  rbind(one(h[event_id %chin% ROAD], "road + walk"),
        one(h[!event_id %chin% ROAD], "everything else"))
}
arms <- Sys.getenv("SEEDHL_ARMS", "shl_45,shl_200")
res <- rbindlist(lapply(strsplit(arms, ",")[[1]], score))
if (!nrow(res)) { cat("no history files for those tags - run the arms first\n"); quit(status = 0) }
print(dcast(res, scope + pairs ~ arm, value.var = "concordance"))
cat("\nIf the road number improves with a longer half-life while the aggregate\n")
cat("does not, then per-event decay IS right and the earlier sweep was blind.\n")
