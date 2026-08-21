# Did the technical/tactical ceiling adjustment do what it was AIMED at?
#
# The pooled score says no - every arm inside the noise floor, and the two
# windows disagreeing in sign. But CEILADJ is not a global effect and a pooled
# number cannot see it: it RAISES the ceiling weight for jump and throw and
# LOWERS it for middle and distance, leaving five families untouched. A real
# effect of the intended shape would appear as a gain in the technical families
# and a gain in the tactical ones, with exactly 0.000 everywhere else - and
# would be diluted to nothing when averaged over all nine.
#
# THE UNTOUCHED FAMILIES ARE THE CONTROL, and a strong one. If sprint, hurdles,
# road, walk or combined move at all, something other than the parameter is
# moving, and the whole comparison is contaminated. Exactly 0.000 there is what
# licenses reading the other four.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
ARMS <- strsplit(Sys.getenv("ADJ_ARMS", "cadj0,cadj005,cadj010,cadj015"), ",")[[1]]
BASE <- Sys.getenv("ADJ_BASE", "cadj0")
YRS  <- as.integer(strsplit(Sys.getenv("ADJ_YEARS", "2025,2026"), ",")[[1]])

reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
TECH <- c("jump", "throw")        # ceiling raised
TACT <- c("middle", "distance")   # ceiling lowered

.load <- function(tag) {
  f <- file.path(OUT, sprintf("seqv3_history_%s.parquet", tag))
  stopifnot("missing arm" = file.exists(f))
  d <- setDT(read_parquet(f, col_select = c("race_key","event_id","date","r_pre","r_use",
                                            "place","perf","seen")))
  if (!"r_use" %chin% names(d)) d[, r_use := r_pre]
  d[!is.finite(r_use), r_use := r_pre]
  d <- d[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 & is.finite(perf)]
  merge(d, reg, by = "event_id", all.x = TRUE)
}

conc <- function(d) {
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  if (nrow(m) < 500) return(NULL)
  dd <- m$r_use.x - m$r_use.y
  cw <- as.numeric((dd > 0) == (m$place.x < m$place.y)); cw[dd == 0] <- 0.5
  data.table(pairs = nrow(m), conc = 100 * mean(cw),
             floor = round(100 * sqrt(0.25 / nrow(m)), 3))
}

res <- rbindlist(lapply(ARMS, function(tg) {
  d <- .load(tg)
  rbindlist(lapply(YRS, function(y) {
    rbindlist(lapply(unique(na.omit(d$family)), function(fm) {
      r <- conc(d[family == fm & year(date) == y])
      if (is.null(r)) NULL else cbind(arm = tg, yr = y, family = fm, r)
    }), fill = TRUE)
  }), fill = TRUE)
}), fill = TRUE)

res[, role := fifelse(family %chin% TECH, "technical (ceiling UP)",
             fifelse(family %chin% TACT, "tactical (ceiling DOWN)", "untouched (control)"))]
for (y in YRS) {
  r <- res[yr == y]
  b <- r[arm == BASE, .(family, base = conc)]
  r <- merge(r, b, by = "family")
  r[, vs_base := round(conc - base, 3)]
  cat(sprintf("\n=== %d ===\n", y))
  print(dcast(r, role + family + pairs ~ arm, value.var = "vs_base")[order(role, family)])
}
cat("\nEvery `untouched (control)` row must be exactly 0.000 in every arm. If it\n")
cat("is not, the arms differ by something other than CEILADJ and nothing else\n")
cat("here can be read. If the controls are clean, the technical and tactical\n")
cat("rows are the actual test, and both should be POSITIVE for the mechanism to\n")
cat("hold - the parameter moves them in opposite directions on purpose.\n")

f <- file.path(OUT, "ceiladj_by_family.json")
writeLines(jsonlite::toJSON(res, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
