# Score cross-event seeding on the COMMON pairs, and the new pairs separately.
#
# WHY THE POOLED NUMBER IS NOT THE ANSWER. Seeding a debutant gives them a
# rating, which makes them `seen`, which makes their debut race scoreable. The
# seeded arms therefore score 68,380 MORE pairs than the identity arm - and
# those pairs are the hardest in the corpus, because they involve an athlete
# nobody had rated before. A raw comparison mixes "did the model get better" with
# "the model is now being asked harder questions", and reports the second as the
# first. Exactly the confound the post-reharvest runbook warns about.
#
# So this splits three ways:
#   COMMON pairs - present in both arms. Did the model get better or worse at
#                  the questions it was already answering? This is the honest
#                  like-for-like, and if it moves, seeding has changed ratings
#                  for athletes who were not even seeded.
#   NEW pairs    - admitted only because seeding made someone scoreable. Above
#                  50% means the seed carries real information; at 50% it is
#                  noise wearing a number.
#   ALL          - the pooled figure, reported last and only for completeness.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
OUT  <- here::here("citiusdata", "data")
BASE <- Sys.getenv("XEV_BASE", "xev_id")
ARMS <- strsplit(Sys.getenv("XEV_ARMS", "xev_id,xev_02,xev_05,xev_10"), ",")[[1]]
YRS  <- as.integer(strsplit(Sys.getenv("XEV_YEARS", "2025,2026"), ",")[[1]])

pairset <- function(tag, yr) {
  f <- file.path(OUT, sprintf("seqv3_history_%s.parquet", tag))
  stopifnot("missing arm" = file.exists(f))
  d <- setDT(read_parquet(f, col_select = c("race_key","athlete_id","date","r_pre",
                                            "r_use","place","perf","seen")))
  if (!"r_use" %chin% names(d)) d[, r_use := r_pre]
  d[!is.finite(r_use), r_use := r_pre]
  d <- d[seen == TRUE & is.finite(r_use) & is.finite(place) & place > 0 &
         is.finite(perf) & year(date) == yr]
  dup <- d[, .(n = .N, marks = uniqueN(round(perf, 9))), by = .(race_key, place)][
           n > 1 & marks > 1, unique(race_key)]
  d <- d[!race_key %chin% dup]
  a <- d[, .(rid = .GRP, i = seq_len(.N), place, r_use, athlete_id), by = race_key]
  m <- merge(a, a, by = "rid", allow.cartesian = TRUE, suffixes = c(".x", ".y"))
  m <- m[i.x < i.y & place.x != place.y]
  dd <- m$r_use.x - m$r_use.y
  m[, cw := as.numeric((dd > 0) == (place.x < place.y))][dd == 0, cw := 0.5]
  # a stable identity for the pair, so two arms can be intersected on it
  # race_key is suffixed by the self-merge like every other shared column
  m[, pid := paste(race_key.x, pmin(athlete_id.x, athlete_id.y),
                   pmax(athlete_id.x, athlete_id.y), sep = "|")]
  m[, .(pid, cw)]
}

for (yr in YRS) {
  b <- pairset(BASE, yr)
  setkey(b, pid)
  cat(sprintf("\n=== %d ===\n", yr))
  out <- rbindlist(lapply(ARMS, function(tg) {
    x <- pairset(tg, yr); setkey(x, pid)
    common <- x[b, nomatch = NULL]          # pairs in BOTH
    newp   <- x[!b, on = "pid"]             # pairs only in this arm
    data.table(arm = tg,
               common_pairs = nrow(common),
               common = round(100 * mean(common$cw), 3),
               common_base = round(100 * mean(common$i.cw), 3),
               new_pairs = nrow(newp),
               new = if (nrow(newp)) round(100 * mean(newp$cw), 3) else NA_real_,
               all_pairs = nrow(x),
               all = round(100 * mean(x$cw), 3))
  }), fill = TRUE)
  out[, common_vs_base := round(common - common_base, 3)]
  out[, common_floor := round(100 * sqrt(0.25 / common_pairs), 3)]
  out[, new_floor := round(100 * sqrt(0.25 / pmax(new_pairs, 1)), 3)]
  print(out[, .(arm, common_pairs, common, common_vs_base, common_floor,
                new_pairs, new, new_floor, all_pairs, all)])
}

cat("\nREAD IT LIKE THIS. `common_vs_base` is whether the model changed on the\n")
cat("questions it was already answering - it should be ~0, because seeding only\n")
cat("touches athletes who had no rating. `new` is the honest verdict on the\n")
cat("feature: those pairs did not exist before, and anything meaningfully above\n")
cat("50 is information the model did not previously have. `all` falls simply\n")
cat("because the new pairs are harder than the average old one, and quoting that\n")
cat("as a regression would be a category error.\n")

f <- file.path(OUT, "xev_seed_scores.json")
cat(sprintf("\n(wrote nothing to %s - this is a console comparison)\n", basename(f)))
