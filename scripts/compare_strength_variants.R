# Compare meet-strength bases: career best (deployed) vs recency variants.
#
# See docs/plans/STRENGTH-METRIC-EXPERIMENT-2026-09-03.md for why and for
# the constraints. Adopts nothing -- writes a comparison table only.
#
# v3, added here: EXPONENTIALLY-WEIGHTED form. Weight each prior race by
# 2^(-age_days / half_life) using the family's OWN fitted half-life from
# _deployed.R, so there is no hand-picked window length -- the constant is
# measured, which the repo's no-hand-tuned-constants rule requires. It is
# also cheap: a weighted cumulative sum, no per-window sorting.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table); library(arrow)
OUT <- here::here("citiusdata", "data")
source(here::here("citiusdata", "scripts", "_deployed.R"))

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, competition_id := as.character(competition_id)]
ch <- ch[!is.na(perf) & !is.na(event_id) & !is.na(date)]

ev <- as.data.table(citius_events())[, .(event_id, family)]
ch <- merge(ch, ev, by = "event_id", all.x = TRUE)
hl_map <- DEPLOYED$hl_family
ch[, hl := fifelse(!is.na(family) & family %chin% names(hl_map),
                   unname(hl_map[family]), DEPLOYED$half_life)]
cat(sprintf("half-lives in use: %s (default %s)\n",
            paste(sprintf("%s=%s", names(hl_map), hl_map), collapse = ", "),
            DEPLOYED$half_life))

setorder(ch, athlete_id, event_id, date)
ch[, d := as.numeric(date)]

# EWMA over STRICTLY PRIOR races. Decay to a common origin so the whole
# thing is a cumulative sum: w_i = 2^(-(t_ref - t_i)/hl) rescaled at read
# time. Using shift() on the cumulative sums excludes the current race.
ch[, wk := 2^((d - min(d)) / hl), by = .(athlete_id, event_id)]
ch[, `:=`(csw = cumsum(wk), cswp = cumsum(wk * perf)), by = .(athlete_id, event_id)]
ch[, `:=`(psw = shift(csw, 1L), pswp = shift(cswp, 1L)), by = .(athlete_id, event_id)]
ch[, form_ew := fifelse(!is.na(psw) & psw > 0, pswp / psw, NA_real_)]
cat(sprintf("rows with EW form: %s of %s (%.1f%%)\n",
            format(ch[!is.na(form_ew), .N], big.mark = ","),
            format(nrow(ch), big.mark = ","),
            100 * ch[!is.na(form_ew), .N] / nrow(ch)))

strength_from <- function(dt, col) {
  x <- copy(dt)[!is.na(get(col))]
  x[, era := 4L * (year(date) %/% 4L)]
  x[, n_era := .N, by = .(event_id, era)]
  x[, p := fifelse(n_era >= 200L, frank(get(col), na.last = "keep") / sum(!is.na(get(col))),
                   NA_real_), by = .(event_id, era)]
  x[is.na(p), p := frank(get(col), na.last = "keep") / sum(!is.na(get(col))), by = event_id]
  fin <- x[!is.na(place) & grepl("final", round, ignore.case = TRUE) &
             !grepl("semi", round, ignore.case = TRUE) & !is.na(p)]
  road <- as.data.table(citius_events())[family == "road", event_id]
  fin[event_id %chin% road, .rk := frank(-perf, ties.method = "first"),
      by = .(competition_id, event_id)]
  fin <- fin[is.na(.rk) | .rk <= 10L][, .rk := NULL]
  q <- fin[, .(q = mean(p), n_ath = .N), by = .(competition_id, event_id, era)][n_ath >= 4]
  q[, n_meets := .N, by = .(event_id, era)]
  q <- q[n_meets >= 3]
  q[, pc := 100 * frank(q, ties.method = "average") / .N, by = .(event_id, era)]
  r <- q[, .(s = round(mean(pc), 1), won = .N), by = competition_id]
  ro <- q[, .(all_road = all(event_id %chin% road)), by = competition_id]
  r <- merge(r, ro, by = "competition_id", all.x = TRUE)
  r[won < 5L & !(!is.na(all_road) & all_road), s := NA_real_]
  r[, .(competition_id, s)]
}

cat("computing EW-based strength...\n")
s_ew <- strength_from(ch, "form_ew")
setnames(s_ew, "s", "s_ew")

ct <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
r5 <- setDT(read_parquet(file.path(OUT, "strength_recency.parquet")))
cmp <- merge(ct[, .(competition_id, comp_name, class, meet_tier, year, s_pb = strength)],
             r5[, .(competition_id, s_r5 = strength_r)], by = "competition_id", all.x = TRUE)
cmp <- merge(cmp, s_ew, by = "competition_id", all.x = TRUE)

ok <- cmp[!is.na(s_pb) & !is.na(s_r5) & !is.na(s_ew)]
cat(sprintf("\ncomparable on %s meets\n", format(nrow(ok), big.mark = ",")))
cat(sprintf("cor(pb, r5) = %.3f | cor(pb, ew) = %.3f | cor(r5, ew) = %.3f\n",
            cor(ok$s_pb, ok$s_r5), cor(ok$s_pb, ok$s_ew), cor(ok$s_r5, ok$s_ew)))

cat("\n=== BIAS CHECK: mean strength by class (does a variant inflate weak classes?) ===\n")
print(ok[, .(n = .N, pb = round(mean(s_pb),1), r5 = round(mean(s_r5),1),
             ew = round(mean(s_ew),1)), by = class][order(-n)][1:12])

cat("\n=== KNOWN-ANSWER PANEL ===\n")
PANEL <- c("Olympic Games","World Athletics Championships","Weltklasse",
           "Prefontaine","Bislett","Meeting de Paris","Golden Gala",
           "Memorial Van Damme","London Athletics Meet","Athletissima",
           "Boston Marathon","London Marathon","Berlin Marathon",
           "Tokyo Marathon","Chicago Marathon","New York City Marathon",
           "Valencia","NCAA Division I","European Athletics Championships",
           "Commonwealth Games")
for (p in PANEL) {
  h <- ok[grepl(p, comp_name, ignore.case = TRUE)]
  if (!nrow(h)) { cat(sprintf("%-34s (no scored meets)\n", p)); next }
  cat(sprintf("%-34s n=%3d  pb %5.1f  r5 %5.1f  ew %5.1f\n",
              p, nrow(h), mean(h$s_pb), mean(h$s_r5), mean(h$s_ew)))
}

cat("\n=== DISAGREEMENT: EW high, career-best low ===\n")
print(ok[order(s_ew - s_pb)][.N:(.N-9)][, .(comp_name, class, s_pb, s_r5, s_ew)])
cat("\n=== DISAGREEMENT: career-best high, EW low ===\n")
print(ok[order(s_pb - s_ew)][.N:(.N-9)][, .(comp_name, class, s_pb, s_r5, s_ew)])

write_parquet(cmp, file.path(OUT, "strength_variant_comparison.parquet"))
cat("\nwrote strength_variant_comparison.parquet\n")
