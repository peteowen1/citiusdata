# Corrected era-effect sizing: elite filter is the feed's OWN top tier
# (OW/GW/GL = Olympics, World Championships, Diamond League "GW"/"GL" grades)
# per citius/R/ability.R's .tier_class(), NOT the catalogue's meet_tier (which
# has severe year-to-year coverage gaps, checked and rejected) and NOT the
# earlier wrong code list that included "F" (verified in .tier_class() to be
# the LOWEST tier, not top -- that mistake is why check_era_effect.R's numbers
# are not trustworthy and are superseded by this script).
suppressMessages(devtools::load_all(here::here("citius")))
suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

corpus <- setDT(readRDS(file.path(D, "athletics_corpus.rds")))
targets <- c("Marathon", "Half Marathon", "10,000 Metres", "5000 Metres",
             "1500 Metres", "800 Metres", "200 Metres", "100 Metres",
             "Shot Put", "Discus Throw", "Hammer Throw")
d <- corpus[discipline %in% targets & sex %in% c("M", "W") & !is.na(mark) & !is.na(date)]
d[, year := data.table::year(date)]
d <- d[year >= 2012 & year <= 2026]
el <- d[toupper(trimws(tier)) %in% c("OW", "GW", "GL")]

cov <- dcast(el[, .N, by = .(discipline, sex, year)], discipline + sex ~ year, value.var = "N", fill = 0)
cat("=== elite (OW/GW/GL) row counts by year ===\n")
print(cov)

el[, oriented := mark * orientation]
data.table::setorder(el, discipline, sex, year, -oriented)
el[, rk := seq_len(.N), by = .(discipline, sex, year)]
top20 <- el[rk <= 20]
n_check <- top20[, .N, by = .(discipline, sex, year)]
cat("\n=== years where top-20 pool has fewer than 15 marks (thin, flag before trusting) ===\n")
print(n_check[N < 15][order(discipline, sex, year)])

annual <- top20[, .(n = .N, mean_mark = mean(mark), orientation = orientation[1]),
               by = .(discipline, sex, year)]
setorder(annual, discipline, sex, year)

size_break <- function(dsc, brk, half_window = 4) {
  a <- annual[discipline == dsc]
  pre  <- a[year >= brk - half_window & year < brk]
  post <- a[year >= brk & year <= brk + half_window]
  data.table(discipline = dsc, break_year = brk,
             pre_n_years = nrow(pre), post_n_years = nrow(post),
             pre_avg_n = mean(pre$n), post_avg_n = mean(post$n),
             pre_mean_mark = mean(pre$mean_mark), post_mean_mark = mean(post$mean_mark))
}

road <- c("Marathon", "Half Marathon", "10,000 Metres", "5000 Metres")
spike <- c("1500 Metres", "800 Metres", "200 Metres", "100 Metres")
control <- c("Shot Put", "Discus Throw", "Hammer Throw")

res <- rbindlist(c(
  lapply(road, size_break, brk = 2017),
  lapply(spike, size_break, brk = 2021),
  lapply(control, size_break, brk = 2017),
  lapply(control, size_break, brk = 2021)
))
res[, orientation := unique(el[, .(discipline, orientation)])$orientation[match(discipline, unique(el[, .(discipline, orientation)])$discipline)]]
res[, level_shift_pct := (post_mean_mark / pre_mean_mark - 1) * orientation * 100]

cat("\n=== level shift pre vs post break (both sexes combined; + is improvement) ===\n")
print(res[, .(pre_n_years = sum(pre_n_years), post_n_years = sum(post_n_years),
              pre_avg_n = mean(pre_avg_n), post_avg_n = mean(post_avg_n),
              level_shift_pct = mean(level_shift_pct)), by = .(discipline, break_year)][order(break_year, discipline)])
