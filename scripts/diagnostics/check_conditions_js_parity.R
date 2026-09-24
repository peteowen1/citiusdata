# Does conditions.js give the same numbers as citius/R/conditions.R?
# R writes random inputs + its own answers, node runs the JS on the same
# inputs, R compares. Any drift between the live (JS) and batch (R) adjusters
# shows up here, not on the site.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({ library(data.table); library(jsonlite) })
PDIR <- here::here("citiusdata", "data", "conditions_params")
set.seed(7)
evs <- c("AT-100Metres-M", "AT-LongJump-W", "AT-5000Metres-M", "AT-Marathon-W", "AT-ShotPut-M")
cases <- rbindlist(lapply(evs, function(ev) {
  p <- conditions_params(ev, PDIR)
  n <- 40
  d <- data.table(event_id = ev,
                  wind = if (is.null(p$wind)) NA_real_ else sample(c(runif(n, -7, 9), NA), n),
                  alt_m = sample(c(runif(n, -5, 3000), NA), n),
                  indoor = sample(c(TRUE, FALSE), n, TRUE),
                  expected = sample(c(rnorm(n, 0, 0.02), NA), n))
  a <- adjust_conditions(p, d$wind, d$alt_m, d$indoor)
  d[, perf := rnorm(n, 0, 0.03)]
  d[, cleaned := perf - a$wind_adj - a$venue_adj - a$indoor_adj]
  d[, resid := cleaned - expected]
  d[, race := rep(1:5, length.out = n)]
  d[, shock_r := race_shock(resid, p$var_race, p$var_resid), by = race]
  d[, shock_loo_r := race_shock_loo(resid, p$var_race, p$var_resid), by = race]
  cbind(d, a)
}))
tmp <- tempfile(fileext = ".json")
write_json(cases, tmp, na = "null", digits = 12)

js <- sprintf('
const c = require(%s); const all = require(%s); const cases = require(%s)
const out = []
const groups = {}
cases.forEach((r, i) => { r.i = i; const k = r.event_id + "|" + r.race; (groups[k] ||= []).push(r) })
for (const k in groups) {
  const p = all[groups[k][0].event_id]
  const rows = groups[k].map(r => ({ i: r.i, perf: r.perf, wind: r.wind ?? NaN, alt_m: r.alt_m ?? NaN, indoor: r.indoor, expected: r.expected ?? NaN }))
  const adj = c.adjustRace(p, rows, p.var_resid, false)
  const loo = c.adjustRace(p, rows, p.var_resid, true)
  adj.forEach((a, j) => out[a.i] = { wind_adj: a.wind_adj, venue_adj: a.venue_adj, indoor_adj: a.indoor_adj, shock_js: a.race_shock, shock_loo_js: loo[j].race_shock })
}
process.stdout.write(JSON.stringify(out))',
  toJSON(file.path(PDIR, "conditions.js"), auto_unbox = TRUE), toJSON(file.path(PDIR, "_all.json"), auto_unbox = TRUE), toJSON(tmp, auto_unbox = TRUE))
jsf <- tempfile(fileext = ".js"); writeLines(js, jsf)
res <- as.data.table(fromJSON(paste(system2("node", jsf, stdout = TRUE), collapse = "")))
stopifnot(nrow(res) == nrow(cases))
d <- data.table(wind = abs(res$wind_adj - cases$wind_adj), venue = abs(res$venue_adj - cases$venue_adj),
                indoor = abs(res$indoor_adj - cases$indoor_adj), shock = abs(res$shock_js - cases$shock_r),
                shock_loo = abs(res$shock_loo_js - cases$shock_loo_r))
cat(sprintf("%d cases over %d events. Max |JS - R| per component (perf units; anything above 1e-9 is a bug):\n", nrow(cases), length(evs)))
print(d[, lapply(.SD, max)])
stopifnot(all(d[, lapply(.SD, max)] < 1e-9))
cat("PARITY OK\n")
