# Fit per-family recency half-life on the athletics corpus / history for elite athletes.
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)

OUT <- here::here("citiusdata", "data")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

say("Loading athletics corpus...")
x <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
x <- flag_implausible(x)[!is.na(perf) & !is.na(event_id) & !is.na(date)]

# Optionally restrict to elite cohort if available
elite_file <- file.path(OUT, "elite_cohort.rds")
if (file.exists(elite_file)) {
  elite <- readRDS(elite_file)
  x <- x[as.character(athlete_id) %in% as.character(elite)]
  say(sprintf("Restricted to %d elite athletes (%s rows)", length(elite), format(nrow(x), big.mark=",")))
} else {
  say(sprintf("Fitting across all athletes (%s rows)", format(nrow(x), big.mark=",")))
}

say("Fitting per-family half-life...")
hl <- fit_half_life(x)

say("Fitted Half-Lives by Event Family:")
print(hl[order(family)])

saveRDS(hl, file.path(OUT, "half_life_fitted.rds"))
say("Wrote half_life_fitted.rds")
