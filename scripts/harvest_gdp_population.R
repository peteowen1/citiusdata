# Population and nominal GDP for every World Bank economy, 1960-2024.
#
# The previous version fetched a hand-picked list of 41 "major sporting
# countries". Because `summary_games_economic_dominance()` builds its
# denominator by summing over whichever nations matched, that turned "share of
# the competing nations' GDP" into "share of the GDP of the handful of nations
# on this list" -- and the list covered a median 11% of medalling nations at the
# African Games and 31% at Glasgow 2026. Fetch everything instead; the API
# returns all economies in one paged call.
#
# Also emits a UK home-nation split. The Commonwealth Games fields England,
# Scotland, Wales and Northern Ireland separately, so mapping all four to GBR
# put the whole UK into the denominator four times over.

library(jsonlite)
library(data.table)
library(arrow)

OUT  <- "C:/dev/citiusverse/citiusdata/data"
INST <- "C:/dev/citiusverse/citius/inst/extdata"
dir.create(OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(INST, showWarnings = FALSE, recursive = TRUE)

fetch_indicator <- function(indicator, value_name) {
  page <- 1L; out <- list(); total_pages <- NA_integer_; failed_at <- NA_integer_
  repeat {
    url <- sprintf(
      "https://api.worldbank.org/v2/country/all/indicator/%s?date=1960:2024&format=json&per_page=15000&page=%d",
      indicator, page)
    res <- tryCatch(fromJSON(url), error = function(e) NULL)
    # A dropped connection on page k of N used to exit this loop exactly like
    # reaching the last page, and printed nothing, so a truncated fetch looked
    # like ordinary missing data downstream. Record where it stopped.
    if (is.null(res) || length(res) < 2 || is.null(res[[2]])) {
      failed_at <- page
      break
    }
    df <- res[[2]]
    out[[length(out) + 1]] <- data.table(
      iso3         = df$countryiso3code,
      country_name = df$country$value,
      year         = as.integer(df$date),
      value        = as.numeric(df$value)
    )
    total_pages <- as.integer(res[[1]]$pages)
    cat(sprintf("  %s page %d/%d\n", indicator, page, total_pages))
    if (page >= total_pages) break
    page <- page + 1L
  }
  if (!is.na(failed_at)) {
    stop(sprintf("%s: fetch failed at page %d of %s -- refusing to return a
truncated series, which would look like missing data for whole countries.",
                 indicator, failed_at,
                 if (is.na(total_pages)) "unknown" else total_pages), call. = FALSE)
  }
  dt <- rbindlist(out)
  dt <- dt[!is.na(value) & nzchar(iso3)]
  setnames(dt, "value", value_name)
  unique(dt, by = c("iso3", "year"))
}

cat("=== World Bank: population ===\n")
pop_dt <- fetch_indicator("SP.POP.TOTL", "population")
cat("=== World Bank: nominal GDP (current US$) ===\n")
gdp_dt <- fetch_indicator("NY.GDP.MKTP.CD", "gdp_usd")

economic_dt <- merge(pop_dt, gdp_dt[, .(iso3, year, gdp_usd)],
                     by = c("iso3", "year"), all = TRUE)

# The API's "all countries" set includes regional and income aggregates
# (WLD, EAS, OED, ...). They are not competitors and would poison any
# denominator they were summed into.
aggregate_codes <- c(
  "WLD","EAS","ECS","LCN","MEA","NAC","SAS","SSF","EAP","ECA","LAC","MNA","SSA",
  "ARB","CSS","CEB","EAR","EMU","EUU","FCS","HPC","HIC","IBD","IBT","IDB","IDA",
  "IDX","LTE","LDC","LMY","LIC","LMC","MIC","OED","OSS","PSS","PST","PRE","SST",
  "TEA","TEC","TLA","TMN","TSA","TSS","UMC","AFE","AFW","INX","NA"
)
economic_dt <- economic_dt[!iso3 %in% aggregate_codes]

# Carry the last observed year forward to 2026 so current editions resolve.
last_year <- economic_dt[, max(year)]
for (y in (last_year + 1L):2026L) {
  fwd <- copy(economic_dt[year == last_year])
  fwd[, `:=`(year = y, carried_forward = TRUE)]
  economic_dt <- rbind(economic_dt, fwd, fill = TRUE)
}
economic_dt[is.na(carried_forward), carried_forward := FALSE]

# --- UK home nations -----------------------------------------------------
# The Commonwealth Games fields four UK teams. ONS mid-year estimates put the
# 2023 population split at England 57.7m / Scotland 5.49m / Wales 3.16m /
# Northern Ireland 1.92m, and regional GVA at roughly 86.4 / 7.6 / 3.5 / 2.5
# per cent. These shares move slowly, so they are applied as constants across
# years and flagged `apportioned` -- an approximation, but a far smaller error
# than counting the whole UK four times.
home_shares <- data.table(
  iso3       = c("ENG",     "SCO",     "WAL",     "NIR"),
  pop_share  = c(0.8604,    0.0819,    0.0471,    0.0286),
  gdp_share  = c(0.8640,    0.0760,    0.0350,    0.0250)
)
gbr <- economic_dt[iso3 == "GBR", .(year, population, gdp_usd, carried_forward)]
home <- home_shares[, .(
  iso3, year = rep(list(gbr$year), .N)
), by = seq_len(nrow(home_shares))][, seq_len := NULL]
home <- rbindlist(lapply(seq_len(nrow(home_shares)), function(i) {
  data.table(
    iso3            = home_shares$iso3[i],
    country_name    = c("England","Scotland","Wales","Northern Ireland")[i],
    year            = gbr$year,
    population      = gbr$population * home_shares$pop_share[i],
    gdp_usd         = gbr$gdp_usd    * home_shares$gdp_share[i],
    carried_forward = gbr$carried_forward,
    apportioned     = TRUE
  )
}))
economic_dt[, apportioned := FALSE]
economic_dt <- rbind(economic_dt, home, fill = TRUE)

economic_dt[, gdp_per_capita := gdp_usd / population]
setorder(economic_dt, iso3, year)

cat(sprintf("\n%d records across %d economies, %d-%d.\n",
            nrow(economic_dt), uniqueN(economic_dt$iso3),
            min(economic_dt$year), max(economic_dt$year)))

stopifnot(economic_dt[iso3 == "AUS" & year == 2020, population] > 24e6,
          economic_dt[iso3 == "AUS" & year == 2020, population] < 27e6,
          economic_dt[iso3 == "IND" & year == 2020, population] > 1.3e9,
          nrow(economic_dt[iso3 == "ENG" & year == 2020]) == 1)
cat("Anchor checks passed: AUS ~25.7m in 2020, IND >1.3bn, ENG present.\n")

write_parquet(economic_dt, file.path(OUT, "country_economic_history.parquet"))
saveRDS(economic_dt, file.path(OUT, "country_economic_history.rds"))
saveRDS(economic_dt, file.path(INST, "country_economic_history.rds"))
cat("Saved country_economic_history.\n")
