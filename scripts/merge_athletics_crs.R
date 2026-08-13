# Fold the Commonwealth results-system athletics capture into the harvest.
#
# The World Athletics feed for Glasgow 2026 holds only four competition days
# (27, 28, 30, 31 July) -- 29 July and 1 August never populated, and re-querying
# on 4 August returned the same 1,000 rows. Sixteen of the forty individual
# events therefore had no final on our side, including both Miles, both long
# jumps, 400m men and women, and the men's 5000m.
#
# The CRS is the same source used for swimming and it has them. Athletics is
# normally taken from the federation feed (see watch_glasgow2026.R) precisely so
# this scrape is not needed; it is needed because the feed did not deliver.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(jsonlite)})
D <- here::here("citiusdata", "data")

source(here::here("citiusdata", "scripts", "_deployed.R"))
crs <- .repair_sex_from_title(parse_crs_export(file.path(D, "glasgow2026_athletics_crs.json")))
if (!is.null(attr(crs, "sex_repaired")))
  cat("repaired", attr(crs, "sex_repaired"), "rows whose route disagreed with the page title
")
cat(sprintf("CRS athletics: %s rows, %d matched events, %d unmatched\n",
            format(nrow(crs), big.mark=","), uniqueN(crs$event_id[!is.na(crs$event_id)]),
            sum(is.na(crs$event_id))))
if (any(is.na(crs$event_id))) {
  cat("\nunmatched titles (relays and para are expected):\n")
  print(head(crs[is.na(event_id), .N, by = .(discipline)][order(-N)], 20))
}

isf <- function(r) grepl("final", r, ignore.case=TRUE) & !grepl("semi|qual", r, ignore.case=TRUE)
old <- setDT(readRDS(file.path(D, "glasgow2026_results.rds")))
oldf <- unique(old[isf(round) & !is.na(place) & place == 1]$event_id)
newf <- unique(crs[isf(round) & !is.na(place) & place == 1]$event_id)
newf <- newf[!is.na(newf)]
cat(sprintf("\nfinals: feed %d | CRS %d | union %d\n",
            length(oldf), length(newf), uniqueN(c(oldf, newf))))
cat("\nevents the CRS ADDS that the feed never had:\n")
print(sort(setdiff(newf, oldf)))

saveRDS(crs, file.path(D, "glasgow2026_athletics_crs.rds"))
arrow::write_parquet(crs, file.path(D, "glasgow2026_athletics_crs.parquet"))
cat("\nwrote glasgow2026_athletics_crs.{rds,parquet}\n")

# Anchor checks, chosen before looking: results read off the rendered pages.
a <- crs[grepl("KERR", athlete_name) & grepl("Mile|MILE|ONE MILE", discipline, ignore.case=TRUE) &
         isf(round)]
cat("\nANCHOR men's Mile winner:\n")
print(crs[event_id == "AT-Mile-M" & isf(round)][order(place)][1:3,
      .(place, athlete_name, country, mark_string)])
cat("\nANCHOR men's long jump (wind must NOT be the mark):\n")
print(crs[event_id == "AT-LongJump-M" & isf(round)][order(place)][1:3,
      .(place, athlete_name, country, mark_string, mark)])
