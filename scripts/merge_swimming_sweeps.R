# Merge a second CRS sweep into the swimming capture.
#
# The original capture (glasgow2026_swimming.json, 27 July) holds 17 of the 34
# individual events. It was taken while the meet was still running, so the later
# sessions were never on the page to be scraped -- availability was mistaken for
# coverage, the same failure the package docs record for Birmingham 2022.
#
# The second sweep (4 August) re-walked every schedule day and captured what the
# first one could not. The two are complementary rather than overlapping: the
# first holds five events the second's sweep never listed, so BOTH are kept and
# the union is what counts.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages({library(data.table); library(jsonlite)})
source(here::here("citiusdata", "scripts", "_merge_guards.R"))
D <- here::here("citiusdata", "data")

f1 <- file.path(D, "glasgow2026_swimming.json")
f2 <- file.path(D, "glasgow2026_swimming_sweep2.json")
stopifnot(file.exists(f1), file.exists(f2))

a <- as.data.table(parse_crs_export(f1)); a[, src := "sweep1"]
b <- as.data.table(parse_crs_export(f2)); b[, src := "sweep2"]
cat(sprintf("sweep1: %s rows, %d events\n", format(nrow(a), big.mark=","),
            uniqueN(a$event_id[!is.na(a$event_id)])))
cat(sprintf("sweep2: %s rows, %d events\n", format(nrow(b), big.mark=","),
            uniqueN(b$event_id[!is.na(b$event_id)])))

m <- rbind(a, b, fill = TRUE)
# A swim is identified by event, round, athlete and mark. The two sweeps overlap
# on the events both saw, so dedupe rather than double-count.
key <- c("event_id", "round", "athlete_name", "mark_string")
m <- m[!duplicated(m[, ..key])]
cat(sprintf("merged: %s rows, %d events (%d duplicate swims dropped)\n",
            format(nrow(m), big.mark=","), uniqueN(m$event_id[!is.na(m$event_id)]),
            nrow(a) + nrow(b) - nrow(m)))

SW <- c("50mFreestyle","100mFreestyle","200mFreestyle","400mFreestyle","800mFreestyle",
        "1500mFreestyle","50mBackstroke","100mBackstroke","200mBackstroke",
        "50mBreaststroke","100mBreaststroke","200mBreaststroke","50mButterfly",
        "100mButterfly","200mButterfly","200mIndividualMedley","400mIndividualMedley")
expect <- c(paste0("SW-", SW, "-M"), paste0("SW-", SW, "-W"))
have <- unique(m$event_id[!is.na(m$event_id)])
isf <- function(r) grepl("final", r, ignore.case=TRUE) & !grepl("semi", r, ignore.case=TRUE)
fin <- unique(m[isf(round) & !is.na(place) & place == 1]$event_id)

cat(sprintf("\nindividual events: %d of %d present (any round)\n",
            sum(expect %in% have), length(expect)))
cat(sprintf("individual events WITH A COMPLETED FINAL: %d of %d\n",
            sum(expect %in% fin), length(expect)))
cat("\nstill absent entirely:\n"); print(setdiff(expect, have))
cat("\npresent but no final captured:\n"); print(setdiff(intersect(expect, have), fin))

# Atomic (tmp-then-rename) -- found by review 2026-09-04, same as
# merge_athletics_crs.R.
citius_atomic_write(m, file.path(D, "glasgow2026_swimming_merged.rds"))
arrow::write_parquet(m, file.path(D, "glasgow2026_swimming_merged.parquet"))
cat("\nwrote glasgow2026_swimming_merged.{rds,parquet}\n")
