# Self-check for resolve_synthetic_ids() (citiusdata#23). Run:
#   Rscript scripts/test_resolve_synthetic_ids.R
# Stops with an error on the first failed expectation.
suppressMessages(library(data.table))
source("scripts/_resolve_synthetic_ids.R")
key <- function(x) vapply(strsplit(toupper(gsub("[^A-Za-z ]", " ", x)), " +"),
                          function(w) paste(sort(w[nzchar(w)]), collapse = ""), "")
res <- data.table(
  sport = "Swimming",
  event_id = c("E1", "E1", "E1", "E2", "E2", "E1"),
  athlete_id = c("111", "222", "333", "444", "555", "666"),
  athlete_name = c("Hannah STERRY", "Ann SMITH", "Ann SMITH",   # SMITH: two people
                   "Zed ZED", "Zed ZED", NA))                   # ZED: same key, 2 ids
pred <- data.table(
  athlete_id = c("CRS:HANNAHSTERRY", "CRS:ANNSMITH", "CRS:ZEDZED", "CRS:HANNAHSTERRY",
                 "CRS:NOBODY", "777"),
  event_id = c("E1", "E1", "E2", "E2", "E1", "E1"),
  athlete = c(NA, "Ann Smith", "Zed Zed", "Hannah Sterry", "Nobody", "Seven"))
out <- resolve_synthetic_ids(pred, res, key)
stopifnot(
  identical(out$athlete_id, c("111", "CRS:ANNSMITH", "CRS:ZEDZED", "CRS:HANNAHSTERRY",
                              "CRS:NOBODY", "777")),   # null-name row resolved from the id
  identical(out$id_unresolved, c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)),
  identical(out$athlete_id_original, pred$athlete_id))
# HANNAHSTERRY in E2 stays unresolved: she has no results row in that event.
cat("resolve_synthetic_ids: all expectations hold\n")
