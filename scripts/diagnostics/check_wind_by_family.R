# Pete: "conditions don't affect rankings is true for track - all the same - but
# what about field where different athletes have different wind?"
#
# The page claims a shared shock cannot reorder a field. That is only true if
# the condition IS shared. In a horizontal jump every attempt has its own wind
# reading, so it is NOT shared and the claim as written is wrong for field.
# Measure the within-race spread of wind by family.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
D <- "C:/dev/citiusverse/citiusdata/data"
reg <- as.data.table(citius::citius_events())[, .(event_id, family)]
evs <- setdiff(sub("^event_id=", "", list.dirs(file.path(D, "athletics_corpus_store"),
        recursive = FALSE, full.names = FALSE)), "__unmatched__")
x <- rbindlist(lapply(evs, function(EV) {
  f <- file.path(D, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(NULL)
  y <- tryCatch(setDT(read_parquet(f, col_select = c("race_key","wind"))), error=function(e) NULL)
  if (is.null(y)) return(NULL)
  y[, event_id := EV][]
}), fill = TRUE)
x <- merge(x[!is.na(wind)], reg, by = "event_id", all.x = TRUE)
cat(sprintf("rows with a wind reading: %s\n\n", format(nrow(x), big.mark=",")))
r <- x[, .(n = .N, uniq = uniqueN(wind)), by = .(race_key, family)]
r <- r[n >= 3]
out <- r[, .(races = .N,
             pct_all_same = round(100*mean(uniq == 1), 1),
             mean_distinct = round(mean(uniq), 2)), by = family][order(-pct_all_same)]
print(out)
cat("\npct_all_same = races where EVERY athlete shares one wind reading.\n")
cat("Near 100 means the condition really is shared and cancels from ordering.\n")
cat("Well below means each athlete met their own wind, and it does NOT cancel.\n\n")
sp <- x[, .(sd_within = stats::sd(wind)), by = .(race_key, family)][is.finite(sd_within)]
print(sp[, .(races = .N, median_sd = round(stats::median(sd_within), 3),
             p90_sd = round(quantile(sd_within, .9), 3)), by = family][order(-median_sd)])
cat("\nmedian_sd is the within-race spread of wind in m/s.\n")
