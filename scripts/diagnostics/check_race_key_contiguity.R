suppressMessages(library(data.table)); suppressMessages(library(arrow))
OUT <- "C:/dev/citiusverse/citiusdata/data"
cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
cat0 <- cat0[meet_tier %in% c("T1_elite","T2_strong"), .(competition_id, meet_tier)]
evs <- setdiff(sub("^event_id=","", list.dirs(file.path(OUT,"athletics_corpus_store"),
                   recursive=FALSE, full.names=FALSE)), "__unmatched__")
dl <- list()
for (EV in evs) {
  x <- tryCatch(setDT(read_parquet(file.path(OUT,
        sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV)),
        col_select = c("athlete_id","competition_id","date","perf","place","race_key","round"))),
        error = function(e) NULL)
  if (is.null(x)) next
  x[, `:=`(event_id = EV, athlete_id = as.character(athlete_id),
           competition_id = as.character(competition_id))]
  dl[[EV]] <- x
}
d <- rbindlist(dl, fill = TRUE); rm(dl); invisible(gc())
d <- merge(d, cat0, by = "competition_id")
d <- d[!is.na(perf) & !is.na(date) & !is.na(race_key) & !is.na(place) & place > 0 &
       date >= as.Date("2020-01-01")]
setorder(d, date, race_key)
d <- unique(d, by = c("race_key","athlete_id"))
d[, .blk := rleid(race_key)]
bad <- d[, .(blocks = uniqueN(.blk), dates = uniqueN(date), n = .N,
             evs = uniqueN(event_id)), by = race_key][blocks > 1]
cat(sprintf("race_keys occupying more than one block: %d\n\n", nrow(bad)))
print(bad)
if (nrow(bad)) {
  k <- bad$race_key[1]
  cat("\nrows of the offending key:\n")
  print(d[race_key == k, .(race_key, event_id, date, round, athlete_id, perf, place)][order(date)])
  cat("\nHow many races sit BETWEEN its two blocks (what a from:to scan would swallow):\n")
  rng <- d[race_key == k, range(.blk)]
  cat(sprintf("  blocks %d..%d -> %s races, %s rows\n",
              rng[1], rng[2],
              format(d[.blk >= rng[1] & .blk <= rng[2], uniqueN(race_key)], big.mark=","),
              format(d[.blk >= rng[1] & .blk <= rng[2], .N], big.mark=",")))
}
