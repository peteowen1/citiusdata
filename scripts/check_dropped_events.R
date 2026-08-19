# WHICH events fall below ten rated athletes, and why. "Too few athletes" can
# mean the event is genuinely rare, or that our tier filter has cut it off at
# the knees - those need opposite responses, so separate them.
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
d <- setDT(read_parquet(file.path(D, "form_display_final.parquet")))
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]
d <- merge(d, reg, by = "event_id", all.x = TRUE)
dep <- d[, .(rated = .N, lead_n = round(max(n_eff[rk == 1]), 1)), by = .(event_id, discipline, sex, family)]
drop <- dep[rated < 10][order(rated)]
cat("=== DROPPED: fewer than 10 rated athletes ===\n")
print(drop[, .(discipline, sex, family, rated, lead_n)])

# how much RAW data does each dropped event actually have, before the T1/T2 cut?
cat("\n=== what exists upstream for each ===\n")
cat0 <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
res <- rbindlist(lapply(drop$event_id, function(EV) {
  f <- file.path(D, sprintf("athletics_corpus_store/event_id=%s/part-0.parquet", EV))
  if (!file.exists(f)) return(data.table(event_id = EV, corpus_rows = 0L,
                                          t1 = 0L, t2 = 0L, uncat = 0L, other = 0L))
  x <- setDT(read_parquet(f, col_select = c("athlete_id","competition_id","date")))
  x[, competition_id := as.character(competition_id)]
  x <- merge(x, cat0[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
  data.table(event_id = EV, corpus_rows = nrow(x),
             t1 = x[meet_tier == "T1_elite", .N], t2 = x[meet_tier == "T2_strong", .N],
             uncat = x[is.na(meet_tier), .N],
             other = x[!is.na(meet_tier) & !meet_tier %chin% c("T1_elite","T2_strong"), .N])
}))
m <- merge(drop[, .(event_id, discipline, sex, rated)], res, by = "event_id")
setorder(m, rated)
print(m[, .(discipline, sex, rated, all_rows = corpus_rows, T1 = t1, T2 = t2,
            T3 = other, uncatalogued = uncat)])
cat("\nT1+T2 is what the engine keeps. A row with plenty of 'uncatalogued' or\n")
cat("'T3' and almost no T1/T2 is a TIERING gap, not a rare event.\n")
