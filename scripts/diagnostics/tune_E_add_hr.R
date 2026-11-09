# Add recent-half-life multipliers to the cached version E cuts (data/flags/tune_E_cache/),
# because the R1-racers tuning chose hr = 2, the top of the first grid. Same fit as
# tune_thin_record_E.R's recent refit; adds R_<hr>, wR_<hr>, kR_<hr> to each cut file.
# Resumable: a cut that already has the columns is skipped. Arrow via PowerShell.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
EV <- c("AT-800Metres-M", "AT-800Metres-W", "AT-1500Metres-M", "AT-1500Metres-W",
        "AT-5000Metres-M", "AT-5000Metres-W")
ADD <- as.numeric(strsplit(Sys.getenv("HR_ADD", "3,4"), ",")[[1]])
co <- setDT(readRDS(file.path(OUT, "athletics_corpus.rds")))
h <- flag_implausible(co[event_id %in% EV]); rm(co); invisible(gc())
h <- h[!is.na(event_id) & !is.na(perf)]; h[, athlete_id := as.character(athlete_id)]
cal <- deployed_calibration(OUT); ep <- deployed_event_params()
for (f in sort(list.files(file.path(OUT, "flags", "tune_E_cache"), pattern = "^cut_.*\\.rds$", full.names = TRUE))) {
  d <- readRDS(f); cu <- d$cut[1]; t1 <- Sys.time(); todo <- ADD[!paste0("R_", ADD) %in% names(d)]
  if (!length(todo)) { cat(basename(f), "already has", paste(ADD, collapse = ","), "\n"); next }
  past <- h[date < cu]
  for (hr in todo) {
    epr <- copy(ep)[event_id %in% EV, half_life := half_life * hr]
    l <- .deployed_ability_raw(past, cu - 1, cal, epr); l[, athlete_id := as.character(athlete_id)]
    l <- l[, .(athlete_id, event_id, rr = ability_raw, wr = w_total, kr = shrinkage * w_total / (1 - shrinkage))]
    setnames(l, c("rr", "wr", "kr"), paste0(c("R_", "wR_", "kR_"), hr))
    n0 <- nrow(d); d <- merge(d, l, by = c("athlete_id", "event_id"), all.x = TRUE); stopifnot(nrow(d) == n0)
  }
  saveRDS(d, paste0(f, ".tmp")); file.rename(paste0(f, ".tmp"), f)
  cat(sprintf("%s: added hr %s, coverage %.4f, %.1f min\n", basename(f), paste(todo, collapse = ","),
              d[, mean(!is.na(get(paste0("R_", todo[1]))))], as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  rm(past); invisible(gc())
}
