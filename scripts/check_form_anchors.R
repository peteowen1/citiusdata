suppressMessages(library(data.table)); suppressMessages(library(arrow))
st <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/data/seqv2_state_final.parquet"))
st[, athlete_id := as.character(athlete_id)]
nm <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/blog/athlete-ratings.parquet"))
nm <- unique(nm[, .(athlete_id = as.character(athlete_id), athlete_name)])
st <- merge(st, nm, by = "athlete_id", all.x = TRUE)
rk <- function(ev, pat, k = 8) {
  e <- st[event_id == ev & n_eff >= 3 & last >= as.Date("2026-01-01")][order(-R)]
  i <- grep(pat, e$athlete_name, ignore.case = TRUE)
  list(top = utils::head(e$athlete_name, k), pos = if (length(i)) i[1] else NA_integer_)
}
chk <- function(ev, pat, want, label) {
  r <- rk(ev, pat)
  ok <- !is.na(r$pos) && r$pos <= want
  cat(sprintf("%-28s %-22s rank %-4s %s\n", ev, label,
              ifelse(is.na(r$pos), "absent", r$pos), if (ok) "OK" else "*** FAIL ***"))
  ok
}
cat("ANCHORS at final knobs (k0 0.95, kappa 3, floor 0.32)\n\n")
res <- c(
  chk("AT-800Metres-W", "Hodgkinson", 3, "Hodgkinson top-3"),
  chk("AT-800Metres-W", "Werro",      3, "Werro top-3"),
  chk("AT-100Metres-M", "Lyles",      3, "Lyles top-3"),
  chk("AT-800Metres-M", "Wanyonyi",   3, "Wanyonyi top-3"),
  chk("AT-800Metres-M", "Arop",       3, "Arop top-3"),
  chk("AT-PoleVault-M", "Duplantis",  1, "Duplantis 1st"),
  chk("AT-LongJump-M",  "Tentoglou",  2, "Tentoglou top-2"))
cat("\n800m W top 8:\n"); print(rk("AT-800Metres-W", "zzz")$top)
cat("\n100m M top 8:\n"); print(rk("AT-100Metres-M", "zzz")$top)
cat(sprintf("\n%d of %d anchors hold\n", sum(res), length(res)))
