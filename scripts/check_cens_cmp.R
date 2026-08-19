# Censoring barely moves ORDERING (69.098 vs 69.127). Does it move LEVEL?
# That is the question the ladder never asked, and the one a displayed mark
# depends on.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
S <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-citiusverse/5a095d02-ce82-4b96-a703-6b96c0eb9c26/scratchpad/opt"
load1 <- function(tag) {
  h <- setDT(read_parquet(file.path(S, tag, sprintf("seqv3_history_%s.parquet", tag))))
  h <- h[seen == TRUE & rc == "final" & is.finite(perf) & is.finite(r_pre)]
  h[, resid := perf - r_pre]
  h[, r_pct := frank(r_pre) / .N, by = event_id]
  h[]
}
a <- load1("cens0"); b <- load1("cens03")
# plain threshold rather than a quoted expression: no eval needed
cmp <- function(x, lab, min_pct = 0) {
  y <- x[r_pct > min_pct & year(date) == 2026]
  data.table(arm = lab, n = nrow(y), bias_pct = round(100*mean(y$resid), 3),
             secs_118 = round(118*(exp(mean(y$resid))-1), 2))
}
cat("2026 FINALS LEVEL BIAS (positive = athlete outran their rating)\n\nall finalists:\n")
print(rbind(cmp(a, "cens 0"), cmp(b, "cens 0.3")))
cat("\ntop 5% of ratings within event — the athletes a page shows:\n")
print(rbind(cmp(a, "cens 0", .95), cmp(b, "cens 0.3", .95)))
cat("\ntop 1%:\n")
print(rbind(cmp(a, "cens 0", .99), cmp(b, "cens 0.3", .99)))

st <- function(tag) setDT(read_parquet(file.path(S, tag, sprintf("seqv2_state_%s.parquet", tag))))
d <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/data/form_display_final.parquet"))
aid <- d[event_id == "AT-800Metres-W" & grepl("Werro", athlete_name), athlete_id][1]
fm <- function(s) sprintf("%d:%05.2f", floor(s/60), s %% 60)
cat("\nWERRO's raw rating (no offset), by arm:\n")
for (tg in c("cens0","cens03")) {
  s <- st(tg); s[, athlete_id := as.character(athlete_id)]
  r <- s[athlete_id == aid & event_id == "AT-800Metres-W"]$R
  cat(sprintf("  %-9s %s\n", tg, if (length(r)) fm(exp(-r)) else "absent"))
}
cat("  (she ran 1:53.80 / 1:53.98 / 1:54.45 in her last three elite finals)\n")
