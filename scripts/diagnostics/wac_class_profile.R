# What each WAC competition class actually IS: size, depth, and who is in it.
#
# The scoring weights say an Olympic final is worth ten category F meets. That
# number came from Pete's judgement, not from the data, and it should at least
# be checked against what the classes look like. If category A meets are nearly
# as strong as Diamond League, weighting them 3 against 10 is arbitrary; if F is
# a different sport entirely, 1 against 10 may be generous.
#
# STRENGTH is measured as how good the marks are, standardised within event so
# a shot put and a marathon are comparable: for each mark, how many
# within-event standard deviations above that event's median it sits, on the
# oriented scale where higher is always better. Averaged per class, that is
# "how fast do people run at this kind of meet".
#
# It is a measure of the FIELD, not of the meet's organisation, and the two come
# apart: a Diamond League has a small elite field, a category F meet has a huge
# mixed one. Depth is reported separately as the field size and the spread
# within a race.
#
# Usage (arrow => PowerShell):
#   powershell -Command 'Rscript citiusdata/scripts/diagnostics/wac_class_profile.R'
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(data.table))
OUT <- here::here("citiusdata", "data")
source(here::here("citiusdata", "scripts", "_score_weights.R"))
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch <- ch[!is.na(tier) & !is.na(event_id) & is.finite(perf)]
ch[, date := as.Date(date)]

# Standardise within event so classes are comparable across events. Robust
# centring and scaling: a median and an IQR-based sd, because a few corrupt
# marks would otherwise drag the mean and inflate the spread.
ch[, `:=`(med = stats::median(perf, na.rm = TRUE),
          sd_e = stats::IQR(perf, na.rm = TRUE) / 1.349), by = event_id]
ch <- ch[is.finite(sd_e) & sd_e > 0]
ch[, z := (perf - med) / sd_e]

w <- wac_score_weights()
prof <- ch[, .(
  meets      = uniqueN(competition_id),
  races      = uniqueN(race_key),
  marks      = .N,
  events     = uniqueN(event_id),
  athletes   = uniqueN(athlete_id),
  per_race   = round(.N / uniqueN(race_key), 1),
  strength   = round(mean(z, na.rm = TRUE), 3),
  top_decile = round(stats::quantile(z, 0.9, na.rm = TRUE), 3),
  spread     = round(stats::sd(z, na.rm = TRUE), 3)
), by = tier]
prof[, weight := { j <- match(tier, names(w)); fifelse(is.na(j), 1, unname(w[j])) }]
setorder(prof, -strength)
cat("=== WAC class profile, whole corpus ===\n")
print(prof[, .(tier, weight, meets, races, marks, events, athletes, per_race,
               strength, top_decile, spread)])
cat("\nstrength is the mean mark in within-event standard deviations above that\n")
cat("event's median, on the oriented scale where higher is better. top_decile is\n")
cat("the 90th percentile of the same, i.e. how good the best are.\n")

cat("\n=== example meets in each class (most races first) ===\n")
nm <- if ("comp_name" %in% names(ch)) "comp_name" else NULL
if (is.null(nm)) {
  cat("(no comp_name column in this corpus vintage)\n")
} else {
  ex <- ch[!is.na(comp_name) & nzchar(comp_name),
           .(races = uniqueN(race_key)), by = .(tier, comp_name)]
  for (tr in prof$tier) {
    top <- head(ex[tier == tr][order(-races)], 4)
    if (nrow(top))
      cat(sprintf("  %-8s %s\n", tr,
                  paste(sprintf("%s (%d)", top$comp_name, top$races), collapse = " | ")))
  }
}

cat("\n=== does the weighting match the strength ordering? ===\n")
chk <- prof[tier %in% names(w)][order(-weight, -strength)]
print(chk[, .(tier, weight, strength, races)])
cs <- stats::cor(chk$weight, chk$strength, method = "spearman")
cat(sprintf("\nSpearman(weight, strength) = %.3f over %d classes\n", cs, nrow(chk)))
cat(if (cs > 0.6)
  "=> the weights track field strength: heavier classes really are the stronger fields.\n"
  else if (cs > 0.2)
  "=> loosely aligned. The weights encode WHAT WE CARE ABOUT more than what is\n   strongest, which is legitimate but should be said out loud.\n"
  else
  "=> the weights do NOT track strength. They are a statement of priorities, not\n   a measurement, and should not be defended as though they were measured.\n")
fwrite(prof, file.path(OUT, "wac_class_profile.csv"))
say("wrote wac_class_profile.csv")
