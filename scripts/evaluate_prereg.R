# Apply the pre-registered decision rules to the context arms, mechanically.
#
# docs/plans/PREREG-CONTEXT-ARMS.md fixes what counts as a win before any number
# exists. That is worth nothing if the rules are then applied by a reader who can
# see the results -- the whole failure mode being guarded against is a plausible
# story fitted to a number after the number arrives, and this session produced
# four of those.
#
# So the rules are code. This script prints a verdict per rule and one overall,
# and it does not know or care which outcome would be convenient.
#
# Usage:  Rscript scripts/evaluate_prereg.R
suppressMessages(devtools::load_all(here::here("citius")))
library(data.table)
source(here::here("citiusdata", "scripts", "_deployed.R"))
OUT <- here::here("citiusdata", "data")
HOLDOUT <- as.Date(Sys.getenv("CITIUS_SCORE_HOLDOUT", "2023-01-01"))
# Overridable so the script can be smoke-tested on arms that already exist,
# rather than first running for real at the end of a six-hour queue. Order is
# reference, candidate, layer-off.
ARMS <- strsplit(Sys.getenv("CITIUS_PREREG_ARMS",
  "backtest_ref3.rds,backtest_cevent.rds,backtest_noctx.rds"), ",")[[1]]
names(ARMS) <- c("ref3", "cevent", "noctx")

miss <- ARMS[!file.exists(file.path(OUT, ARMS))]
if (length(miss)) cli::cli_abort("Missing arm{?s}: {.file {miss}}")

# Same provenance rule score_arm.R enforces: an arm comparison is only an arm
# comparison if both arms saw the same data.
vint <- vapply(ARMS, function(f) {
  m <- readRDS(file.path(OUT, f))$meta
  paste(m$history_md5, if (identical(m$history_source, "store")) m$store_md5 else "", sep = "|")
}, character(1))
if (length(unique(vint)) > 1L) {
  cli::cli_abort(c("x" = "Arms were built on different history vintages.",
                   "*" = "{names(vint)}: {substr(vint, 1, 8)}"))
}

ch <- setDT(readRDS(file.path(OUT, "championship_results.rds")))
ch[, athlete_id := as.character(athlete_id)]
act <- ch[!is.na(mark) & !is.na(race_key) & !is.na(place) & place > 0,
          .(race_id = race_key, athlete_id, actual = mark, event_id, date, competition_id)]
cat_tbl <- setDT(arrow::read_parquet(file.path(OUT, "competition_catalogue.parquet")))
ev <- as.data.table(citius_events())[, .(event_id, orientation, family)]

load_one <- function(f, tag) {
  b <- readRDS(file.path(OUT, f))
  p <- merge(as.data.table(b$predictions)[, .(race_id, athlete_id = as.character(athlete_id),
                                              mark = median_mark, p_gold)],
             as.data.table(b$outcomes)[, .(race_id, athlete_id = as.character(athlete_id), hit)],
             by = c("race_id", "athlete_id"))
  setnames(p, c("mark", "p_gold"), paste0(tag, c("_mark", "_gold")))
  p
}
d <- load_one(ARMS[["ref3"]], "ref")
for (n in c("cevent", "noctx")) {
  o <- load_one(ARMS[[n]], n)
  d <- merge(d, o[, !"hit"], by = c("race_id", "athlete_id"))
}
d <- merge(d, act, by = c("race_id", "athlete_id"))
d <- merge(d, ev, by = "event_id")
d <- merge(d, cat_tbl[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
d <- d[meet_tier == "T1_elite" & date >= HOLDOUT]
if (!nrow(d)) cli::cli_abort("No T1 rows to evaluate.")

d[, t_perf := orientation * log(actual)]
for (n in c("ref", "cevent", "noctx")) {
  d[, (paste0(n, "_perf")) := orientation * log(get(paste0(n, "_mark")))]
}
# Centred within race: the metric is within-race differentiation, not level.
d[, t_c := t_perf - mean(t_perf), by = race_id]
for (n in c("ref", "cevent", "noctx")) {
  d[, (paste0(n, "_c")) := get(paste0(n, "_perf")) - mean(get(paste0(n, "_perf"))), by = race_id]
  d[, (paste0(n, "_ae")) := abs(get(paste0(n, "_c")) - t_c)]
}

d[, grp := fcase(event_id %like% "AT-400Metres", "400m/400mH",
                 family == "throw", "throws",
                 family == "middle", "middle",
                 default = "rest")]

# arm vs ref3, paired on the same predictions.
cmp <- function(arm, g = NULL) {
  x <- if (is.null(g)) d else d[grp == g]
  a <- x[[paste0(arm, "_ae")]]; r <- x$ref_ae
  if (length(a) < 30L) return(list(n = length(a), rel = NA_real_, p = NA_real_))
  list(n = length(a), rel = 100 * (mean(a) / mean(r) - 1),
       p = tryCatch(stats::t.test(a - r)$p.value, error = function(e) NA_real_))
}
brier <- function(arm) mean((d[[paste0(arm, "_gold")]] - d$hit)^2)

groups <- c("400m/400mH", "throws", "middle", "rest")
tab <- rbindlist(lapply(c("cevent", "noctx"), function(a)
  rbindlist(lapply(c(list(NULL), as.list(groups)), function(g) {
    r <- cmp(a, g)
    data.table(arm = a, group = if (is.null(g)) "ALL T1" else g,
               n = r$n, rel = round(r$rel, 3), p = signif(r$p, 3))
  }))))
cli::cli_h1("Centred marks MAE vs ref3 (negative = arm better)")
print(tab)

cli::cli_h2("gold Brier on T1")
gb <- data.table(arm = c("ref3", "cevent", "noctx"),
                 brier = round(vapply(c("ref", "cevent", "noctx"), brier, 0), 6))
gb[, vs_ref := round(100 * (brier / brier[arm == "ref3"] - 1), 3)]
print(gb)

# ---- the pre-registered rules, applied without discretion -------------------
verdict <- function(rule, pass, detail) {
  cli::cli_alert(paste0(if (isTRUE(pass)) "PASS  " else if (is.na(pass)) "N/A   " else "FAIL  ",
                        rule, "  -- ", detail))
  invisible(pass)
}
cli::cli_h1("Pre-registered rules (docs/plans/PREREG-CONTEXT-ARMS.md)")

g400 <- cmp("cevent", "400m/400mH")
gb_bad <- gb[arm == "cevent"]$vs_ref
r1 <- isTRUE(g400$rel < 0 && g400$p < 0.05) && isTRUE(gb_bad < 1)
verdict("R1 cevent adopted: 400m improves p<0.05 and gold Brier not degraded",
        r1, sprintf("400m %+.2f%% p=%.3g | gold Brier %+.2f%%", g400$rel, g400$p, gb_bad))

n_all <- cmp("noctx", NULL)
r2 <- isTRUE(n_all$rel < 0 && n_all$p < 0.05)
# Phrased so PASS means the layer earned its place. The rule is "if noctx wins,
# delete the layer", and printing that outcome as FAIL would read as the run
# having gone wrong when it is a clean result that happens to be inconvenient.
verdict("R2 the context layer earns its place (noctx does NOT beat ref3)",
        !r2, sprintf("noctx %+.2f%% p=%.3g%s", n_all$rel, n_all$p,
                     if (r2) "  ==> DELETE THE LAYER, do not refit" else ""))

gmid <- cmp("cevent", "middle")
r3 <- !(isTRUE(g400$rel < 0) && isTRUE(gmid$rel > 0 && gmid$p < 0.05))
verdict("R3 cevent must not buy the 400m by damaging middle distance",
        r3, sprintf("middle %+.2f%% p=%.3g", gmid$rel, gmid$p))

allg <- vapply(groups, function(g) abs(cmp("cevent", g)$rel), 0)
r4 <- any(allg > 0.2, na.rm = TRUE)
verdict("R4 cevent is not inert (>0.2% on some group)", r4,
        sprintf("max |rel| %.2f%% (%s)", max(allg, na.rm = TRUE),
                names(allg)[which.max(allg)]))

mv <- c(cevent = cmp("cevent", NULL)$rel, noctx = n_all$rel)
r5 <- !(all(abs(mv) > 0.3, na.rm = TRUE) && diff(range(mv, na.rm = TRUE)) < 0.2)
verdict("R5 arms not moving in lockstep (that would be a plumbing artefact)",
        r5, sprintf("cevent %+.2f%%, noctx %+.2f%%", mv[["cevent"]], mv[["noctx"]]))

cli::cli_h2("Anchors")
a1 <- isTRUE(cmp("noctx", "400m/400mH")$rel != 0)
n400 <- cmp("noctx", "400m/400mH"); nmid <- cmp("noctx", "middle")
a2 <- isTRUE(abs(n400$rel) > abs(nmid$rel))
verdict("A1 switching the layer off moves the 400m MORE than middle distance",
        a2, sprintf("400m %+.2f%% vs middle %+.2f%%", n400$rel, nmid$rel))
cli::cli_alert_info(
  "A2 (ref3 beats baseline on gold Brier/logloss on T1) is checked by score_arm.R,
   and A3 (middle stays a large model win) by diagnose_marks.R per arm.")

cli::cli_h1(if (r1 && r3 && r4 && r5 && !r2) "VERDICT: adopt cevent"
            else if (r2) "VERDICT: delete the context layer"
            else "VERDICT: cevent NOT adopted")
