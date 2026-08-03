# Build the matched calibration pair for the season/indoor A/B.
#
# The OFF arm is the DEPLOYED calibration, byte-for-byte. The ON arm is that
# same object with `$season` and `$indoor` attached and nothing else touched.
#
# Deliberately NOT done by re-running `calibrate()`. A full rebaseline also
# refits the wind coefficients and the round/tier precisions, so the arm
# difference would carry three changes at once -- the shared-vintage confound
# that once made six single-variable arms all score ~1.7% for the same wrong
# reason. Fitting only the two effects and attaching them to the existing file
# means the pair differs in exactly two list elements, which is asserted below
# rather than trusted.
#
# It is also 40 minutes cheaper and does not run the 480k-race decomposition,
# which OOM-killed on this machine when run alongside anything else.
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
library(data.table); library(arrow)
D <- "C:/dev/citiusverse/citiusdata/data"
SRC <- Sys.getenv("CITIUS_SEASON_SRC", "calibration_corpus_athfoul.rds")
say <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

off <- readRDS(file.path(D, SRC))
say("deployed calibration: ", SRC)
if (!is.null(off$season) || !is.null(off$indoor)) {
  stop("source already carries $season or $indoor; the OFF arm would not be ",
       "the deployed configuration and the A/B would understate the effect.")
}

say("reading corpus ...")
x <- as.data.table(read_parquet(
  file.path(D, "athletics_corpus.parquet"),
  col_select = c("athlete_id", "event_id", "date", "perf", "mark",
                 "venue_country", "indoor", "is_technical")))
say("rows: ", format(nrow(x), big.mark = ","))

# Same cleaning, same order as the pipeline: flagging is a grouped operation and
# must run before anything reads `perf`, not after.
x <- flag_implausible(x)
x <- x[!is.na(perf) & !is.na(event_id)]
say("after flagging: ", format(nrow(x), big.mark = ","))
say("venue_country non-missing: ",
    sprintf("%.1f%%", 100 * mean(!is.na(x$venue_country) & nzchar(x$venue_country))))

say("fitting season ...")
season <- fit_season_effect(x)
say("  cells: ", nrow(season), " over ", uniqueN(season$family), " famil(ies), ",
    "hemispheres: ", paste(sort(unique(season$hemi)), collapse = "/"))

say("fitting indoor ...")
indoor <- fit_indoor_effect(x)
say("  families: ", nrow(indoor))

if (!nrow(season)) {
  stop("no season cells fitted -- the ON arm would be identical to OFF and the ",
       "A/B would report a dead heat that reads as a null result.")
}

on <- off
on$season <- season
on$indoor <- indoor

# The pair must differ in exactly the two intended elements.
diff_names <- setdiff(union(names(on), names(off)), intersect(names(on), names(off)))
stopifnot(setequal(diff_names, c("season", "indoor")))
common <- intersect(names(on), names(off))
same <- vapply(common, function(n) isTRUE(all.equal(on[[n]], off[[n]])), logical(1))
if (!all(same)) stop("arms differ outside season/indoor in: ",
                     paste(common[!same], collapse = ", "))
say("verified: the arms differ in exactly $season and $indoor.")

saveRDS(off, file.path(D, "calibration_season_off.rds"))
saveRDS(on,  file.path(D, "calibration_season_on.rds"))
say("wrote calibration_season_off.rds and calibration_season_on.rds")

cat("\n--- season offsets, % of a mark (positive = sharper than own average) ---\n")
s <- copy(season)[, offset_pct := round(100 * offset, 3)]
print(dcast(s, family + hemi ~ month, value.var = "offset_pct"))
cat("\n--- indoor offsets, % of a mark ---\n")
print(copy(as.data.table(indoor))[, offset_pct := round(100 * offset, 3)][])

# Anchor check: the northern outdoor season peaks in summer. If May-July does not
# come out above September-October for the track families, the fit is wrong in a
# way the aggregate numbers will not show.
n_sum <- s[hemi == "N" & month %in% 5:7, mean(offset)]
n_aut <- s[hemi == "N" & month %in% 9:10, mean(offset)]
cat(sprintf("\nANCHOR northern May-Jul (%.3f%%) vs Sep-Oct (%.3f%%): %s\n",
            100 * n_sum, 100 * n_aut,
            if (isTRUE(n_sum > n_aut)) "PASS" else "FAIL - investigate before running the arm"))
