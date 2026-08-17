# Put the World Marathon Majors into the competition catalogue.
#
# WHY THIS IS A KNOWLEDGE FIX, NOT A MEASUREMENT ONE.
# build_competition_catalogue.R already tiers `road_race` by measured strength,
# and its own comment gives the principle: "We do not need a statistic to
# discover that the Olympic Games is the top tier of athletics - that is
# knowledge, and forcing it through an estimator only gave the estimator a
# chance to be wrong." London, Berlin, Boston, Chicago, New York and Tokyo are
# the same kind of knowledge.
#
# But they were never mis-tiered - they were ABSENT. 76% of road rows have no
# catalogue entry at all (157,551 rows across 6,093 competitions), and the
# engine inner-joins to the catalogue, so those rows are dropped before any tier
# is consulted. 275 uncatalogued marathon competitions have a sub-2:10 winner.
#
# The names come from the ATHLETE cache, which carries `competition` on every
# result row - 356,029 named competition-events across 32,073 competitions,
# already on disk. No scraping.
#
# The catalogue is joined at ENGINE time, not baked into the corpus, so this
# needs a catalogue update and a re-run, not a corpus rebuild.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
CAT <- file.path(D, "competition_catalogue.parquet")
LOOK <- file.path(D, "competition_name_lookup.parquet")

# --- names from the athlete cache, cached so the 87k-file scan happens once ---
if (!file.exists(LOOK)) {
  f <- list.files(file.path(D, "ath_athlete_cache"), full.names = TRUE)
  cat(sprintf("building competition name lookup from %s athlete files...\n",
              format(length(f), big.mark = ",")))
  nm <- rbindlist(lapply(f, function(p) {
    x <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.null(x) || !all(c("competition_id", "competition") %in% names(x))) return(NULL)
    y <- as.data.table(x)
    y <- y[!is.na(competition) & nzchar(competition),
           .(competition_id = as.character(competition_id), competition, date,
             venue_country = if ("venue_country" %in% names(y)) venue_country else NA_character_)]
    unique(y, by = "competition_id")
  }), fill = TRUE)
  nm <- nm[, .(competition = competition[1], date = min(date),
               venue_country = venue_country[1]), by = competition_id]
  write_parquet(nm, LOOK)
} else nm <- setDT(read_parquet(LOOK))
cat(sprintf("competition names available: %s\n", format(nrow(nm), big.mark = ",")))

cat0 <- setDT(read_parquet(CAT))
cat0[, competition_id := as.character(competition_id)]
cat(sprintf("catalogue before: %s competitions (%s road_race)\n",
            format(nrow(cat0), big.mark = ","), format(cat0[class == "road_race", .N], big.mark = ",")))

# --- the knowledge -----------------------------------------------------------
# Abbott World Marathon Majors, plus Sydney which joined for 2025. Matched on
# name because sponsor prefixes change year to year (Virgin Money -> TCS London)
# while the city does not.
# Matched on CITY plus "Marathon", not on the official title. Sponsors change
# and so do the titles: the New York race appears as "New York Marathon",
# "TCS New York Marathon" AND "TCS New York City Marathon", and an exact-title
# match caught only the third. Pete spotted the gap from the counts - New York
# showing four editions with the most recent in 2019, for a race run annually.
WMM_CITY <- c("London", "Berlin", "Boston", "Chicago", "New York", "NYC",
              "Tokyo", "Sydney")
# World Athletics Platinum Label road races: elite fields, below the majors.
PLATINUM <- c("Valencia", "Amsterdam Marathon", "Rotterdam", "Marathon de Paris",
              "Frankfurt Marathon", "Dubai Marathon", "Houston Marathon",
              "Osaka Women's Marathon", "Nagoya Women's Marathon", "Seville Marathon",
              "Xiamen", "Hangzhou Marathon", "Osaka Marathon")
rx <- function(v) paste0("(", paste(v, collapse = "|"), ")")

miss <- nm[!competition_id %chin% cat0$competition_id]
# A half marathon is a different event and must not be tiered as the major.
# "Halb" catches the German spelling, which "Berlin.*Marathon" would otherwise
# sweep up via "Generali Berliner Halbmarathon".
is_half <- function(x) grepl("Half|Halb|Semi[- ]|Mini|10 ?K|5 ?K|Relay", x, ignore.case = TRUE)
miss[, is_wmm  := grepl(rx(WMM_CITY), competition, ignore.case = TRUE) &
                  grepl("Marathon", competition, ignore.case = TRUE) &
                  !is_half(competition)]
# !is_half() belongs here too, not only on is_wmm. Without it the bare city names
# in PLATINUM ("Valencia", "Xiamen", "Rotterdam") match that city's HALF marathon
# and tier it as a full-marathon label race. That is how the three Valencia Half
# Marathon ids entered the catalogue - by accident rather than by decision. The
# label halves are handled deliberately in augment_catalogue_road_half_majors.R.
miss[, is_plat := !is_wmm & grepl(rx(PLATINUM), competition, ignore.case = TRUE) &
                  !is_half(competition)]
add <- miss[is_wmm | is_plat]
if (!nrow(add)) { cat("nothing to add\n"); quit(status = 0) }

add[, `:=`(class = fifelse(is_wmm, "marathon_major", "road_label"),
           meet_tier = fifelse(is_wmm, "T1_elite", "T2_strong"))]
cat(sprintf("\nadding %s competitions: %s majors (T1), %s label races (T2)\n",
            format(nrow(add), big.mark = ","),
            format(add[is_wmm == TRUE, .N], big.mark = ","), format(add[is_plat == TRUE, .N], big.mark = ",")))
print(add[, .(competitions = .N, first = min(date), last = max(date)),
          by = .(class, meet_tier)])
cat("\nnamed competitions being added (top 20 by count):\n")
print(add[, .N, by = .(competition, meet_tier)][order(-N)][1:min(20, .N)])

# Carry the NAME across - without it every added row is anonymous in the
# catalogue (54 of 54 marathon_major rows currently have comp_name NA).
new <- data.table(competition_id = add$competition_id, class = add$class,
                  meet_tier = add$meet_tier)
if ("comp_name" %in% names(cat0)) new[, comp_name := add$competition]
for (cn in setdiff(names(cat0), names(new))) new[, (cn) := NA]
out <- rbind(cat0, new[, names(cat0), with = FALSE])
stopifnot("duplicate competition ids" = !anyDuplicated(out$competition_id),
          "every added row must carry a tier" = !any(is.na(out$meet_tier[out$class %chin% c("marathon_major","road_label")])))
if (!file.exists(paste0(CAT, ".bak"))) file.copy(CAT, paste0(CAT, ".bak"))
# arrow memory-maps a parquet it has read, so writing back to the same path
# fails with "user-mapped section open". Write beside it, release the mapping,
# then replace.
tmp <- paste0(CAT, ".tmp")
n_after <- nrow(out)          # captured before rm(), which the final cat() needs
write_parquet(out, tmp)
rm(cat0, nm, miss, add, new, out); invisible(gc())
ok <- file.copy(tmp, CAT, overwrite = TRUE)
stopifnot("could not replace the catalogue" = isTRUE(ok))
unlink(tmp)
cat(sprintf("\ncatalogue after: %s competitions (backup at competition_catalogue.parquet.bak)\n",
            format(n_after, big.mark = ",")))
