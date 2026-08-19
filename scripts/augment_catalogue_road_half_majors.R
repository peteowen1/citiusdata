# Put the World Athletics Label HALF MARATHONS into the competition catalogue.
#
# Same class of fix as augment_catalogue_road_majors.R, and the same principle:
# the elite halves were never mis-tiered, they were ABSENT. The engine inner-
# joins to the catalogue, so a competition with no entry is dropped before any
# tier is consulted.
#
# Measured before this script: the men's Half Marathon reached the engine with
# 949 rows from 26 competitions, against 28,095 rows and 1,504 competitions
# sitting in the corpus store; the women's, 438 rows from 20. Ras Al Khaimah,
# Copenhagen, Houston, Gold Coast, Delhi, Lisbon and Prague were all harvested
# and all uncatalogued. NO SCRAPING IS NEEDED - the names come from the cached
# competition_name_lookup.parquet the marathon fix already built.
#
# The genuine World Athletics (formerly IAAF) World Half Marathon Championships
# is deliberately NOT handled here: build_competition_catalogue.R's `world_other`
# rule already matches "World Half Marathon" and tiers it T1_elite. Everything
# this script adds is the LABEL circuit, which is T2_strong - a strong field, not
# a global championship.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
CAT <- file.path(D, "competition_catalogue.parquet")
LOOK <- file.path(D, "competition_name_lookup.parquet")
stopifnot("run augment_catalogue_road_majors.R first - it builds the name lookup" =
            file.exists(LOOK))

nm <- setDT(read_parquet(LOOK))
cat(sprintf("competition names available: %s\n", format(nrow(nm), big.mark = ",")))
cat0 <- setDT(read_parquet(CAT))
cat0[, competition_id := as.character(competition_id)]
cat(sprintf("catalogue before: %s competitions (%s road_label)\n",
            format(nrow(cat0), big.mark = ","),
            format(cat0[class == "road_label", .N], big.mark = ",")))

# --- the knowledge -----------------------------------------------------------
# World Athletics Label half marathons, matched on CITY plus an explicit half
# requirement. City rather than official title because sponsors change yearly:
# Delhi appears as Airtel, Vedanta and plain New Delhi; Houston as Aramco.
HALF_LABEL_CITY <- c("Copenhagen", "Ras Al Khaimah", "RAK", "Lisbon",
                     "New Delhi", "Delhi", "Gold Coast", "Valencia", "Prague",
                     "Barcelona", "Houston", "Berlin", "Cardiff", "Bogota",
                     "Marugame", "Napoli", "Naples", "Verona")
# WORD BOUNDARIES ARE NOT OPTIONAL HERE. Without \\b, "RAK" matches inside
# "Marrakesh" and "Napoli" inside "Indianapolis" - both happened on the first
# run of this script, which added 87 competitions against an estimate of 44.
# This is the same failure the !is_half() guard in augment_catalogue_road_majors.R
# exists to prevent: a bare substring quietly recruiting races nobody chose.
rx <- function(v) paste0("(\\b", paste(v, collapse = "\\b|\\b"), "\\b)")
# Inverse of the marathon script's exclusion: here a half is what we WANT, and
# the other short road distances sharing these city names are what we must keep
# out. A "Mini Marathon" or a 10K in Prague is not a label half marathon.
is_half  <- function(x) grepl("Half|Halb|Semi[- ]?Marat|Mezzo", x, ignore.case = TRUE)
is_other <- function(x) grepl("Mini|10 ?K|5 ?K|Relay|Marathon Relay|Ekiden|Quarter",
                              x, ignore.case = TRUE)

# SELF-REPAIRING. Drop any half marathon this script previously placed before
# recomputing, so a re-run corrects its own earlier rows instead of leaving them
# stranded. Without this the first run's bad matches would be permanent - they
# are in the catalogue, so `miss` no longer sees them.
# ONLY road_label. An earlier version of this line also swept `world_other`,
# which this script does NOT own - build_competition_catalogue.R assigns it, and
# so does augment_catalogue_coverage.R. The delete-then-readd cycle rebuilt those
# rows from three columns and nulled comp_name, year, results, athletes, events
# and strength on 16 genuine World Half Marathon Championship editions in the
# live catalogue. A "self-repair" must only repair its own rows.
mine <- cat0[class == "road_label" &
             competition_id %chin% nm[is_half(competition), competition_id]]
if (nrow(mine)) {
  cat(sprintf("removing %d previously-placed half marathon(s) to recompute\n",
              nrow(mine)))
  cat0 <- cat0[!competition_id %chin% mine$competition_id]
}

miss <- nm[!competition_id %chin% cat0$competition_id]
cand <- miss[is_half(competition) & !is_other(competition) &
             grepl(rx(HALF_LABEL_CITY), competition, ignore.case = TRUE)]
# A genuine WORLD championship half is not a label race and must not be tiered
# T2 because its host city is on the list above - the first run put "New Delhi
# IAAF World Half Marathon Championships" in as T2_strong, demoting a global
# championship to a city road race.
#
# But "Championships" ALONE is far too loose, and putting it in this pattern
# tiered 170 NATIONAL championships - Latvian, Cyprus, Algerian, Russian - as
# T1_elite, i.e. level with the Olympics. National championships are a different
# question with a different tier and they are deliberately OUT OF SCOPE here:
# this script covers the World Athletics label circuit and the world
# championship, nothing else.
is_champs <- function(x)
  grepl("(IAAF |World Athletics |World )(Half Marathon|Road Running) Championships",
        x, ignore.case = TRUE)
is_natl <- function(x) grepl("Championships", x, ignore.case = TRUE) & !is_champs(x)
champs <- miss[is_half(competition) & !is_other(competition) & is_champs(competition)]
add <- rbind(cand[!is_champs(competition) & !is_natl(competition)], champs, fill = TRUE)
add <- unique(add, by = "competition_id")
if (!nrow(add)) { cat("nothing to add\n"); quit(status = 0) }
add[, `:=`(class     = fifelse(is_champs(competition), "world_other", "road_label"),
           meet_tier = fifelse(is_champs(competition), "T1_elite", "T2_strong"))]

# ANCHOR CHECK before writing: every added competition must actually be a half.
# A full marathon reaching this set would be tiered as a half marathon's peer,
# which is the exact mistake the marathon script's !is_half() guard exists to
# prevent - and which it applies to is_wmm but not is_plat.
bad <- add[!is_half(competition)]
stopifnot("a non-half competition reached the add set" = nrow(bad) == 0,
          "an added id is already catalogued" =
            !any(add$competition_id %chin% cat0$competition_id))
cat(sprintf("\nadding %s half marathons\n", format(nrow(add), big.mark = ",")))
print(add[, .(competitions = .N, first = min(date), last = max(date)),
          by = .(class, meet_tier)])
cat("\nnamed competitions being added (top 25 by edition count):\n")
print(add[, .N, by = competition][order(-N)][1:min(25, .N)])

# Carry the NAME across. Without this every row this script adds lands with
# comp_name NA, which is why 203 of 203 road_label rows in the live catalogue
# are anonymous - invisible to every later audit or spot-check that reports a
# competition by name.
new <- data.table(competition_id = add$competition_id, class = add$class,
                  meet_tier = add$meet_tier)
if ("comp_name" %in% names(cat0)) new[, comp_name := add$competition]
for (cn in setdiff(names(cat0), names(new))) new[, (cn) := NA]
out <- rbind(cat0, new[, names(cat0), with = FALSE])
stopifnot("duplicate competition ids" = !anyDuplicated(out$competition_id),
          "every added row must carry a tier" =
            !any(is.na(out$meet_tier[out$class == "road_label"])),
          # every failure of the first two runs, now asserted rather than hoped for
          "a substring match recruited a city nobody listed" =
            !any(grepl("Marrakesh|Marrakech|Indianapolis", add$competition, ignore.case = TRUE)),
          "a world championship was tiered as a label race" =
            !any(is_champs(add$competition) & add$meet_tier != "T1_elite"),
          "a national championship was tiered T1_elite" =
            !any(add$meet_tier == "T1_elite" &
                 !grepl("World|IAAF", add$competition, ignore.case = TRUE)),
          "national championships are out of scope for this script" =
            !any(is_natl(add$competition)),
          # a T1 addition is a global championship: there is one per year at most,
          # so a large count means the pattern has gone loose again
          "too many T1 additions to be world championships" =
            sum(add$meet_tier == "T1_elite") <= 30)
if (!file.exists(paste0(CAT, ".bak"))) file.copy(CAT, paste0(CAT, ".bak"))
# arrow memory-maps a parquet it has read, so writing back to the same path
# fails with "user-mapped section open". Write beside it, release, then replace.
tmp <- paste0(CAT, ".tmp")
n_after <- nrow(out); n_added <- nrow(add)
write_parquet(out, tmp)
rm(cat0, nm, miss, add, new, out); invisible(gc())
ok <- file.copy(tmp, CAT, overwrite = TRUE)
stopifnot("could not replace the catalogue" = isTRUE(ok))
unlink(tmp)
cat(sprintf("\ncatalogue after: %s competitions (+%s)\n",
            format(n_after, big.mark = ","), format(n_added, big.mark = ",")))
cat("This changes nothing until form_ratings.R is re-run - the catalogue is\n")
cat("joined at engine time, not baked into the corpus.\n")
