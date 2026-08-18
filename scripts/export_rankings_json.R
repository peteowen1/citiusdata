# Top 10 per event, as JSON for the model explainer page.
#
# Formatting happens HERE, not in the browser: the seconds/metres/points
# distinction and the m:ss.xx convention are properties of the event registry,
# and a second implementation in JavaScript is a second thing to get wrong. The
# page receives display strings plus the raw values it needs for sorting.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table)); library(jsonlite)

D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
TOPN <- as.integer(Sys.getenv("RANK_TOP_N", "10"))
d <- setDT(read_parquet(file.path(D, sprintf("form_display_%s.parquet", TAG))))
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family, unit)]
d <- merge(d, reg, by = "event_id", all.x = TRUE, suffixes = c("", ".reg"))
d <- d[!is.na(discipline) & is.finite(pred_mark)]
d[, athlete_id := as.character(athlete_id)]
# 6.1% of ranked athletes had no name in the display table. The competition
# cache carries athlete_name on every result row and covers 30 of the 36; the
# rest are shown as their id rather than blank, so a reader can at least tell
# one anonymous athlete from another and look the id up.
lk <- file.path(D, "athlete_name_lookup.rds")
if (file.exists(lk)) {
  nml <- readRDS(lk); nml[, athlete_id := as.character(athlete_id)]
  d <- merge(d, nml[, .(athlete_id, nm2 = athlete_name)], by = "athlete_id", all.x = TRUE)
  d[(is.na(athlete_name) | !nzchar(athlete_name)) & !is.na(nm2), athlete_name := nm2]
}
d[is.na(athlete_name) | !nzchar(athlete_name), athlete_name := paste0("Athlete ", athlete_id)]

# --- track vs field ----------------------------------------------------------
# Combined events sit with field by convention: they are scored in points and a
# reader looking for "field" expects to find the decathlon there.
# Three groups, not two. Putting the half marathon under "track" because it is
# a running event is the kind of tidy-looking wrongness a reader spots at once.
# Race walks split by SURFACE, not by family. "10,000 Metres Race Walk" is a
# TRACK event and "10 Kilometres Race Walk" is a road event - same distance,
# different races, and World Athletics keeps separate records for them. Putting
# both under road (as `family == "walk"` does) was wrong, and visibly so: the
# track 10,000m walk was sitting in the road section.
#
# The distinction is carried by the name: athletics uses METRES for track and
# KILOMETRES for road, which is exactly why both spellings exist.
d[, grp := fifelse(family %chin% c("sprint","middle","distance","hurdles"), "track",
            fifelse(family == "walk" & grepl("Metres", discipline), "track",
            fifelse(family %chin% c("road","walk"), "road", "field")))]

# --- a distance to sort by ---------------------------------------------------
# Parsed from the discipline name, which carries it explicitly ("800 Metres",
# "20 Kilometres Race Walk"). Field events have no distance, so they fall back
# to alphabetical within their group rather than being forced onto a scale that
# does not apply to them.
d[, dist_num := as.numeric(gsub(",", "", sub("^([0-9,.]+).*$", "\\1", discipline)))]
d[!grepl("^[0-9]", discipline), dist_num := NA_real_]
d[grepl("Kilometre|Mile", discipline) & is.finite(dist_num), dist_num := dist_num * 1000]
d[grepl("Mile", discipline) & is.finite(dist_num), dist_num := dist_num * 1.609]

# Marks over an hour need HOURS. Without this a 2:00:07 marathon renders as
# "120:07.28" - which looks like a number rather than a time and hides that the
# rating is implausible. Road events are exactly where the ratings need the most
# scrutiny, so a formatter that obscures them is worse than useless.
fmt <- function(mark, unit) {
  ifelse(is.na(mark), NA_character_,
    ifelse(unit == "seconds",
      ifelse(mark >= 3600,
             sprintf("%d:%02d:%02d", floor(mark / 3600),
                     floor((mark %% 3600) / 60), round(mark %% 60)),
      ifelse(mark >= 60,
             sprintf("%d:%05.2f", floor(mark / 60), mark %% 60),
             sprintf("%.2f", mark))),
      ifelse(unit == "points", format(round(mark), big.mark = ","),
             sprintf("%.2f", mark))))
}
# An event with a handful of ranked athletes cannot support a "top 10", and
# printing one implies a depth of competition that is not there. Dropped rather
# than shown with a caveat: the page is a ranking, and a ranking of three people
# is not a ranking. 11 events fall out, all minor (2000m steeplechase W, 5km
# race walk M, half marathon M each had 1-6).
# NOVELTY DISTANCES. Pete: "think 600m and 1000m can go". The principled rule
# is the one that catches them for a reason rather than by name: an event with
# ZERO T1_elite races has no elite competition to rank, so a top ten of it is a
# top ten of nobody in particular. Fourteen events qualify - 150m, 300m, 600m,
# 1000m, 2000m, 2000m steeplechase, weight throw, and the short race walks -
# and the ladder already flagged 300/600/1000m as the model's worst events.
#
# Overridable, because "no T1 race in OUR corpus" is a statement about coverage
# as well as about the sport: half marathon has only two and is plainly real.
DROP_ZERO_T1 <- Sys.getenv("RANK_DROP_ZERO_T1", "1") != "0"
if (DROP_ZERO_T1) {
  cg <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet")))
  cg[, competition_id := as.character(competition_id)]
  hh <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                           col_select = c("race_key", "event_id")))
  hh[, competition_id := tstrsplit(race_key, "[|]", keep = 1L)[[1]]]
  hh <- merge(hh, cg[, .(competition_id, meet_tier)], by = "competition_id", all.x = TRUE)
  t1 <- hh[, .(t1 = uniqueN(race_key[meet_tier == "T1_elite"])), by = event_id]
  zero <- t1[t1 == 0, event_id]
  if (length(zero)) {
    gone <- unique(d[event_id %chin% zero, .(discipline, sex)])
    cat(sprintf("dropping %d events with no T1_elite race: %s
", nrow(gone),
                paste(sprintf("%s %s", gone$discipline, gone$sex), collapse = "; ")))
    d <- d[!event_id %chin% zero]
  }
}

MINA <- as.integer(Sys.getenv("RANK_MIN_ATHLETES", "10"))
depth <- d[, .(n_ath = .N), by = event_id]
drop <- depth[n_ath < MINA]
if (nrow(drop)) cat(sprintf("dropping %d events with fewer than %d ranked athletes
",
                            nrow(drop), MINA))
d <- d[event_id %chin% depth[n_ath >= MINA, event_id]]
# --- athlete metadata: nationality, age, evidence -----------------------------
# From the profile harvest. World Athletics use IOC codes, which are NOT ISO
# 3166 - SUI is Switzerland, GER Germany, NED the Netherlands, RSA South Africa.
# A flag built by feeding IOC codes to a regional-indicator conversion would
# render a wrong flag rather than none, which is worse.
IOC2ISO <- c(
  ALG="DZ", ARM="AM", AUS="AU", BAH="BS", BEL="BE", BLR="BY", BOT="BW", BRA="BR",
  BRN="BH", BUL="BG", CAN="CA", CHN="CN", CMR="CM", COL="CO", CUB="CU", CZE="CZ",
  DMA="DM", DOM="DO", ECU="EC", ESP="ES", EST="EE", ETH="ET", FIN="FI", FRA="FR",
  GAM="GM", GBR="GB", GER="DE", GRE="GR", GRN="GD", HUN="HU", IND="IN", IRL="IE",
  ISR="IL", ITA="IT", IVB="VG", JAM="JM", JPN="JP", KAZ="KZ", KEN="KE", KOR="KR",
  LCA="LC", LTU="LT", LUX="LU", MAR="MA", MEX="MX", NED="NL", NGR="NG", NOR="NO",
  NZL="NZ", PAK="PK", PAN="PA", PER="PE", POL="PL", POR="PT", QAT="QA", ROU="RO",
  RSA="ZA", RUS="RU", SEN="SN", SLO="SI", SRB="RS", SRI="LK", SUI="CH", SVK="SK",
  SWE="SE", THA="TH", TTO="TT", TUN="TN", UGA="UG", UKR="UA", USA="US", UZB="UZ",
  VEN="VE", ZAM="ZM", ZIM="ZW", AUT="AT", DEN="DK", CRO="HR", TUR="TR", CHI="CL",
  PHI="PH", INA="ID", MAS="MY", SGP="SG", VIE="VN", EGY="EG", KSA="SA", UAE="AE",
  JOR="JO", LBN="LB", CYP="CY", MLT="MT", ISL="IS", LAT="LV", GEO="GE", AZE="AZ",
  MDA="MD", MKD="MK", ALB="AL", BIH="BA", MNE="ME", KOS="XK", LUX="LU", AND="AD",
  MON="MC", SMR="SM", LIE="LI", ERI="ER", ETH="ET", SUD="SD", TAN="TZ", RWA="RW",
  BDI="BI", DJI="DJ", SOM="SO", GHA="GH", CIV="CI", BUR="BF", MLI="ML", NIG="NE",
  TOG="TG", BEN="BJ", GUI="GN", ANG="AO", MOZ="MZ", NAM="NA", ZAF="ZA", LES="LS",
  SWZ="SZ", MRI="MU", SEY="SC", MAD="MG", MAW="MW", COD="CD", CGO="CG", GAB="GA",
  CHA="TD", CAF="CF", LBA="LY", MTN="MR", ARG="AR", URU="UY", PAR="PY", BOL="BO",
  CRC="CR", GUA="GT", HON="HN", ESA="SV", NCA="NI", CAY="KY", PUR="PR", BAR="BB",
  ANT="AG", SKN="KN", VIN="VC", BIZ="BZ", GUY="GY", SUR="SR", HAI="HT", JPN="JP",
  PRK="KP", MGL="MN", NEP="NP", BAN="BD", BHU="BT", MYA="MM", CAM="KH", LAO="LA",
  TPE="TW", HKG="HK", MAC="MO", FIJ="FJ", PNG="PG", SAM="WS", TGA="TO", VAN="VU",
  SOL="SB", KIR="KI", NRU="NR", TUV="TV", PLW="PW", FSM="FM", MHL="MH", COK="CK")
flag_of <- function(ioc) {
  iso <- unname(IOC2ISO[ioc])
  vapply(seq_along(iso), function(i) {
    if (is.na(iso[i]) || nchar(iso[i]) != 2) return("")
    ch <- utf8ToInt(substr(iso[i], 1, 1)) - 65L
    cl <- utf8ToInt(substr(iso[i], 2, 2)) - 65L
    if (ch < 0 || ch > 25 || cl < 0 || cl > 25) return("")
    intToUtf8(c(0x1F1E6L + ch, 0x1F1E6L + cl))
  }, character(1))
}
mf <- file.path(D, "athlete_meta.parquet")
if (file.exists(mf)) {
  am <- setDT(read_parquet(mf))[, .(athlete_id = as.character(athlete_id), country, birthdate)]
  d <- merge(d, am, by = "athlete_id", all.x = TRUE)
  ASOF <- max(d$last, na.rm = TRUE)
  d[, age := round(as.numeric(ASOF - birthdate) / 365.25, 1)]
  d[, flag := flag_of(country)]
  cat(sprintf("metadata joined: country on %.0f%% of ranked rows, age on %.0f%%
",
      100 * mean(!is.na(d[rk <= TOPN, country])), 100 * mean(!is.na(d[rk <= TOPN, age]))))
  miss <- unique(d[rk <= TOPN & !is.na(country) & flag == "", country])
  if (length(miss)) cat(sprintf("  no flag mapping for: %s
", paste(miss, collapse = ", ")))
} else {
  d[, `:=`(country = NA_character_, age = NA_real_, flag = "")]
  cat("athlete_meta.parquet not found - no nationality or age
")
}

setorder(d, event_id, rk)
top <- d[rk <= TOPN]
top[, `:=`(typical_s = fmt(pred_mark, unit), good_s = fmt(peak_mark, unit),
           # the mark the table is SORTED by. Without it the page shows `typical`
           # (from R) beside an order computed from R_ceil, and the two disagree
           # often enough to look broken - Bednarek's 19.67 below Lyles' 19.80.
           rank_s = fmt(rank_mark, unit))]

ev <- unique(d[, .(event_id, discipline, sex, family, grp, unit, dist_num)])
# Coverage flag. Combined events are the worst case: the men's decathlon is
# topped by 8,172 when world class is 8,500+, because the T1/T2 filter leaves
# the actual decathletes largely outside the corpus. Better to say so on the
# page than to let a reader assume the list is the world order.
lead <- d[rk == 1, .(event_id, lead_n = n_eff)]
ev <- merge(ev, lead, by = "event_id", all.x = TRUE)
ev[, thin := is.finite(lead_n) & lead_n < 6]
setorder(ev, grp, dist_num, discipline)
out <- lapply(seq_len(nrow(ev)), function(i) {
  e <- ev[i]
  a <- top[event_id == e$event_id]
  list(id = e$event_id, name = e$discipline, sex = e$sex, grp = e$grp,
       family = e$family, unit = e$unit, thin = unname(e$thin),
       dist = if (is.finite(e$dist_num)) e$dist_num else NULL,
       athletes = lapply(seq_len(nrow(a)), function(j) list(
         rk = a$rk[j], name = a$athlete_name[j],
         nat = if (is.na(a$country[j])) NULL else a$country[j],
         flag = if (is.na(a$flag[j]) || !nzchar(a$flag[j])) NULL else a$flag[j],
         age = if (is.na(a$age[j])) NULL else a$age[j],
         rating = a$rank_s[j],
         typical = a$typical_s[j],
         # NULL, not a number, where the good-day mark is suppressed: the page
         # must show an explicit dash rather than imply a missing value is zero
         good = if (is.na(a$good_s[j])) NULL else a$good_s[j],
         n = round(a$n_eff[j], 1))))
})
f <- file.path(D, "form_rankings.json")
write_json(out, f, auto_unbox = TRUE, null = "null", pretty = FALSE)
cat(sprintf("wrote %s\n  %d events (%d track, %d field), %s athlete rows, top %d each\n",
    f, nrow(ev), ev[grp == "track", .N], ev[grp == "field", .N],
    format(nrow(top), big.mark = ","), TOPN))
cat(sprintf("  good-day marks shown on %d of %d rows (suppressed below n_eff 8)\n",
    sum(!is.na(top$good_s)), nrow(top)))
cat(sprintf("  ratings as at %s\n", max(d$last, na.rm = TRUE)))
