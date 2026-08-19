# Elevation for every venue in the corpus, from GeoNames rather than by hand.
#
# WHY. Three separate checks - the wind curves, the venue effect, and whether
# venue_adj measures the place or the occasion - are all anchored on altitude,
# and all three are limited by the same thing: a reference set of 43 hand-typed
# cities, which matches 66 sprint/jump venue-family cells. That sample, not
# modelling capacity, is what stops those questions being decidable. GeoNames
# publishes an SRTM elevation for 235,416 populated places; the corpus has 3,493
# outdoor venue cities. This joins them.
#
# THE HARD PART IS NOT ELEVATION, IT IS WHICH CITY. "Birmingham" is a large city
# in England and a larger one in Alabama, and they are 130 m and 197 m. Getting
# that wrong does not fail loudly - it quietly adds noise to the anchor the other
# checks depend on, in the direction of finding nothing.
#
# The corpus carries a country on 100% of rows, but as IOC codes (GER, RSA, NED),
# which are not ISO and differ for about thirty countries. Rather than hand-type
# that mapping - hand-typed reference data is what put us here - it is LEARNED:
# cities whose name exists in exactly one country need no disambiguation at all,
# and those cities reveal which ISO country each IOC code means. The map is then
# asserted to be clean before it is used on the ambiguous ones.
#
# EVERY STEP IS VALIDATED AGAINST THE 43 HAND-TYPED VALUES at the end. If the
# geocoder cannot reproduce numbers a human looked up, it is not trustworthy for
# the 3,450 nobody has checked - and if it disagrees, the hand-typed set is
# audited too, because one wrong entry in static reference data predicts others.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
GEO <- Sys.getenv("GEONAMES_CITIES", "")
stopifnot("set GEONAMES_CITIES to the unzipped cities500.txt" =
            nzchar(GEO) && file.exists(GEO))

# --- 1. the venues we need ----------------------------------------------------
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("venue_city","venue_country","indoor",
                                        "scoreable","perf")))
c0 <- c0[scoreable == TRUE & is.finite(perf) & (is.na(indoor) | indoor == FALSE)]
c0 <- c0[!is.na(venue_city) & nzchar(venue_city) & !is.na(venue_country)]
ven <- c0[, .(marks = .N), by = .(venue_city, ioc = venue_country)]
# a city recorded under two countries is a data problem, not a geography one;
# keep the dominant reading and count how often that happens
ven[, tot := sum(marks), by = venue_city]
setorder(ven, venue_city, -marks)
dup <- ven[, .N, by = venue_city][N > 1]
ven <- ven[, .SD[1], by = venue_city]
cat(sprintf("venues: %s cities, %s outdoor marks | %d cities carry >1 country\n",
            format(nrow(ven), big.mark = ","), format(sum(ven$tot), big.mark = ","),
            nrow(dup)))
stopifnot("no venues loaded" = nrow(ven) > 1000)

# --- 2. the GeoNames index ----------------------------------------------------
# columns are positional and undocumented in the file itself; named here from the
# published readme. dem is the SRTM value and is always present; the elevation
# column is a surveyed figure and usually is not, so prefer it and fall back.
gn <- fread(GEO, sep = "\t", quote = "", header = FALSE, showProgress = FALSE,
            select = c(2, 3, 4, 5, 6, 9, 15, 16, 17),
            col.names = c("name","ascii","alt","lat","lon","iso2","pop","elev","dem"))
gn[, elev_m := fifelse(is.finite(elev) & elev != 0, as.numeric(elev), as.numeric(dem))]
gn <- gn[is.finite(elev_m)]
cat(sprintf("geonames: %s places with an elevation\n", format(nrow(gn), big.mark = ",")))
# A handful of entries carry no country (disputed or unassigned territory).
# Dropped rather than asserted away, but the SHARE is checked - if this suddenly
# removed most of the file, the column positions have shifted, which is the
# failure mode that matters for a positionally-parsed file with no header.
.n_all <- nrow(gn)
gn <- gn[nchar(iso2) == 2L]
cat(sprintf("  %s dropped for having no country code (%.2f%%)\n",
            format(.n_all - nrow(gn), big.mark = ","),
            100 * (1 - nrow(gn) / .n_all)))
stopifnot("geonames file looks wrong - column positions may have shifted" =
            nrow(gn) > 100000 && nrow(gn) / .n_all > 0.95)

# one row per (name, place), covering the native name, the ascii name and every
# published alternate - venue_city is sometimes "Bruxelles" and sometimes
# "Brussels", and both must resolve to the same 13 m.
key <- function(x) tolower(trimws(x))
idx <- rbindlist(list(
  gn[, .(nm = key(name),  iso2, pop, elev_m, lat, lon)],
  gn[nzchar(ascii) & ascii != name, .(nm = key(ascii), iso2, pop, elev_m, lat, lon)],
  gn[nzchar(alt), .(nm = key(unlist(strsplit(alt, ",", fixed = TRUE))),
                    iso2 = rep(iso2, lengths(strsplit(alt, ",", fixed = TRUE))),
                    pop  = rep(pop,  lengths(strsplit(alt, ",", fixed = TRUE))),
                    elev_m = rep(elev_m, lengths(strsplit(alt, ",", fixed = TRUE))),
                    lat = rep(lat, lengths(strsplit(alt, ",", fixed = TRUE))),
                    lon = rep(lon, lengths(strsplit(alt, ",", fixed = TRUE))))]
), use.names = TRUE)
idx <- idx[nzchar(nm)]
setorder(idx, nm, -pop)
cat(sprintf("name index: %s entries over %s distinct names\n",
            format(nrow(idx), big.mark = ","), format(uniqueN(idx$nm), big.mark = ",")))

# --- 3. learn IOC -> ISO2 from the cities that need no disambiguation ----------
# A name found in exactly one country cannot be got wrong. Those cities are free
# evidence about what each IOC code means, so the mapping is derived rather than
# typed. Weighted by marks so a busy venue outvotes a one-off.
ven[, nm := key(venue_city)]
solo <- idx[, .(countries = uniqueN(iso2)), by = nm][countries == 1]
u <- merge(ven, idx[nm %chin% solo$nm, .SD[1], by = nm][, .(nm, iso2)], by = "nm")
map <- u[, .(marks = sum(marks)), by = .(ioc, iso2)]
map[, share := marks / sum(marks), by = ioc]
setorder(map, ioc, -share)
best <- map[, .SD[1], by = ioc]
# A code whose evidence is split is a code we have not actually identified, so
# it is dropped rather than guessed at. Reported, because a large drop would mean
# the learning step is broken rather than merely cautious.
good <- best[share >= 0.60 & marks >= 50]
cat(sprintf("\nIOC -> ISO2 learned from %s unambiguous cities: %d codes, %d kept at >=60%% agreement\n",
            format(nrow(u), big.mark = ","), nrow(best), nrow(good)))
cat(sprintf("median agreement among kept codes: %.1f%%\n", 100 * median(good$share)))
stopifnot("learned too few country codes to be usable" = nrow(good) >= 60)
print(good[order(-marks)][seq_len(min(8L, .N)), .(ioc, iso2, marks, share = round(share, 3))])
# the mapping is DERIVED, so spot-check it against cases that are known to differ
chk <- good[ioc %chin% c("GER","RSA","NED","SUI","GRE","POR","DEN","JAM","USA","GBR")]
cat("\ncodes that differ from ISO3, as learned:\n")
print(chk[, .(ioc, iso2, share = round(share, 3))])
stopifnot("the learned map got a known code wrong" =
            all(good[ioc == "GER", iso2] == "DE", good[ioc == "RSA", iso2] == "ZA",
                good[ioc == "NED", iso2] == "NL", good[ioc == "GBR", iso2] == "GB"))

# --- 4. resolve every venue ---------------------------------------------------
ven <- merge(ven, good[, .(ioc, iso2)], by = "ioc", all.x = TRUE)
# within the right country take the largest place of that name: venue names in
# results feeds are the city, and the biggest same-named settlement in a country
# is what a results feed means by it
cand <- merge(ven[!is.na(iso2), .(venue_city, nm, ioc, iso2, tot)],
              idx, by = c("nm", "iso2"), allow.cartesian = TRUE)
setorder(cand, venue_city, -pop)
hit <- cand[, .SD[1], by = venue_city][, .(venue_city, iso2, alt_m = elev_m,
                                           lat, lon, pop, tot)]
hit[, how := "country+name"]
# fall back to a globally unique name when the country is unknown or has no match
miss <- ven[!venue_city %chin% hit$venue_city]
fb <- merge(miss[, .(venue_city, nm, tot)],
            idx[nm %chin% solo$nm, .SD[1], by = nm][, .(nm, iso2, elev_m, lat, lon, pop)],
            by = "nm")
if (nrow(fb)) {
  fb <- fb[, .(venue_city, iso2, alt_m = elev_m, lat, lon, pop, tot, how = "unique-name")]
  hit <- rbind(hit, fb)
}
setorder(hit, -tot)
cov_cities <- 100 * nrow(hit) / nrow(ven)
cov_marks  <- 100 * sum(hit$tot) / sum(ven$tot)
cat(sprintf("\nresolved %d of %d cities (%.1f%%), covering %.1f%% of outdoor marks\n",
            nrow(hit), nrow(ven), cov_cities, cov_marks))
print(hit[, .(cities = .N, marks = sum(tot)), by = how])
stopifnot("resolved too few marks to be worth shipping" = cov_marks >= 80)

# --- 5. the validation that decides whether any of this is usable -------------
# The 43 hand-typed cities from check_venue_effect.R. If the geocoder cannot
# reproduce numbers a human looked up, it cannot be trusted on the 3,450 nobody
# has checked.
ALT <- data.table(venue_city = c(
  "Mexico City","Toluca","Bogota","Bogota","Quito","La Paz","Cochabamba",
  "Addis Ababa","Nairobi","Eldoret","Iten","Asmara",
  "Johannesburg","Pretoria","Potchefstroom","Bloemfontein",
  "Denver","Colorado Springs","Albuquerque","Provo","Flagstaff","Boulder","El Paso",
  "Sestriere","Font-Romeu","Ifrane",
  "London","Eugene","Tokyo","Doha","Roma","Rome","Monaco","Paris","Bruxelles",
  "Brussels","Stockholm","Oslo","Budapest","Birmingham","Glasgow","Zurich","Zurich"),
  hand_m = c(
  2240, 2660, 2640, 2640, 2850, 3640, 2560,
  2355, 1795, 2100, 2400, 2325,
  1753, 1339, 1350, 1395,
  1609, 1839, 1619, 1387, 2106, 1655, 1140,
  2035, 1800, 1665,
  11, 130, 40, 10, 21, 21, 5, 35, 13,
  13, 28, 23, 102, 140, 26, 408, 408))
ALT <- unique(ALT, by = "venue_city")
val <- merge(ALT, hit[, .(venue_city, alt_m, iso2)], by = "venue_city")
val[, diff := alt_m - hand_m]
cat(sprintf("\n=== validation against %d of the %d hand-typed cities ===\n",
            nrow(val), nrow(ALT)))
cat(sprintf("correlation with the hand-typed values : %.4f\n",
            stats::cor(val$alt_m, val$hand_m)))
cat(sprintf("median absolute difference             : %.0f m\n",
            stats::median(abs(val$diff))))
cat(sprintf("within 100 m                           : %d of %d\n",
            sum(abs(val$diff) <= 100), nrow(val)))
val[, adiff := abs(diff)]          # setorder takes columns, not expressions
setorder(val, -adiff)
cat("\nlargest disagreements - these are the ones to look at by eye:\n")
print(val[seq_len(min(10L, .N)), .(venue_city, iso2, hand_m, geonames = round(alt_m),
                                   diff = round(diff))])
stopifnot("geonames does not reproduce the hand-typed elevations - do not ship" =
            stats::cor(val$alt_m, val$hand_m) > 0.95)

f <- file.path(D, "venue_elevation.parquet")
write_parquet(hit[, .(venue_city, iso2, alt_m, lat, lon, pop, marks = tot, how)], f)
cat(sprintf("\nwrote %s: %d venues, %.1f%% of outdoor marks (was 43 cities)\n",
            basename(f), nrow(hit), cov_marks))
