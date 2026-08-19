# Venue elevations, from the geocoded table with the hand-typed set as fallback.
#
# WHY THIS EXISTS. build_venue_elevation.R produced 3,101 geocoded venues
# covering 91.8% of outdoor marks, and for several hours nothing read it - every
# consumer still used the 43 hand-typed cities it was built to replace. A
# validated artefact that nothing consumes is the same as not having built it,
# and this repo has done that before. One loader, sourced by every consumer.
#
# THE FALLBACK IS LOUD, NOT SILENT. If the geocoded table is missing, this
# returns the hand-typed 43 and says so, because a check that quietly runs on 43
# cities while reporting itself as the 3,101-venue version would be worse than
# one that fails.
#
# The hand-typed set is retained deliberately: it is the validation target for
# the geocoder (correlation 0.9994, median difference 9 m), so deleting it would
# remove the only independent check on 3,101 automatically resolved elevations.
venue_elevation <- function(D = here::here("citiusdata", "data"),
                            quiet = FALSE) {
  # Elevations in metres, hand-checked. Also the geocoder's validation target.
  ALT <- data.table::data.table(venue_city = c(
    "Mexico City","Toluca","Bogota","Bogota","Quito","La Paz","Cochabamba",
    "Addis Ababa","Nairobi","Eldoret","Iten","Asmara",
    "Johannesburg","Pretoria","Potchefstroom","Bloemfontein",
    "Denver","Colorado Springs","Albuquerque","Provo","Flagstaff","Boulder","El Paso",
    "Sestriere","Font-Romeu","Ifrane",
    "London","Eugene","Tokyo","Doha","Roma","Rome","Monaco","Paris","Bruxelles",
    "Brussels","Stockholm","Oslo","Budapest","Birmingham","Glasgow","Zurich","Zurich"),
    alt_m = c(
    2240, 2660, 2640, 2640, 2850, 3640, 2560,
    2355, 1795, 2100, 2400, 2325,
    1753, 1339, 1350, 1395,
    1609, 1839, 1619, 1387, 2106, 1655, 1140,
    2035, 1800, 1665,
    11, 130, 40, 10, 21, 21, 5, 35, 13,
    13, 28, 23, 102, 140, 26, 408, 408))
  ALT <- unique(ALT, by = "venue_city")

  f <- file.path(D, "venue_elevation.parquet")
  if (!file.exists(f)) {
    if (!quiet)
      cat(sprintf(paste0("venue elevation: geocoded table NOT FOUND, falling back to ",
                         "%d hand-typed cities.\n  Run build_venue_elevation.R ",
                         "(needs GEONAMES_CITIES) to widen this.\n"), nrow(ALT)))
    return(ALT[, .(venue_city, alt_m, src = "hand")])
  }
  g <- data.table::setDT(arrow::read_parquet(f))
  stopifnot("venue_elevation.parquet has no alt_m column" = "alt_m" %chin% names(g),
            "venue_elevation.parquet is implausibly small" = nrow(g) > 500)
  g <- g[is.finite(alt_m), .(venue_city, alt_m, src = "geocoded")]
  # Hand-typed entries win where they disagree: a human looked those up, and they
  # are the set the geocoder was validated against.
  out <- rbind(ALT[, .(venue_city, alt_m, src = "hand")],
               g[!venue_city %chin% ALT$venue_city])
  if (!quiet)
    cat(sprintf("venue elevation: %s venues (%s geocoded + %d hand-typed)\n",
                format(nrow(out), big.mark = ","),
                format(sum(out$src == "geocoded"), big.mark = ","),
                sum(out$src == "hand")))
  out[]
}
