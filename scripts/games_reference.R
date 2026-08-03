# Reference tables for the multi-sport games harvest: which editions exist,
# their Wikipedia slugs, hosts, and a fallback participating-nation count.
#
# `nations_count_map` is HAND-MAINTAINED and is therefore treated as a
# cross-check, not as truth. `harvest_participating_nations.R` scrapes the
# count from each edition's infobox and writes
# `data/games_participating_nations.csv`; the analysis prefers the scraped
# value and falls back to this map only where the scrape fails. Any
# disagreement between the two is reported rather than silently resolved.

series_years <- list(
  olympics_summer = c(1896, 1900, 1904, 1908, 1912, 1920, 1924, 1928, 1932, 1936,
                      1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984,
                      1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024),
  olympics_winter = c(1924, 1928, 1932, 1936, 1948, 1952, 1956, 1960, 1964, 1968,
                      1972, 1976, 1980, 1984, 1988, 1992, 1994, 1998, 2002, 2006,
                      2010, 2014, 2018, 2022, 2026),
  asian_games     = c(1951, 1954, 1958, 1962, 1966, 1970, 1974, 1978, 1982, 1986,
                      1990, 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022),
  panam_games     = c(1951, 1955, 1959, 1963, 1967, 1971, 1975, 1979, 1983, 1987,
                      1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023),
  european_games  = c(2015, 2019, 2023)
)

# Series whose article titles changed over time need explicit slugs.
cw_slugs <- list(
  list(1930, "1930_British_Empire_Games"), list(1934, "1934_British_Empire_Games"),
  list(1938, "1938_British_Empire_Games"), list(1950, "1950_British_Empire_Games"),
  list(1954, "1954_British_Empire_and_Commonwealth_Games"),
  list(1958, "1958_British_Empire_and_Commonwealth_Games"),
  list(1962, "1962_British_Empire_and_Commonwealth_Games"),
  list(1966, "1966_British_Empire_and_Commonwealth_Games"),
  list(1970, "1970_British_Commonwealth_Games"), list(1974, "1974_British_Commonwealth_Games"),
  list(1978, "1978_Commonwealth_Games"), list(1982, "1982_Commonwealth_Games"),
  list(1986, "1986_Commonwealth_Games"), list(1990, "1990_Commonwealth_Games"),
  list(1994, "1994_Commonwealth_Games"), list(1998, "1998_Commonwealth_Games"),
  list(2002, "2002_Commonwealth_Games"), list(2006, "2006_Commonwealth_Games"),
  list(2010, "2010_Commonwealth_Games"), list(2014, "2014_Commonwealth_Games"),
  list(2018, "2018_Commonwealth_Games"), list(2022, "2022_Commonwealth_Games"),
  list(2026, "2026_Commonwealth_Games")
)

afr_slugs <- list(
  list(1965, "1965_All-Africa_Games"), list(1973, "1973_All-Africa_Games"),
  list(1978, "1978_All-Africa_Games"), list(1987, "1987_All-Africa_Games"),
  list(1991, "1991_All-Africa_Games"), list(1995, "1995_All-Africa_Games"),
  list(1999, "1999_All-Africa_Games"), list(2003, "2003_All-Africa_Games"),
  list(2007, "2007_All-Africa_Games"), list(2011, "2011_All-Africa_Games"),
  list(2015, "2015_African_Games"),    list(2019, "2019_African_Games"),
  list(2023, "2023_African_Games")
)

pac_slugs <- list(
  list(1963, "1963_South_Pacific_Games"), list(1966, "1966_South_Pacific_Games"),
  list(1969, "1969_South_Pacific_Games"), list(1971, "1971_South_Pacific_Games"),
  list(1975, "1975_South_Pacific_Games"), list(1979, "1979_South_Pacific_Games"),
  list(1983, "1983_South_Pacific_Games"), list(1987, "1987_South_Pacific_Games"),
  list(1991, "1991_South_Pacific_Games"), list(1995, "1995_South_Pacific_Games"),
  list(1999, "1999_South_Pacific_Games"), list(2003, "2003_South_Pacific_Games"),
  list(2007, "2007_Pacific_Games"),       list(2011, "2011_Pacific_Games"),
  list(2015, "2015_Pacific_Games"),       list(2019, "2019_Pacific_Games"),
  list(2023, "2023_Pacific_Games")
)

#' All editions as a flat list of list(year, slug, games).
all_editions <- function() {
  out <- list()
  add <- function(y, s, g) out[[length(out) + 1]] <<- list(y, s, g)
  for (y in series_years$olympics_summer) add(y, sprintf("%d_Summer_Olympics", y), "olympics_summer")
  for (y in series_years$olympics_winter) add(y, sprintf("%d_Winter_Olympics", y), "olympics_winter")
  for (it in cw_slugs)  add(it[[1]], it[[2]], "commonwealth")
  for (y in series_years$asian_games)     add(y, sprintf("%d_Asian_Games", y), "asian_games")
  for (y in series_years$panam_games)     add(y, sprintf("%d_Pan_American_Games", y), "panam_games")
  for (it in afr_slugs) add(it[[1]], it[[2]], "african_games")
  for (y in series_years$european_games)  add(y, sprintf("%d_European_Games", y), "european_games")
  for (it in pac_slugs) add(it[[1]], it[[2]], "pacific_games")
  out
}

hosts_map <- list(
  # Summer Olympics
  "olympics_summer_1896" = "Athens, Greece", "olympics_summer_1900" = "Paris, France",
  "olympics_summer_1904" = "St. Louis, United States", "olympics_summer_1908" = "London, Great Britain",
  "olympics_summer_1912" = "Stockholm, Sweden", "olympics_summer_1920" = "Antwerp, Belgium",
  "olympics_summer_1924" = "Paris, France", "olympics_summer_1928" = "Amsterdam, Netherlands",
  "olympics_summer_1932" = "Los Angeles, United States", "olympics_summer_1936" = "Berlin, Germany",
  "olympics_summer_1948" = "London, Great Britain", "olympics_summer_1952" = "Helsinki, Finland",
  "olympics_summer_1956" = "Melbourne, Australia", "olympics_summer_1960" = "Rome, Italy",
  "olympics_summer_1964" = "Tokyo, Japan", "olympics_summer_1968" = "Mexico City, Mexico",
  "olympics_summer_1972" = "Munich, West Germany", "olympics_summer_1976" = "Montreal, Canada",
  "olympics_summer_1980" = "Moscow, Soviet Union", "olympics_summer_1984" = "Los Angeles, United States",
  "olympics_summer_1988" = "Seoul, South Korea", "olympics_summer_1992" = "Barcelona, Spain",
  "olympics_summer_1996" = "Atlanta, United States", "olympics_summer_2000" = "Sydney, Australia",
  "olympics_summer_2004" = "Athens, Greece", "olympics_summer_2008" = "Beijing, China",
  "olympics_summer_2012" = "London, Great Britain", "olympics_summer_2016" = "Rio de Janeiro, Brazil",
  "olympics_summer_2020" = "Tokyo, Japan", "olympics_summer_2024" = "Paris, France",

  # Winter Olympics
  "olympics_winter_1924" = "Chamonix, France", "olympics_winter_1928" = "St. Moritz, Switzerland",
  "olympics_winter_1932" = "Lake Placid, United States", "olympics_winter_1936" = "Garmisch-Partenkirchen, Germany",
  "olympics_winter_1948" = "St. Moritz, Switzerland", "olympics_winter_1952" = "Oslo, Norway",
  "olympics_winter_1956" = "Cortina d'Ampezzo, Italy", "olympics_winter_1960" = "Squaw Valley, United States",
  "olympics_winter_1964" = "Innsbruck, Austria", "olympics_winter_1968" = "Grenoble, France",
  "olympics_winter_1972" = "Sapporo, Japan", "olympics_winter_1976" = "Innsbruck, Austria",
  "olympics_winter_1980" = "Lake Placid, United States", "olympics_winter_1984" = "Sarajevo, Yugoslavia",
  "olympics_winter_1988" = "Calgary, Canada", "olympics_winter_1992" = "Albertville, France",
  "olympics_winter_1994" = "Lillehammer, Norway", "olympics_winter_1998" = "Nagano, Japan",
  "olympics_winter_2002" = "Salt Lake City, United States", "olympics_winter_2006" = "Turin, Italy",
  "olympics_winter_2010" = "Vancouver, Canada", "olympics_winter_2014" = "Sochi, Russia",
  "olympics_winter_2018" = "Pyeongchang, South Korea", "olympics_winter_2022" = "Beijing, China",
  "olympics_winter_2026" = "Milano-Cortina, Italy",

  # Commonwealth Games
  "commonwealth_1930" = "Hamilton, Canada", "commonwealth_1934" = "London, England",
  "commonwealth_1938" = "Sydney, Australia", "commonwealth_1950" = "Auckland, New Zealand",
  "commonwealth_1954" = "Vancouver, Canada", "commonwealth_1958" = "Cardiff, Wales",
  "commonwealth_1962" = "Perth, Australia", "commonwealth_1966" = "Kingston, Jamaica",
  "commonwealth_1970" = "Edinburgh, Scotland", "commonwealth_1974" = "Christchurch, New Zealand",
  "commonwealth_1978" = "Edmonton, Canada", "commonwealth_1982" = "Brisbane, Australia",
  "commonwealth_1986" = "Edinburgh, Scotland", "commonwealth_1990" = "Auckland, New Zealand",
  "commonwealth_1994" = "Victoria, Canada", "commonwealth_1998" = "Kuala Lumpur, Malaysia",
  "commonwealth_2002" = "Manchester, England", "commonwealth_2006" = "Melbourne, Australia",
  "commonwealth_2010" = "Delhi, India", "commonwealth_2014" = "Glasgow, Scotland",
  "commonwealth_2018" = "Gold Coast, Australia", "commonwealth_2022" = "Birmingham, England",
  "commonwealth_2026" = "Glasgow, Scotland",

  # Asian Games
  "asian_games_1951" = "New Delhi, India", "asian_games_1954" = "Manila, Philippines",
  "asian_games_1958" = "Tokyo, Japan", "asian_games_1962" = "Jakarta, Indonesia",
  "asian_games_1966" = "Bangkok, Thailand", "asian_games_1970" = "Bangkok, Thailand",
  "asian_games_1974" = "Tehran, Iran", "asian_games_1978" = "Bangkok, Thailand",
  "asian_games_1982" = "New Delhi, India", "asian_games_1986" = "Seoul, South Korea",
  "asian_games_1990" = "Beijing, China", "asian_games_1994" = "Hiroshima, Japan",
  "asian_games_1998" = "Bangkok, Thailand", "asian_games_2002" = "Busan, South Korea",
  "asian_games_2006" = "Doha, Qatar", "asian_games_2010" = "Guangzhou, China",
  "asian_games_2014" = "Incheon, South Korea", "asian_games_2018" = "Jakarta-Palembang, Indonesia",
  "asian_games_2022" = "Hangzhou, China",

  # Pan American Games
  "panam_games_1951" = "Buenos Aires, Argentina", "panam_games_1955" = "Mexico City, Mexico",
  "panam_games_1959" = "Chicago, United States", "panam_games_1963" = "Sao Paulo, Brazil",
  "panam_games_1967" = "Winnipeg, Canada", "panam_games_1971" = "Cali, Colombia",
  "panam_games_1975" = "Mexico City, Mexico", "panam_games_1979" = "San Juan, Puerto Rico",
  "panam_games_1983" = "Caracas, Venezuela", "panam_games_1987" = "Indianapolis, United States",
  "panam_games_1991" = "Havana, Cuba", "panam_games_1995" = "Mar del Plata, Argentina",
  "panam_games_1999" = "Winnipeg, Canada", "panam_games_2003" = "Santo Domingo, Dominican Republic",
  "panam_games_2007" = "Rio de Janeiro, Brazil", "panam_games_2011" = "Guadalajara, Mexico",
  "panam_games_2015" = "Toronto, Canada", "panam_games_2019" = "Lima, Peru",
  "panam_games_2023" = "Santiago, Chile",

  # African Games
  "african_games_1965" = "Brazzaville, Congo", "african_games_1973" = "Lagos, Nigeria",
  "african_games_1978" = "Algiers, Algeria", "african_games_1987" = "Nairobi, Kenya",
  "african_games_1991" = "Cairo, Egypt", "african_games_1995" = "Harare, Zimbabwe",
  "african_games_1999" = "Johannesburg, South Africa", "african_games_2003" = "Abuja, Nigeria",
  "african_games_2007" = "Algiers, Algeria", "african_games_2011" = "Maputo, Mozambique",
  "african_games_2015" = "Brazzaville, Congo", "african_games_2019" = "Rabat, Morocco",
  "african_games_2023" = "Accra, Ghana",

  # European Games
  "european_games_2015" = "Baku, Azerbaijan", "european_games_2019" = "Minsk, Belarus",
  "european_games_2023" = "Krakow, Poland",

  # Pacific Games
  "pacific_games_1963" = "Suva, Fiji", "pacific_games_1966" = "Noumea, New Caledonia",
  "pacific_games_1969" = "Port Moresby, Papua New Guinea", "pacific_games_1971" = "Papeete, French Polynesia",
  "pacific_games_1975" = "Tumon, Guam", "pacific_games_1979" = "Suva, Fiji",
  "pacific_games_1983" = "Apia, Western Samoa", "pacific_games_1987" = "Noumea, New Caledonia",
  "pacific_games_1991" = "Port Moresby, Papua New Guinea", "pacific_games_1995" = "Papeete, French Polynesia",
  "pacific_games_1999" = "Santa Rita, Guam", "pacific_games_2003" = "Suva, Fiji",
  "pacific_games_2007" = "Apia, Samoa", "pacific_games_2011" = "Noumea, New Caledonia",
  "pacific_games_2015" = "Port Moresby, Papua New Guinea", "pacific_games_2019" = "Apia, Samoa",
  "pacific_games_2023" = "Honiara, Solomon Islands"
)

#' The nation a host city belongs to, as it appears in medal tables.
#'
#' Commonwealth hosts are resolved to the home nation that actually competes
#' (Scotland, England, Wales), not to Great Britain, because the Commonwealth
#' Games splits the UK into separate teams.
host_nation_map <- list(
  "Greece" = "Greece", "France" = "France", "United States" = "United States",
  "Great Britain" = "Great Britain", "Sweden" = "Sweden", "Belgium" = "Belgium",
  "Netherlands" = "Netherlands", "Germany" = "Germany", "Finland" = "Finland",
  "Australia" = "Australia", "Italy" = "Italy", "Japan" = "Japan",
  "Mexico" = "Mexico", "West Germany" = "West Germany", "Canada" = "Canada",
  "Soviet Union" = "Soviet Union", "South Korea" = "South Korea", "Spain" = "Spain",
  "China" = "China", "Brazil" = "Brazil", "Switzerland" = "Switzerland",
  "Norway" = "Norway", "Austria" = "Austria", "Yugoslavia" = "Yugoslavia",
  "Russia" = "Russia", "England" = "England", "Scotland" = "Scotland",
  "Wales" = "Wales", "New Zealand" = "New Zealand", "Jamaica" = "Jamaica",
  "Malaysia" = "Malaysia", "India" = "India", "Philippines" = "Philippines",
  "Indonesia" = "Indonesia", "Thailand" = "Thailand", "Iran" = "Iran",
  "Qatar" = "Qatar", "Argentina" = "Argentina", "Colombia" = "Colombia",
  "Puerto Rico" = "Puerto Rico", "Venezuela" = "Venezuela", "Cuba" = "Cuba",
  "Dominican Republic" = "Dominican Republic", "Peru" = "Peru", "Chile" = "Chile",
  "Congo" = "Congo", "Nigeria" = "Nigeria", "Algeria" = "Algeria",
  "Kenya" = "Kenya", "Egypt" = "Egypt", "Zimbabwe" = "Zimbabwe",
  "South Africa" = "South Africa", "Mozambique" = "Mozambique", "Morocco" = "Morocco",
  "Ghana" = "Ghana", "Azerbaijan" = "Azerbaijan", "Belarus" = "Belarus",
  "Poland" = "Poland", "Fiji" = "Fiji", "New Caledonia" = "New Caledonia",
  "Papua New Guinea" = "Papua New Guinea", "French Polynesia" = "French Polynesia",
  "Guam" = "Guam", "Western Samoa" = "Samoa", "Samoa" = "Samoa",
  "Solomon Islands" = "Solomon Islands"
)

#' Extract the host nation from a "City, Country" host string.
host_nation <- function(host_string) {
  vapply(host_string, function(h) {
    if (is.na(h) || !nzchar(h)) return(NA_character_)
    parts <- trimws(strsplit(h, ",")[[1]])
    cty <- parts[length(parts)]
    m <- host_nation_map[[cty]]
    if (is.null(m)) cty else m
  }, character(1), USE.NAMES = FALSE)
}

#' Editions where a boycott or exclusion materially thinned the field.
#'
#' Any dominance ranking has to carry this, because the top of the list is
#' otherwise decided by who stayed home. The United States took 36.7% of the
#' golds at Los Angeles 1984 with the Soviet bloc absent, and the Soviet Union
#' took 39.2% at Moscow 1980 with the United States absent -- consecutive
#' editions, each inflated by the other side's absence. Both sit near the top of
#' every metric here and neither is a like-for-like performance.
#'
#' `nations_absent` is approximate and is used for labelling, not arithmetic.
boycott_map <- list(
  "olympics_summer_1956" = list(reason = "Suez and Hungary withdrawals", nations_absent = 7L),
  "olympics_summer_1976" = list(reason = "African boycott over New Zealand/South Africa", nations_absent = 29L),
  "olympics_summer_1980" = list(reason = "US-led boycott", nations_absent = 66L),
  "olympics_summer_1984" = list(reason = "Soviet-led boycott", nations_absent = 14L),
  "commonwealth_1986"    = list(reason = "Boycott over apartheid sporting links", nations_absent = 32L),
  "asian_games_1962"     = list(reason = "Israel and Taiwan excluded", nations_absent = 2L),
  "asian_games_1974"     = list(reason = "Withdrawals over Israel's participation", nations_absent = 3L)
)

#' Is this edition boycott-affected?
is_boycott_edition <- function(games, year) {
  vapply(paste0(games, "_", year),
         function(k) !is.null(boycott_map[[k]]), logical(1), USE.NAMES = FALSE)
}

#' Why, as a short label. `NA` where the edition is clean.
boycott_reason <- function(games, year) {
  vapply(paste0(games, "_", year), function(k) {
    b <- boycott_map[[k]]
    if (is.null(b)) NA_character_ else b$reason
  }, character(1), USE.NAMES = FALSE)
}

# Fallback participating-nation counts. Cross-check only -- see file header.
nations_count_map <- list(
  "olympics_summer_1896" = 14L, "olympics_summer_1900" = 29L, "olympics_summer_1904" = 12L,
  "olympics_summer_1908" = 22L, "olympics_summer_1912" = 28L, "olympics_summer_1920" = 29L,
  "olympics_summer_1924" = 44L, "olympics_summer_1928" = 46L, "olympics_summer_1932" = 37L,
  "olympics_summer_1936" = 49L, "olympics_summer_1948" = 59L, "olympics_summer_1952" = 69L,
  "olympics_summer_1956" = 72L, "olympics_summer_1960" = 83L, "olympics_summer_1964" = 93L,
  "olympics_summer_1968" = 112L, "olympics_summer_1972" = 121L, "olympics_summer_1976" = 92L,
  "olympics_summer_1980" = 80L, "olympics_summer_1984" = 140L, "olympics_summer_1988" = 159L,
  "olympics_summer_1992" = 169L, "olympics_summer_1996" = 197L, "olympics_summer_2000" = 199L,
  "olympics_summer_2004" = 201L, "olympics_summer_2008" = 204L, "olympics_summer_2012" = 204L,
  "olympics_summer_2016" = 207L, "olympics_summer_2020" = 206L, "olympics_summer_2024" = 206L,

  "olympics_winter_1924" = 16L, "olympics_winter_1928" = 25L, "olympics_winter_1932" = 17L,
  "olympics_winter_1936" = 28L, "olympics_winter_1948" = 28L, "olympics_winter_1952" = 30L,
  "olympics_winter_1956" = 32L, "olympics_winter_1960" = 30L, "olympics_winter_1964" = 36L,
  "olympics_winter_1968" = 37L, "olympics_winter_1972" = 35L, "olympics_winter_1976" = 37L,
  "olympics_winter_1980" = 37L, "olympics_winter_1984" = 49L, "olympics_winter_1988" = 57L,
  "olympics_winter_1992" = 64L, "olympics_winter_1994" = 67L, "olympics_winter_1998" = 72L,
  "olympics_winter_2002" = 78L, "olympics_winter_2006" = 80L, "olympics_winter_2010" = 82L,
  "olympics_winter_2014" = 88L, "olympics_winter_2018" = 92L, "olympics_winter_2022" = 91L,
  "olympics_winter_2026" = 92L,

  "commonwealth_1930" = 11L, "commonwealth_1934" = 16L, "commonwealth_1938" = 15L,
  "commonwealth_1950" = 12L, "commonwealth_1954" = 24L, "commonwealth_1958" = 35L,
  "commonwealth_1962" = 35L, "commonwealth_1966" = 34L, "commonwealth_1970" = 42L,
  "commonwealth_1974" = 38L, "commonwealth_1978" = 46L, "commonwealth_1982" = 46L,
  "commonwealth_1986" = 26L, "commonwealth_1990" = 55L, "commonwealth_1994" = 63L,
  "commonwealth_1998" = 70L, "commonwealth_2002" = 72L, "commonwealth_2006" = 71L,
  "commonwealth_2010" = 71L, "commonwealth_2014" = 71L, "commonwealth_2018" = 71L,
  "commonwealth_2022" = 72L, "commonwealth_2026" = 74L,

  "asian_games_1951" = 11L, "asian_games_1954" = 18L, "asian_games_1958" = 20L,
  "asian_games_1962" = 16L, "asian_games_1966" = 18L, "asian_games_1970" = 18L,
  "asian_games_1974" = 19L, "asian_games_1978" = 19L, "asian_games_1982" = 33L,
  "asian_games_1986" = 27L, "asian_games_1990" = 36L, "asian_games_1994" = 42L,
  "asian_games_1998" = 41L, "asian_games_2002" = 44L, "asian_games_2006" = 45L,
  "asian_games_2010" = 45L, "asian_games_2014" = 45L, "asian_games_2018" = 45L,
  "asian_games_2022" = 45L,

  "panam_games_1951" = 21L, "panam_games_1955" = 22L, "panam_games_1959" = 25L,
  "panam_games_1963" = 22L, "panam_games_1967" = 29L, "panam_games_1971" = 29L,
  "panam_games_1975" = 33L, "panam_games_1979" = 34L, "panam_games_1983" = 36L,
  "panam_games_1987" = 38L, "panam_games_1991" = 39L, "panam_games_1995" = 42L,
  "panam_games_1999" = 42L, "panam_games_2003" = 42L, "panam_games_2007" = 42L,
  "panam_games_2011" = 42L, "panam_games_2015" = 41L, "panam_games_2019" = 41L,
  "panam_games_2023" = 41L,

  "african_games_1965" = 30L, "african_games_1973" = 35L, "african_games_1978" = 38L,
  "african_games_1987" = 42L, "african_games_1991" = 43L, "african_games_1995" = 46L,
  "african_games_1999" = 53L, "african_games_2003" = 53L, "african_games_2007" = 52L,
  "african_games_2011" = 53L, "african_games_2015" = 54L, "african_games_2019" = 54L,
  "african_games_2023" = 54L,

  "european_games_2015" = 50L, "european_games_2019" = 50L, "european_games_2023" = 48L,

  "pacific_games_1963" = 13L, "pacific_games_1966" = 14L, "pacific_games_1969" = 12L,
  "pacific_games_1971" = 14L, "pacific_games_1975" = 13L, "pacific_games_1979" = 19L,
  "pacific_games_1983" = 15L, "pacific_games_1987" = 12L, "pacific_games_1991" = 16L,
  "pacific_games_1995" = 12L, "pacific_games_1999" = 21L, "pacific_games_2003" = 22L,
  "pacific_games_2007" = 22L, "pacific_games_2011" = 22L, "pacific_games_2015" = 24L,
  "pacific_games_2019" = 24L, "pacific_games_2023" = 24L
)
