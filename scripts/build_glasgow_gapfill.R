# The last three Glasgow events, captured from the results system.
#
# These were the three the harvest could not reach, and each was missing for a
# different reason:
#
#   Women's heptathlon -- the overall standings are not under `athletic-result`
#     at all. Combined events publish per-component result pages and put the
#     points table behind a COMBINED tab on route family `athletic-summaries`,
#     which no sweep for "athletic-result" hrefs will ever find.
#   Women's 10,000m race walk -- the final route existed and simply failed to
#     capture while the app's renderer was degrading late in the session.
#   Women's 1500m freestyle -- swum in two sections, and the plain /FNL-/000100
#     route is empty; the results live on the /0001FA-- and /0001SL-- variants.
#
# Every row below was read off the rendered pages and cross-checked against
# Wikipedia's medal tables before being written down: O'Connor 6569 / O'Dowda
# 6278 / West 6104; Montag 42:13.40 / McMillen 42:39.87 / Henderson 43:27.46;
# Pallister 15:41.03 / Fairweather 15:58.97 / Walker 16:01.37. All three agree.
suppressMessages(devtools::load_all("C:/dev/citiusverse/citius", quiet = TRUE))
suppressMessages({library(data.table); library(jsonlite)})
D <- "C:/dev/citiusverse/citiusdata/data"

pages <- list(
  list("athletic-result/ATH/M/W/10000MW-----------/FNL-/000100--",
       "01 Aug 2026, 10:30  |  Finished  \u2022  Scotstoun Stadium",
       "WOMEN'S 10,000M RACE WALK FINAL"),
  list("athletic-result/SWM/ST/W/1500MFR-----------/FNL-/0001FA--",
       "26 Jul 2026, 21:04  |  Finished  \u2022  Tollcross Swimming Centre",
       "WOMEN'S 1500M FREESTYLE FASTEST HEAT"),
  list("athletic-result/SWM/ST/W/1500MFR-----------/FNL-/0001SL--",
       "26 Jul 2026, 11:17  |  Finished  \u2022  Tollcross Swimming Centre",
       "WOMEN'S 1500M FREESTYLE SLOWEST HEAT 1"),
  list("athletic-result/ATH/M/W/HEPTATH-----------/FNL-/000100--",
       "29 Jul 2026, 20:55  |  Finished  \u2022  Scotstoun Stadium",
       "WOMEN'S HEPTATHLON FINAL"),
  # The two 400m finals are re-captured here because the sweep read their
  # REACTION TIME as the mark. Sprints and hurdles split the results header over
  # three lines -- "Rank...Name", then a "Reaction Time" tooltip, then the real
  # column names "R.T. / Time / Record" -- so a parser reading only the "Rank..."
  # line finds no value column and falls back to the first non-empty value,
  # which is the reaction time. 216 rows across ten events were affected; the
  # feed's correct value wins for eight of them, and only these two had no feed
  # row to override it.
  list("athletic-result/ATH/S/M/400M--------------/FNL-/000100--",
       "01 Aug 2026, 12:00  |  Finished  \u2022  Scotstoun Stadium",
       "MEN'S 400M FINAL"),
  list("athletic-result/ATH/S/W/400M--------------/FNL-/000100--",
       "01 Aug 2026, 12:20  |  Finished  \u2022  Scotstoun Stadium",
       "WOMEN'S 400M FINAL"))

r <- function(p, ht, rk, ln, nat, nm, rt, tm) list(p, ht, rk, ln, nat, nm, rt, tm)
rows <- list(
  # --- women's 10,000m race walk ---
  r(0,"Final","1","2","AUS","Jemima MONTAG","","42:13.40"),
  r(0,"Final","2","5","AUS","Elizabeth McMILLEN","","42:39.87"),
  r(0,"Final","3","4","AUS","Rebecca HENDERSON","","43:27.46"),
  r(0,"Final","4","1","IND","PRIYANKA","","45:53.93"),
  r(0,"Final","5","3","KEN","Silvia Jerono KEMBOI","","51:30.80"),
  # --- women's 1500m freestyle, fastest section ---
  r(1,"Final - Fastest Heat","1","4","AUS","Lani PALLISTER","0.70","15:41.03"),
  r(1,"Final - Fastest Heat","2","5","NZL","Erika FAIRWEATHER","0.72","15:58.97"),
  r(1,"Final - Fastest Heat","3","1","AUS","Molly WALKER","0.74","16:01.37"),
  r(1,"Final - Fastest Heat","4","3","SGP","Ching Hwee GAN","0.61","16:06.00"),
  r(1,"Final - Fastest Heat","5","7","NZL","Caitlin DEANS","0.80","16:08.62"),
  r(1,"Final - Fastest Heat","6","6","AUS","Tiana KRITZINGER","0.72","16:11.18"),
  r(1,"Final - Fastest Heat","7","8","ENG","Amelie BLOCKSIDGE","0.83","16:27.55"),
  r(1,"Final - Fastest Heat","8","2","NZL","Eve THOMAS","0.70","16:29.63"),
  # --- women's 1500m freestyle, slowest section ---
  r(2,"Final - Slowest Heat 1","1","4","CAY","Kyra RABESS","0.66","17:12.27"),
  r(2,"Final - Slowest Heat 1","2","5","SGP","En Xi Sarah SIM","0.64","17:22.06"),
  r(2,"Final - Slowest Heat 1","3","2","JEY","Clara GINNIS","0.77","17:36.94"),
  r(2,"Final - Slowest Heat 1","4","3","JEY","Hannah STERRY","0.64","17:41.15"),
  r(2,"Final - Slowest Heat 1","5","6","CAY","Harper BARROWMAN","0.85","17:55.90"),
  # --- women's heptathlon, overall points ---
  r(3,"Final","1","1","NIR","Katherine O'CONNOR","","6569"),
  r(3,"Final","2","2","ENG","Jade O'DOWDA","","6278"),
  r(3,"Final","3","3","AUS","Tori WEST","","6104"),
  r(3,"Final","4","4","ENG","Ellen BARBER","","6050"),
  r(3,"Final","5","5","NZL","Maddie WILSON","","6005"),
  r(3,"Final","6","6","ENG","Niamh EMERSON","","6002"),
  r(3,"Final","7","7","AUS","Mia SCERRI","","5996"),
  r(3,"Final","8","8","NIR","Anna McCAULEY","","5994"),
  r(3,"Final","9","9","NZL","Briana STEPHENSON","","5843"),
  r(3,"Final","10","10","CAN","Sienna MACDONALD","","5827"),
  r(3,"Final","11","11","GIB","Ella RUSH","","5551"),
  r(3,"Final","12","12","JEY","Lucy WOODWARD","","5530"),
  r(3,"Final","13","13","CAN","Hannah BLAIR","","5340"),
  r(3,"Final","14","14","CMR","Adele MAFOGANG TENKEU","","5262"),
  # --- men's 400m final (reaction time kept in its own field) ---
  r(4,"Final","1","5","NGR","Samuel OGAZI","0.196","44.25"),
  r(4,"Final","2","4","TTO","Jereem RICHARDS","0.153","44.82"),
  r(4,"Final","3","2","RSA","Zakithi NENE","0.147","45.21"),
  r(4,"Final","4","6","BOT","Lee Bhekempilo EPPIE","0.149","45.31"),
  r(4,"Final","5","7","AUS","Thomas REYNOLDS","0.172","45.57"),
  r(4,"Final","6","8","ZAM","Muzala SAMUKONGA","0.189","45.61"),
  r(4,"Final","7","1","JAM","Zandrion BARNES","0.171","45.68"),
  r(4,"Final","8","3","NGR","Edidiong Okon UDO","0.199","45.81"),
  # --- women's 400m final ---
  r(5,"Final","1","4","JAM","Dejanea OAKLEY","0.138","50.21"),
  r(5,"Final","2","5","BAR","Sada WILLIAMS","0.163","50.99"),
  r(5,"Final","3","6","NGR","Ella ONOJUVWEVWO","0.246","51.00"),
  r(5,"Final","4","3","WAL","Charlotte HENRICH","0.148","51.16"),
  r(5,"Final","5","1","SCO","Nicole YEARGIN","0.156","51.27"),
  r(5,"Final","6","7","ENG","Yemi Mary JOHN","0.170","51.60"),
  r(5,"Final","7","2","CAN","Lauren GALE","0.187","52.01"),
  r(5,"Final","8","8","BOT","Obakeng KAMBERUKA","0.184","52.53"))

f <- file.path(D, "glasgow2026_gapfill.json")
write_json(list(p = pages, r = rows), f, auto_unbox = TRUE)
cat("wrote", f, "\n")

g <- as.data.table(parse_crs_export(f))
cat(sprintf("\nparsed %d rows | events: %s\n", nrow(g),
            paste(sort(unique(g$event_id[!is.na(g$event_id)])), collapse = ", ")))
if (any(is.na(g$event_id))) {
  cat("UNMATCHED titles:\n"); print(unique(g[is.na(event_id)]$discipline))
}
# These rows are typed by hand off rendered pages, which is exactly the input a
# round-trip check exists for -- a mistyped page index, a dropped row or a title
# that matches no event are all easy and all silent. Printing a diagnostic and
# exiting 0 is not a check; anyone regenerating this file would have to notice
# the line. Hard-fail instead.
stopifnot(
  "a hand-entered row was lost in the round trip" = nrow(g) == length(rows),
  "a page title matched no event in the registry" = !any(is.na(g$event_id)),
  # Parenthesised on purpose: `%in%` binds TIGHTER than `-`, so the unbracketed
  # form parses as `(idx %in% seq_along(pages)) - 1L` and compares nothing.
  "a hand-entered page index is out of range" =
    all(vapply(rows, function(x) x[[1]], numeric(1)) %in% (seq_along(pages) - 1L)),
  "a hand-entered mark failed to parse" = all(is.finite(g$mark))
)
cat("\npodiums as parsed:\n")
print(g[!is.na(place) & place <= 3, .(event_id, place, athlete_name, country, mark_string)][
  order(event_id, place)])
