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
       "WOMEN'S HEPTATHLON FINAL"))

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
  r(3,"Final","14","14","CMR","Adele MAFOGANG TENKEU","","5262"))

f <- file.path(D, "glasgow2026_gapfill.json")
write_json(list(p = pages, r = rows), f, auto_unbox = TRUE)
cat("wrote", f, "\n")

g <- as.data.table(parse_crs_export(f))
cat(sprintf("\nparsed %d rows | events: %s\n", nrow(g),
            paste(sort(unique(g$event_id[!is.na(g$event_id)])), collapse = ", ")))
if (any(is.na(g$event_id))) {
  cat("UNMATCHED titles:\n"); print(unique(g[is.na(event_id)]$discipline))
}
cat("\npodiums as parsed:\n")
print(g[!is.na(place) & place <= 3, .(event_id, place, athlete_name, country, mark_string)][
  order(event_id, place)])
