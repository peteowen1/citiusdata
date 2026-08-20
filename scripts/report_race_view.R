# A race as the model sees it: the result, and what each mark was WORTH.
#
# The published result is what happened. The adjusted mark is what it says about
# ability once the wind and the venue are taken out - which is the thing the
# rating actually consumes since 2026-08-19. Showing both, side by side, makes
# the correction inspectable instead of something that happens off-page.
#
# Races are chosen to make the mechanism visible rather than cherry-picked to
# flatter it: the strongest tailwind, the strongest headwind, the highest
# altitude, and a championship final where the corrections are near zero. That
# last one matters - a reader should see the adjustment do NOTHING when there is
# nothing to adjust, or the column looks like a fudge factor.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D    <- here::here("citiusdata", "data")
MINF <- as.integer(Sys.getenv("RACE_MIN_FINISHERS", "6"))
FROM <- as.Date(Sys.getenv("RACE_FROM", "2023-01-01"))

am <- setDT(read_parquet(file.path(D, "adjusted_marks.parquet")))
am[, athlete_id := as.character(athlete_id)]
d  <- setDT(read_parquet(file.path(D, "form_display_final.parquet")))
# NAMES COME FROM THE NAME TABLE, NOT THE RANKINGS. This used to read names out
# of form_display_final, which holds only currently-ranked athletes - so anyone
# outside the rankings rendered as "unnamed" on a results page that has nothing
# to do with rankings. The Paris 2024 women's marathon showed its winner as
# "unnamed"; we knew perfectly well she was Sifan Hassan. athlete_name_lookup
# carries 87,119 athletes against the display table's 71,485.
nm <- unique(d[, .(athlete_id = as.character(athlete_id), athlete_name)])
nm <- nm[!is.na(athlete_name)]
.lk <- file.path(D, "athlete_name_lookup.rds")
if (file.exists(.lk)) {
  L <- as.data.table(readRDS(.lk))
  L[, athlete_id := as.character(athlete_id)]
  L <- unique(L[!is.na(athlete_name) & nzchar(athlete_name), .(athlete_id, athlete_name)],
              by = "athlete_id")
  nm <- unique(rbind(nm, L[!athlete_id %chin% nm$athlete_id]), by = "athlete_id")
}
# The union must beat the source it WIDENS, not the whole display. The first
# version compared against uniqueN(d$athlete_id) and passed only because the
# display then held 71,485 athletes; after the recency windows went to 800 days
# it holds far more, many of whom have no name in any source, and the guard fired
# on a correct state. Comparing a union against a population it never claimed to
# cover is the wrong test - this compares it against the display's own named
# athletes, which is the thing the lookup is there to extend.
.n_from_display <- uniqueN(d[!is.na(athlete_name), athlete_id])
stopifnot("the name table is smaller than the rankings it was meant to widen" =
            nrow(nm) >= .n_from_display)
cat(sprintf("names: %s from rankings, %s after adding the lookup (+%s)
",
            format(.n_from_display, big.mark = ","), format(nrow(nm), big.mark = ","),
            format(nrow(nm) - .n_from_display, big.mark = ",")))
cat(sprintf("names available for %s athletes\n", format(nrow(nm), big.mark = ",")))
am <- merge(am, nm, by = "athlete_id", all.x = TRUE)
am <- am[date >= FROM & is.finite(place) & place > 0]
cat(sprintf("adjusted performances since %s: %s\n", FROM, format(nrow(am), big.mark = ",")))

# a race is usable if it has enough finishers and we can name most of them
rk <- am[, .(n = .N, named = sum(!is.na(athlete_name)), date = date[1],
             event_id = event_id[1], discipline = discipline[1], sex = sex[1],
             family = family[1], comp_name = comp_name[1], venue_city = venue_city[1],
             wind = wind[1], unit = unit[1],
             wind_adj = stats::median(wind_adj), venue_adj = stats::median(venue_adj)),
         by = race_key]
rk <- rk[n >= MINF & named >= pmin(n, MINF)]
rk[, total_adj := abs(wind_adj) + abs(venue_adj)]
cat(sprintf("usable races: %s\n", format(nrow(rk), big.mark = ",")))
stopifnot("no usable races" = nrow(rk) > 50)

# `take`, NOT `n`. `n` is a COLUMN of rk (the finisher count), and data.table
# evaluates `i` in the table own frame - so min(n, .N) silently became the
# minimum finisher count, six, and every label grabbed six races. The view then
# printed a marathon beside a 19.43 and called it one race. Documented in this
# repo own rules as parameter/column shadowing, and still easy to walk into.
pick <- function(lab, dt, take = 1L) {
  if (!nrow(dt)) { cat(sprintf("  NO CANDIDATE for '%s'\n", lab)); return(NULL) }
  dt[seq_len(min(take, nrow(dt)))][, .(race_key, label = lab)]
}
cat("\nselecting illustrative races:\n")
sel <- rbindlist(list(
  pick("Strongest tailwind",
       rk[family %chin% c("sprint","hurdles") & is.finite(wind) & wind > 0][order(-wind)]),
  pick("Strongest headwind",
       rk[family %chin% c("sprint","hurdles") & is.finite(wind) & wind < 0][order(wind)]),
  pick("Highest-altitude distance race",
       rk[family %chin% c("distance","middle") & venue_adj < 0][order(venue_adj)]),
  pick("A race needing almost no correction",
       rk[total_adj < 0.0005 & n >= 8][order(-n)])
), fill = TRUE)
stopifnot("no races selected" = !is.null(sel) && nrow(sel) > 0)
cat(sprintf("selected %d race(s): %s\n", nrow(sel), paste(sel$label, collapse = " | ")))
# One label must mean ONE race. The first version printed a "race" containing a
# marathon, a 19.43 and a 59.45 - unmistakably several events - and nothing
# stopped it, because the selection silently returned more than one key per
# label. race_key itself is fine: 719,848 keys, each mapping to exactly one event.
stopifnot("a label selected more than one race" = !anyDuplicated(sel$label))

out <- merge(am[race_key %chin% sel$race_key], sel, by = "race_key")
setorder(out, label, place)
out <- out[, .(label, race_key, date, discipline, sex, comp_name, venue_city,
               wind, place, athlete = athlete_name, mark, adj_mark, adj_delta,
               wind_adj, venue_adj, unit)]
stopifnot("selection produced no rows" = nrow(out) > 0)
chk <- out[, .(events = uniqueN(discipline), keys = uniqueN(race_key)), by = label]
if (any(chk$events > 1 | chk$keys > 1)) {
  print(chk)
  stop("a label spans more than one race or event - the view would show a ",
       "marathon next to a 100m and call it one race")
}

fmt <- function(m, unit) {
  ifelse(is.na(m), NA_character_,
  ifelse(!(unit %chin% c("s", "seconds")), sprintf("%.2f", m),
  ifelse(m < 60, sprintf("%.2f", m),
  ifelse(m < 3600, sprintf("%d:%05.2f", floor(m/60), m %% 60),
         sprintf("%d:%02d:%02.0f", floor(m/3600), floor((m %% 3600)/60), m %% 60)))))
}
out[, `:=`(mark_s = fmt(mark, unit), adj_s = fmt(adj_mark, unit))]

for (L in unique(out$label)) {
  x <- out[label == L]
  cat(sprintf("\n== %s ==\n%s, %s, %s %s | wind %s\n", L,
              x$comp_name[1] %||% "(no competition name)", x$venue_city[1] %||% "?",
              x$discipline[1], x$sex[1],
              ifelse(is.finite(x$wind[1]), sprintf("%+.1f", x$wind[1]), "n/a")))
  print(x[, .(place, athlete = substr(athlete, 1, 22), result = mark_s,
              adjusted = adj_s, change = round(adj_delta, 3))])
}

f <- file.path(D, "race_view_report.json")
writeLines(jsonlite::toJSON(out, dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s (%d rows over %d races)\n", basename(f), nrow(out),
            uniqueN(out$race_key)))
