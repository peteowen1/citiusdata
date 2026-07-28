# Harvest SwimCloud for swimmers neither World Aquatics nor Swim England has.
#
# WHERE THIS FITS: three sources, each covering what the others structurally
# cannot.
#   World Aquatics  international, sanctioned meets      (stable PersonId)
#   Swim England    British domestic, all home nations   (stable tiref)
#   SwimCloud       everything else -- e.g. an Australian domestic swimmer,
#                   who neither of the other two will ever hold
#
# After the Swim England sweep exactly ONE Glasgow finalist had no prior form
# anywhere: Inez Miller (AUS). That is this script's reason to exist -- a small
# residual, not a bulk source.
#
# WHY IT NEEDS A BROWSER: SwimCloud returns 403 to every non-browser client,
# including headless Chrome, which advertises itself in the user agent and is
# served a challenge page. A real Chrome is served normally, so the harvester
# launches one and drives it. The browser is genuine; only the driving is
# automated.
#
# Usage:  Rscript scripts/harvest_swimcloud.R "Inez Miller" "Someone Else"
VERSE <- "C:/dev/citiusverse"
suppressMessages({library(citius); library(data.table); library(rvest); library(xml2)})
OUT <- file.path(VERSE, "citiusdata", "data")
CACHE <- file.path(OUT, "swimcloud_cache")
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
say <- function(...) cat(sprintf(...), "\n", sep = "")
names_in <- commandArgs(trailingOnly = TRUE)
if (!length(names_in)) { say("give at least one swimmer name"); quit(save = "no") }

sess <- swimcloud_session()
on.exit(sess$close(), add = TRUE)
say("browser session open")

# ---- name -> swimmer id ----------------------------------------------------
find_swimmer <- function(nm) {
  h <- tryCatch(swimcloud_fetch(sess, sprintf("/swimmers/?name=%s",
                utils::URLencode(nm, reserved = TRUE)), concurrency = 1L),
                error = function(e) NULL)
  if (is.null(h)) return(NULL)
  doc <- read_html(h[1])
  a <- html_elements(doc, "a[href*='/swimmer/']")
  if (!length(a)) return(NULL)
  data.table(id = sub(".*/swimmer/([0-9]+)/.*", "\\1", html_attr(a, "href")),
             name = trimws(html_text(a)))[nzchar(name)][1]
}

# A bare number is taken as a SwimCloud swimmer id. The site's name search is
# an XHR whose parameters are not documented and which returns nothing to a
# plain GET, so ids are the reliable input; a name is attempted best-effort.
# Find an id from the profile URL: swimcloud.com/swimmer/<id>/
found <- rbindlist(lapply(names_in, function(nm) {
  if (grepl("^[0-9]+$", nm)) return(data.table(query = nm, id = nm, found = paste("id", nm)))
  s <- find_swimmer(nm)
  data.table(query = nm,
             id = if (is.null(s)) NA_character_ else s$id,
             found = if (is.null(s)) NA_character_ else s$name)
}), fill = TRUE)
print(found)
todo <- found[!is.na(id)]
if (!nrow(todo)) { say("\nnothing resolved"); quit(save = "no") }

# ---- their meets, then every race in them ----------------------------------
t0 <- Sys.time()
all <- rbindlist(lapply(seq_len(nrow(todo)), function(i) {
  h <- swimcloud_fetch(sess, sprintf("/swimmer/%s/meets/", todo$id[i]), concurrency = 1L)
  hrefs <- na.omit(html_attr(html_elements(read_html(h[1]), "a"), "href"))
  # Match the id rather than sub()-ing a non-match back to itself, which silently
  # produces a malformed path and a 404 later.
  m <- regmatches(hrefs, regexpr("/results/[0-9]+/", hrefs))
  meets <- unique(sub("/results/([0-9]+)/", "\\1", m))
  say("  %s: %d meet%s", todo$found[i], length(meets), if (length(meets) == 1) "" else "s")
  rbindlist(lapply(meets, function(mid) {
    mh <- tryCatch(swimcloud_fetch(sess, sprintf("/results/%s/", mid), concurrency = 1L),
                   error = function(e) NULL)
    if (is.null(mh)) return(NULL)
    ev <- na.omit(html_attr(html_elements(read_html(mh[1]), "a"), "href"))
    ev <- regmatches(ev, regexpr("/results/[0-9]+/event/[0-9]+/", ev))
    evs <- unique(sub(".*/event/([0-9]+)/", "\\1", ev))
    if (!length(evs)) return(NULL)
    # 6 concurrent is the measured ceiling: 10 returns HTTP 429 and silently
    # drops ~29% of pages while looking like the fastest setting.
    #
    # Chunked with a pause between chunks. A meet can carry 65 event pages and
    # nine meets in one burst exhausted the retry budget -- and because the
    # failure was caught and turned into NULL, the whole harvest reported "no
    # results parsed" with no indication that anything had gone wrong.
    chunks <- split(evs, ceiling(seq_along(evs) / 24L))
    got <- lapply(seq_along(chunks), function(ci) {
      ev_ids <- chunks[[ci]]
      pages <- tryCatch(
        swimcloud_fetch(sess, sprintf("/results/%s/event/%s/", mid, ev_ids),
                        concurrency = 4L),
        error = function(e) { say("    meet %s chunk %d FAILED: %s", mid, ci,
                                  conditionMessage(e)); NULL })
      if (ci < length(chunks)) Sys.sleep(1)
      if (is.null(pages)) return(NULL)
      rbindlist(lapply(seq_along(pages), function(k)
        swimcloud_parse_event(pages[k], mid, ev_ids[k])), fill = TRUE)
    })
    rbindlist(got, fill = TRUE)
  }), fill = TRUE)
}), fill = TRUE)

if (!nrow(all)) { say("\nno results parsed"); quit(save = "no") }

all[, event_id := match_event(discipline, sex)]
all[, mark := parse_mark(mark_string)]
say("\n%s rows | %s swimmers | %s races | event matched %.0f%% | marks %.0f%%",
    format(nrow(all), big.mark = ","), format(uniqueN(all$athlete_id), big.mark = ","),
    format(uniqueN(all$race_key), big.mark = ","),
    100 * mean(!is.na(all$event_id)), 100 * mean(!is.na(all$mark)))
un <- all[is.na(event_id), .N, by = discipline][order(-N)]
if (nrow(un)) { say("\nunmatched disciplines:"); print(head(un, 8)) }

saveRDS(all, file.path(CACHE, sprintf("swimcloud_%s.rds",
                                      paste(todo$id, collapse = "_"))))
say("\ndone in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
