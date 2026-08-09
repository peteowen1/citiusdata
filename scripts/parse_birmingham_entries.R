# Birmingham 2026 European Athletics Championships -- official entry list.
#
# Turns the European Athletics "Final Entries - Athletes List by event and SB"
# PDF into the SAME json shape `glasgow2026_entries.json` uses, so
# `predict_glasgow_entries.R`'s consumption path works unchanged:
#
#   { events: ["Men's 100m", ...],
#     rows:   [[event_index_0based, nation, "First SURNAME", "05 Aug 2006", pb, sb], ...] }
#
# Why a PDF and not an API: there is no start-list endpoint on the World
# Athletics feed (probed, 404s), and `live.european-athletics.com` is
# Cloudflare-protected exactly like Glasgow's CRS. The Final Entries PDF is
# unauthenticated and was published 2026-07-31, ten days before the meet -- a
# strictly better position than Glasgow, which needed a hand-driven browser.
#
# The PDF is the SOURCE OF TRUTH for who is entered. It is NOT the source of
# truth for how many events exist -- that comes from the published timetable
# (44 individual events, 22 per sex). Availability is not coverage; that mistake
# was made at Birmingham 2022 and again at Glasgow 2026 swimming.

suppressMessages(library(data.table)); library(jsonlite)

PDF_URL <- paste0("https://european-athletics.directus.app/assets/",
                  "62025d5b-630d-4f54-942c-3201c9a25591/",
                  "ECH26%20-%20Final%20Entries%20Athletes%20List%20By%20Event%20%26%20SB.pdf")
# Same root convention as predict_glasgow_pretournament.R. `here::here()` is not
# used: with no .here or .Rproj marker in the tree it anchors on the working
# directory, which resolves to citiusdata/ and doubles the path when this is run
# through Rscript rather than from the verse root.
VERSE   <- "C:/dev/citiusverse"
OUT     <- file.path(VERSE, "citiusdata", "data")
PDF     <- file.path(OUT, "birmingham2026_entries.pdf")
JSON    <- file.path(OUT, "birmingham2026_entries.json")

# citiusdata#8. This used to download only `if (!file.exists(PDF))`, which meant
# that after the very first run the pipeline could never see a reissue of the
# entry list. European Athletics republishes this file at the SAME url as
# withdrawals and replacements land, and the ones that matter land closest to
# the meet -- so the window where a re-run is most worth doing was exactly the
# window where it could not work. Every later run re-derived the same card from
# the same snapshot and stamped it with a fresh `generated_at`: not a stale
# file, a stale file that looks fresh, which is the failure this whole chain
# exists to prevent.
#
# It also made the re-run ritual unable to answer its own question. On 9 August
# the card was rebuilt to pick up official start lists and the output was
# compared against live to decide whether to publish -- "nothing changed" was
# read as "the lists are not out yet", when it could not have said otherwise.
#
# Compare bytes rather than trusting headers: Last-Modified is advisory, and the
# file is ~340 KB against the four minutes this chain spends downstream, so
# fetching it every time costs nothing worth optimising.
fetch_entry_list <- function(url, path) {
  tmp <- tempfile(fileext = ".pdf")
  code <- tryCatch(
    suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE)),
    error = function(e) 1L)

  # A directus error is served as an HTML page with a 200-ish shape, so "we got
  # bytes" is not "we got the entry list". Check it is actually a PDF.
  looks_pdf <- file.exists(tmp) && file.size(tmp) > 4L &&
    identical(rawToChar(readBin(tmp, "raw", n = 4L)), "%PDF")
  ok <- identical(as.integer(code), 0L) && looks_pdf

  if (!ok) {
    if (!file.exists(path)) {
      cli::cli_abort(c(
        "Could not download the entry list, and there is no cached copy.",
        i = "Nothing downstream can be trusted without it, so this stops here."))
    }
    age <- round(as.numeric(difftime(Sys.time(), file.mtime(path), units = "days")), 1)
    cli::cli_alert_danger(c(
      "Entry-list download FAILED - continuing on the CACHED copy, {age} day{?s} old. ",
      "Treat every entry, round count and advancement probability below as that old."))
    return(invisible(FALSE))
  }

  if (file.exists(path) &&
      identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(path)))) {
    cli::cli_alert_info("Entry list unchanged upstream since the last fetch.")
    return(invisible(FALSE))
  }

  had_copy <- file.exists(path)
  file.copy(tmp, path, overwrite = TRUE)
  if (had_copy) {
    cli::cli_alert_warning(c(
      "Entry list has CHANGED upstream and has been refreshed. ",
      "Round structure and every advancement probability may move -- ",
      "this is the case a re-run exists for."))
  } else {
    cli::cli_alert_info("Downloaded the final entries PDF for the first time.")
  }
  invisible(TRUE)
}

fetch_entry_list(PDF_URL, PDF)

txt <- pdftools::pdf_text(PDF)
cli::cli_alert_info("{length(txt)} page{?s} of entry list.")

lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
lines <- gsub("\r", "", lines, fixed = TRUE)

# --- the PDF's own totals, asserted later against what we parse ---------------
hdr <- grep("Tot\\. Number of countries", lines)
tot <- if (length(hdr)) {
  n <- as.integer(regmatches(lines[hdr[1] + 1],
        gregexpr("[0-9]+", lines[hdr[1] + 1]))[[1]])
  list(countries = n[1], athletes = n[2], men = n[3], women = n[4])
} else NULL
if (!is.null(tot)) cli::cli_alert_info(
  "PDF header claims {tot$athletes} athletes ({tot$men}M/{tot$women}W), {tot$countries} countries.")

# --- walk the lines, carrying the current event ------------------------------
# An event header looks like:
#   "100 Metres Men    Num. of countries: 23    Num. of athletes: 40"
# A data row starts with a 3-letter federation code and splits on 2+ spaces.
EV_RE  <- "^\\s*(.+?)\\s{2,}Num\\. of countries:\\s*(\\d+)\\s+Num\\. of athletes:\\s*(\\d+)"
ROW_RE <- "^\\s*([A-Z]{3})\\s{2,}(.+)$"

cur <- NA_character_
out <- list()
declared <- list()

for (ln in lines) {
  m <- regmatches(ln, regexec(EV_RE, ln))[[1]]
  if (length(m) == 4) {
    cur <- trimws(m[2])
    declared[[cur]] <- as.integer(m[4])
    next
  }
  if (is.na(cur)) next
  r <- regmatches(ln, regexec(ROW_RE, ln))[[1]]
  if (length(r) != 3) next
  f <- trimws(strsplit(r[3], "\\s{2,}")[[1]])
  f <- f[nzchar(f)]
  # surname, first name, dob are required; pb and sb are optional and either
  # may be absent (a relay-only entrant often has neither).
  if (length(f) < 3) next
  dob_i <- grep("^\\d{2}/\\d{2}/\\d{4}$", f)
  if (!length(dob_i)) next
  dob_i <- dob_i[1]
  if (dob_i < 3) next
  surname <- f[1]
  first   <- paste(f[2:(dob_i - 1)], collapse = " ")
  marks   <- f[-(1:dob_i)]
  out[[length(out) + 1]] <- data.table(
    event = cur, nation = r[2], surname = surname, first = first,
    dob_raw = f[dob_i],
    pb = if (length(marks) >= 1) marks[1] else NA_character_,
    sb = if (length(marks) >= 2) marks[2] else NA_character_)
}

e <- rbindlist(out, fill = TRUE)
cli::cli_alert_info("Parsed {nrow(e)} entry row{?s} across {uniqueN(e$event)} event{?s}.")

# --- assert the parse against the PDF's own declared counts ------------------
# A silent under-parse is the failure mode here: a regex that misses a name
# shape drops athletes without erroring, and a thinner field quietly changes
# every probability on the card.
dec <- data.table(event = names(declared), declared = unlist(declared))
got <- e[, .(parsed = .N), by = event]
chk <- merge(dec, got, by = "event", all = TRUE)
chk[is.na(parsed), parsed := 0L]
bad <- chk[declared != parsed]
if (nrow(bad)) {
  print(bad)
  cli::cli_abort("Parsed count disagrees with the PDF's declared count in {nrow(bad)} event{?s}.")
}
cli::cli_alert_success("Per-event counts match the PDF's own declared totals in all {nrow(chk)} events.")

# --- shape it the way the Glasgow consumer expects ---------------------------
# "100 Metres Men" -> sex M, discipline "100 Metres", label "Men's 100 Metres".
e[, sex := fifelse(grepl("\\bMen$", event), "M",
            fifelse(grepl("\\bWomen$", event), "W", NA_character_))]
e[, discipline := trimws(sub("\\s+(Men|Women)$", "", event))]

# Relays are team events citius does not model, and they carry no usable
# individual mark. Para classifications are not modelled either.
is_relay <- grepl("Relay", e$discipline, ignore.case = TRUE)
is_para  <- grepl("^(T|F)[0-9]{2}|Para", e$discipline)
cli::cli_alert_info("Dropping {sum(is_relay)} relay row{?s} and {sum(is_para)} para row{?s}.")
e <- e[!is_relay & !is_para]
e <- e[!is.na(sex)]

# DoB dd/mm/yyyy -> "05 Aug 2006", matching the Glasgow file exactly.
e[, dob := format(as.Date(dob_raw, "%d/%m/%Y"), "%d %b %Y")]
# Name as "First SURNAME", matching the Glasgow file exactly.
e[, athlete := paste(first, surname)]
e[, label := paste0(fifelse(sex == "M", "Men's ", "Women's "), discipline)]

setorder(e, label, nation, surname)
evs <- unique(e$label)
e[, ev_idx := match(label, evs) - 1L]

rows <- lapply(seq_len(nrow(e)), function(i)
  list(e$ev_idx[i], e$nation[i], e$athlete[i], e$dob[i],
       if (is.na(e$pb[i])) NULL else e$pb[i],
       if (is.na(e$sb[i])) NULL else e$sb[i]))

write_json(list(events = evs, rows = rows), JSON, auto_unbox = TRUE, null = "null")

cli::cli_alert_success("Wrote {basename(JSON)}: {length(evs)} individual events, {nrow(e)} entries.")
cli::cli_alert_info("Individual events per sex: M {uniqueN(e[sex=='M']$label)}, W {uniqueN(e[sex=='W']$label)}.")

# --- COVERAGE, reported loudly ----------------------------------------------
# `predict_glasgow_entries.R` drops rows where match_event() returns NA. That is
# correct behaviour and a silent one: an event the registry does not know simply
# vanishes, and the card looks complete while covering less than the programme.
# That is the Birmingham 2022 and Glasgow 2026 swimming failure, twice recorded.
# So resolve here and SAY what will not be modelled, with the row count it costs.
suppressMessages(devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE))
cov <- unique(e[, .(label, discipline, sex)])
cov[, event_id := citius::match_event(discipline, sex)]
miss <- cov[is.na(event_id)]
n_ok  <- nrow(cov) - nrow(miss)
if (nrow(miss)) {
  cli::cli_alert_danger(
    "{nrow(miss)} of {nrow(cov)} programme events have NO registry event_id and will be dropped:")
  print(miss[, .(label, discipline, sex)])
  cli::cli_alert_info(
    "Entries lost: {e[label %in% miss$label, .N]} of {nrow(e)}.")
} else {
  cli::cli_alert_success("All {nrow(cov)} events resolve to a registry event_id.")
}
cli::cli_alert_info("MODELLABLE: {n_ok} of {nrow(cov)} individual events.")

print(merge(e[, .(entries = .N, nations = uniqueN(nation)), by = label],
            cov[, .(label, event_id)], by = "label")[order(label)])
