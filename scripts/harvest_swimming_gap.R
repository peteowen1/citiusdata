# Close the Glasgow swimming coverage gap athlete by athlete.
#
# The crosswalk names every Glasgow swimmer with no prior history. Some are
# genuinely absent from World Aquatics -- domestic-only swimmers whose form
# lives with a national federation -- but others ARE registered and simply were
# never harvested, because our meet sweep only covers sanctioned meets they
# never swam at.
#
# This resolves each missing swimmer against the World Aquatics athlete search,
# then fetches the career of everyone it can identify. Finalists are done first:
# they decide medals, and a heat-only swimmer with no history costs almost
# nothing.
#
# The acceptance rule is deliberately strict. A wrong match fuses two careers
# into one ability estimate and nothing downstream can detect it, so a candidate
# is accepted only when the Glasgow name tokens are a SUBSET of theirs -- which
# admits a middle name we do not hold ("Luis WEEKES" -> "Luis Sebastian WEEKES")
# and rejects a different person ("Halle HARRIS" -> "Harry HARRIS") -- or when
# surname and given initial agree and the search returned exactly one candidate.
#
# Usage:  Rscript scripts/harvest_swimming_gap.R [max_athletes]
VERSE <- "C:/dev/citiusverse"
suppressMessages({
  devtools::load_all(file.path(VERSE, "citius"), quiet = TRUE)
  library(data.table)
})
D <- file.path(VERSE, "citiusdata", "data")
args <- commandArgs(trailingOnly = TRUE)
MAX <- if (length(args)) as.integer(args[1]) else Inf
say <- function(...) cat(sprintf(...), "\n", sep = "")

# NOTE ON IDS: the athlete-search endpoint returns NUMERIC ids while the meet
# sweep keys athletes by UUID, and the same swimmer can legitimately be reachable
# by both. Harvesting by numeric id therefore created a SECOND copy of five
# swimmers who were already in the corpus -- Adam Peaty among them -- with
# identical swims under a different athlete_id.
#
# That is quiet but harmful: duplicated results inflate w_total, which reduces
# shrinkage, so the swimmers we took extra trouble over end up least regressed
# to the mean. Skip anyone whose name already resolves in the corpus, and treat
# the uuid as canonical.
# ---- who is missing, worst first ------------------------------------------
xw <- setDT(arrow::read_parquet(file.path(D, "athlete_crosswalk_swimming.parquet")))
linked <- xw[, .(n = uniqueN(source)), by = person_id][n > 1L, person_id]
miss <- xw[source == "crs_glasgow2026" & !person_id %in% linked,
           .(athlete_name, country)]

g <- glasgow_swimming(D)[!is.na(event_id)]
g[, lvl := fifelse(grepl("final", round, ignore.case = TRUE) &
                     !grepl("semi", round, ignore.case = TRUE), 1L,
           fifelse(grepl("semi", round, ignore.case = TRUE), 2L, 3L))]
miss <- merge(miss, g[, .(lvl = min(lvl), sex = sex[1L]), by = athlete_name],
              by = "athlete_name", all.x = TRUE)
setorder(miss, lvl, athlete_name)
say("%d Glasgow swimmers unmatched (%d finalists, %d semi-finalists, %d heat-only)",
    nrow(miss), sum(miss$lvl == 1L, na.rm = TRUE),
    sum(miss$lvl == 2L, na.rm = TRUE), sum(miss$lvl == 3L, na.rm = TRUE))
if (is.finite(MAX)) miss <- head(miss, MAX)

# ---- resolve against the World Aquatics athlete search ---------------------
# A hyphenated surname is ONE name, so hyphens are kept while other punctuation
# is dropped. Splitting on them is what let "Adam RAMSAY-PEATY" collapse onto
# "Adam PEATY" -- a different and far more decorated swimmer.
toks <- function(x) {
  x <- toupper(gsub("[^A-Za-z' -]", " ", as.character(x)))
  p <- strsplit(trimws(gsub("\\s+", " ", x)), " ")[[1]]
  gsub("'", "", p[nzchar(p)])
}
# Both the Games feed and the World Aquatics *search* endpoint write given names
# first, so the surname is the last token in each. (The results feed does not --
# it writes "SHORT Samuel" -- which is why name order is never assumed globally.)
parts <- function(p) list(sur = p[length(p)], giv = p[-length(p)])

# The search endpoint returns 10 results by default and advertises no total, so
# an unpaged query silently truncates: four different surnames each came back
# with EXACTLY 10 candidates, which is a page size, not a coincidence. Pass
# pageSize and keep going until a page is empty -- do NOT stop on a short page,
# because the server caps a page below the requested size and the first short
# page is not the last one.
search_wa <- function(q, page_size = 100, max_pages = 40) {
  out <- list()
  for (pg in seq_len(max_pages) - 1L) {
    u <- sprintf("%s/athletes?name=%s&pageSize=%d&page=%d", aquatics_base_url(),
                 utils::URLencode(q, reserved = TRUE), page_size, pg)
    r <- tryCatch(citius_get_json(u), error = function(e) NULL)
    if (is.null(r) || !length(r$content)) break
    out[[length(out) + 1L]] <- rbindlist(lapply(r$content, function(x) data.table(
      id = as.character(x$id %||% NA), full = x$fullName %||% NA_character_,
      nat = x$nationality %||% NA_character_)), fill = TRUE)
  }
  if (!length(out)) return(NULL)
  unique(rbindlist(out, fill = TRUE))
}

# Accept only when the surnames match EXACTLY and one set of given names
# contains the other. That single rule covers both directions of the middle-name
# problem, which the feeds produce in both:
#
#   "Freya COLBERT"      -> "Freya Constance COLBERT"   we hold fewer given names
#   "Mikkel Jun Jie LEE" -> "Mikkel LEE"                we hold more
#
# and it refuses "Adam RAMSAY-PEATY" -> "Adam PEATY", because RAMSAY-PEATY and
# PEATY are different surnames however similar they look.
# The FORENAME must match exactly as well. Middle names are optional in a feed;
# a first name is not. Without this the rule accepted "Jun LEE" and "Jie LEE"
# against "Mikkel Jun Jie LEE" -- harmless when the search was truncated to ten
# candidates, but once paginated it returned several "acceptable" people and the
# ambiguity check then rejected the correct match outright.
accepts <- function(mine, cand) {
  a <- parts(mine); b <- parts(cand)
  if (!length(a$sur) || !length(b$sur) || a$sur != b$sur) return(FALSE)
  if (!length(a$giv) || !length(b$giv) || a$giv[1L] != b$giv[1L]) return(FALSE)
  all(a$giv %in% b$giv) || all(b$giv %in% a$giv)
}

resolve_one <- function(nm) {
  p <- toks(nm)
  if (length(p) < 2L) return(NULL)
  sur <- parts(p)$sur
  # Several query forms, because the search matches on different orderings and
  # a surname-only query is the one that finds short-form given names.
  qs <- unique(c(nm, paste(c(sur, parts(p)$giv), collapse = " "),
                 paste(p[1L], sur), sur))
  for (q in qs) {
    r <- search_wa(q)
    if (is.null(r) || !nrow(r)) next
    ok <- r[vapply(full, function(f) accepts(p, toks(f)), logical(1))]
    # More than one acceptable candidate identifies nobody -- two swimmers with
    # the same surname and compatible given names cannot be told apart here.
    if (nrow(ok) == 1L) return(cbind(ok, rule = "surname+given"))
    if (nrow(ok) > 1L) return(NULL)
  }
  NULL
}

# Hand-verified identities bypass the search entirely. Adam RAMSAY-PEATY is
# Adam PEATY -- he married Holly Ramsay in December 2025 and took her family
# name -- which no name rule can infer and the search cannot reveal, since
# World Aquatics still lists the old surname.
MANUAL <- file.path(D, "athlete_links_manual.csv")
manual <- if (file.exists(MANUAL)) setDT(read.csv(MANUAL, stringsAsFactors = FALSE)) else NULL
if (!is.null(manual)) {
  mm <- manual[source == "crs_glasgow2026" & nzchar(athlete_name)]
  if (nrow(mm)) {
    say("manual links: %d identity/identities taken as given", nrow(mm))
    miss <- miss[!athlete_name %in% mm$athlete_name]
  }
}

say("\nresolving %d swimmers against World Aquatics ...", nrow(miss))
t0 <- Sys.time()
res <- rbindlist(lapply(seq_len(nrow(miss)), function(i) {
  hit <- resolve_one(miss$athlete_name[i])
  if (i %% 25L == 0L) say("  %d/%d (%.0fs)", i, nrow(miss),
                          as.numeric(difftime(Sys.time(), t0, units = "secs")))
  data.table(athlete_name = miss$athlete_name[i], country = miss$country[i],
             lvl = miss$lvl[i], sex = miss$sex[i],
             wa_id = if (is.null(hit)) NA_character_ else hit$id,
             wa_name = if (is.null(hit)) NA_character_ else hit$full,
             rule = if (is.null(hit)) NA_character_ else hit$rule)
}), fill = TRUE)
say("resolved %d of %d (%.0f%%) in %.0fs", sum(!is.na(res$wa_id)), nrow(res),
    100 * mean(!is.na(res$wa_id)),
    as.numeric(difftime(Sys.time(), t0, units = "secs")))
print(res[!is.na(wa_id) & lvl == 1L, .(athlete_name, wa_name, country, rule)])
# ACCUMULATE. Each run only searches whoever is still unmatched, so overwriting
# replaces every identity established earlier with the empty result of the last
# pass -- which silently dropped a swimmer whose only link was a verified one,
# even though his career was already harvested.
RES <- file.path(D, "glasgow_swimming_gap_resolution.rds")
if (file.exists(RES)) {
  prev <- setDT(readRDS(RES))
  res <- unique(rbind(prev[!is.na(wa_id)], res, fill = TRUE), by = "athlete_name")
}
saveRDS(res, RES)
say("resolution file now holds %d identity/identities", sum(!is.na(res$wa_id)))

# ---- fetch careers ---------------------------------------------------------
todo <- res[!is.na(wa_id)]
if (!is.null(manual) && nrow(mm <- manual[source == "crs_glasgow2026" &
                                          nzchar(athlete_name)])) {
  todo <- rbind(todo, mm[, .(athlete_name, country = NA_character_,
                             lvl = NA_integer_, sex = NA_character_,
                             wa_id = as.character(link_id),
                             wa_name = NA_character_, rule = "manual")],
                fill = TRUE)
}
if (!nrow(todo)) { say("\nnothing to fetch"); quit(save = "no") }
# Write into the per-athlete cache, exactly as the main sweep does, and let
# assemble_swimming_careers.R rebuild the corpus from it. An earlier version
# merged straight into swim_athlete_history.rds and de-duplicated on a partial
# key, which silently deleted 3,305 EXISTING rows while adding 954. The cache is
# the source of truth; the assembled corpus is derived and disposable.
CACHE <- file.path(D, "swim_athlete_cache")
# Guard against re-harvesting someone already present under a different id
# format. Compare on the resolved World Aquatics name, not the Games name --
# the Games name is precisely what failed to match in the first place.
hist_f <- file.path(D, "swim_athlete_history.rds")
if (file.exists(hist_f) && nrow(todo)) {
  known <- unique(athlete_key(setDT(readRDS(hist_f))$athlete_name))
  dup <- !is.na(todo$wa_name) & athlete_key(todo$wa_name) %in% known
  if (any(dup)) {
    say("skipping %d already in the corpus under another id: %s", sum(dup),
        paste(todo$wa_name[dup], collapse = ", "))
    todo <- todo[!dup]
  }
}
if (!nrow(todo)) { say("\nnothing new to fetch"); quit(save = "no") }

say("\nfetching %d careers into the cache ...", nrow(todo))
t0 <- Sys.time()
n_swims <- vapply(seq_len(nrow(todo)), function(i) {
  r <- tryCatch(aquatics_athlete_results(todo$wa_id[i], sex = todo$sex[i]),
                error = function(e) NULL)
  saveRDS(if (is.null(r)) data.table() else r,
          file.path(CACHE, paste0(todo$wa_id[i], ".rds")))
  if (i %% 25L == 0L) say("  %d/%d (%.0fs)", i, nrow(todo),
                          as.numeric(difftime(Sys.time(), t0, units = "secs")))
  if (is.null(r)) 0L else nrow(r)
}, integer(1))
say("fetched %s swims for %d athletes in %.0fs",
    format(sum(n_swims), big.mark = ","), sum(n_swims > 0L),
    as.numeric(difftime(Sys.time(), t0, units = "secs")))

say("\nwrote %d cache file%s to swim_athlete_cache/", nrow(todo),
    if (nrow(todo) == 1L) "" else "s")
say("now rebuild, in order:")
say("  Rscript scripts/assemble_swimming_careers.R   # cache -> corpus")
say("  Rscript scripts/build_crosswalk.R             # corpus -> coverage")
