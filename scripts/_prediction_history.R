# History of each meet's published predictions (inthegame-blog#610), so the
# site can show a forecast "as at" an earlier date. export_athletics_blog.R
# overwrites athletics/<meet>-predictions.parquet on every publish; a refreshed
# card (new cutoff or config) would otherwise erase the earlier forecast from
# R2. This keeps every DISTINCT published version in
# athletics/<meet>-predictions-history.parquet (and -nations-history), each row
# stamped `as_at` = the card's generated_at, alongside the `cutoff` and
# `config` columns the card already carries.
#
# Same pattern as torpdata's update_sim_history (scripts/parquet_helpers.R):
# - the previous history is read back from the PUBLIC bucket, because a CI
#   runner's blog/ dir is empty and the published copy is the record;
# - only a 404 means "no history yet"; any other failure stops the history
#   step, since treating it as empty would publish a one-version history over
#   the real one;
# - a history smaller than the one read is never written.
#
# "Changed" is judged by `content_md5`, a hash of the content without
# generated_at, stored on every snapshot row. The exporter's own upload memo
# cannot answer it: that memo is a local file, empty on every CI run. A rerun
# that produces the same card therefore adds nothing. Every hash here is of a
# table read back from parquet, so a card held in memory and the same card
# read from R2 hash alike (checked on budapest2026, 2026-10-07).
#
# SEEDING. The exporter re-stamps generated_at with the run time, so the card
# live on R2 when this first runs (budapest2026: as at 2026-09-30) would be lost
# by the first publish that changes it. With no history yet, the published
# card becomes the first version, under its own generated_at; if the new card
# has the same content, that is the only version.

# Canonical form, so the CI runner and a laptop (different arrow/data.table
# versions) hash the same card alike: columns sorted by name, attributes and
# classes dropped, every number as double, factors as their labels.
history_hash <- function(dt) {
  dt <- as.data.frame(dt)
  cols <- sort(setdiff(names(dt), "generated_at"), method = "radix")
  canon <- lapply(dt[cols], function(x) {
    if (is.factor(x)) x <- as.character(x)
    if (is.numeric(x) || inherits(x, c("Date", "POSIXt"))) as.double(unclass(x)) else as.vector(x)
  })
  digest::digest(canon, algo = "md5")
}

as_snapshot <- function(card, hash) {
  snap <- data.table::copy(data.table::as.data.table(card))
  snap[, as_at := format(max(generated_at), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
  snap[, content_md5 := hash]
  snap
}

# Bytes of `url`, NULL on a 404, an error on anything else.
fetch_or_404 <- function(url) {
  resp <- curl::curl_fetch_memory(url)
  code <- as.integer(resp$status_code)
  if (identical(code, 404L)) return(NULL)
  if (!identical(code, 200L)) stop(sprintf("HTTP %d reading %s", code, url))
  resp$content
}

# `new`: the card about to be published, as read back from the parquet the
# exporter wrote (has generated_at). `url`: where the published history lives;
# `seed_url`: the published card itself, used only when there is no history
# yet. Returns the combined history to publish, or NULL when this card's
# content is already the latest snapshot (nothing to publish).
update_prediction_history <- function(new, url, seed_url = NULL, fetch = fetch_or_404) {
  new <- data.table::as.data.table(new)
  if (!"generated_at" %in% names(new)) stop("card has no generated_at, so a snapshot has no as_at")
  hash <- history_hash(new)
  raw <- fetch(url)
  hist <- if (is.null(raw)) NULL else data.table::as.data.table(arrow::read_parquet(raw))
  if (!is.null(hist) && nrow(hist) > 0L) {
    if (!all(c("as_at", "content_md5") %in% names(hist))) stop("published history has no as_at/content_md5 columns: ", url)
    last <- hist[as_at == max(as_at)]
    if (identical(unique(last$content_md5), hash)) return(NULL)
  } else if (!is.null(seed_url)) {
    seed_raw <- fetch(seed_url)
    if (!is.null(seed_raw)) {
      seed <- data.table::as.data.table(arrow::read_parquet(seed_raw))
      hist <- as_snapshot(seed, history_hash(seed))
      message("seeded the history with the card already on R2 (as at ", hist$as_at[1], ")")
      # Same content as the new card: that version is the live one, under its
      # own (earlier, true) stamp, and the history starts with it alone.
      if (identical(hist$content_md5[1], hash)) return(hist)
    }
  }
  snap <- as_snapshot(new, hash)
  if (!is.null(hist) && snap$as_at[1] %in% hist$as_at) stop("as_at ", snap$as_at[1], " is already in the published history: ", url)
  out <- data.table::rbindlist(list(hist, snap), use.names = TRUE, fill = TRUE)
  # Never publish less than we read: every earlier version must survive whole.
  if (!is.null(hist)) {
    kept <- out[as_at %in% unique(hist$as_at), .N, by = as_at]
    was <- hist[, .N, by = as_at]
    if (nrow(out) < nrow(hist) || !isTRUE(all.equal(was[order(as_at)], kept[order(as_at)], check.attributes = FALSE))) {
      stop("history would shrink or lose rows of an earlier version: ", url)
    }
  }
  data.table::setorder(out, as_at)
  out
}
