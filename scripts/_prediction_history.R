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


# One card in the cross-verse long shape (citius#32): a row per athlete per
# market (gold, medal, final) with a probability, plus the citius ids as extra
# columns. publish_prediction_history() (citius R/predictions_history.R) adds
# published_utc and model_version and validates the rest. A market column the
# card lacks is skipped; an athlete with no probability for a market gets no
# row for it rather than an NA.
card_history_rows <- function(card, meet_id) {
  card <- data.table::as.data.table(card)
  need <- c("event_id", "athlete", "athlete_id", "nation")
  miss <- setdiff(need, names(card))
  if (length(miss)) stop("card for ", meet_id, " lacks column(s): ", paste(miss, collapse = ", "))
  # One row per athlete per event, or every probability is double-counted: the
  # leftover cg2026 card stacks two forecasts of six events and its gold sums
  # to 2 (found 2026-11-07). Refuse rather than publish that.
  dup <- duplicated(card[, .(event_id, athlete_id)])
  if (any(dup)) {
    stop("card for ", meet_id, " repeats ", sum(dup), " athlete-event row(s), e.g. ",
         card$event_id[dup][1], " ", card$athlete_id[dup][1])
  }
  mk <- c(gold = "p_gold", medal = "p_medal", final = "p_final")
  mk <- mk[mk %in% names(card)]
  if (!length(mk)) stop("card for ", meet_id, " has no p_gold/p_medal/p_final column")
  out <- data.table::rbindlist(lapply(names(mk), function(m) {
    p <- as.numeric(card[[mk[[m]]]])
    keep <- !is.na(p)
    data.table::data.table(
      verse = "citius", event = paste0(meet_id, ":", card$event_id[keep]), market = m,
      # The model's own name for the athlete; the id when the card has no name.
      selection = data.table::fifelse(is.na(card$athlete[keep]) | !nzchar(card$athlete[keep]),
                                      paste0("athlete ", card$athlete_id[keep]), card$athlete[keep]),
      probability = p[keep],
      meet_id = meet_id, event_id = card$event_id[keep],
      athlete_id = as.character(card$athlete_id[keep]), nation = card$nation[keep])
  }))
  if (!nrow(out)) stop("card for ", meet_id, " produced no history rows")
  out
}
