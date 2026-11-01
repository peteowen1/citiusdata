# Resolve synthetic "CRS:<KEY>" prediction ids onto the ids the results use
# (citiusdata#23).
#
# predict_glasgow_swimming.R keys a swimmer it cannot link to World Aquatics
# history as "CRS:" + athlete_key(name). The blog builds its athlete roster by
# merging predictions and results on EXACT athlete_id, so that swimmer showed up
# twice with identical labels -- one entry holding the predictions, one the
# results.
#
# This is deliberately NOT a loose name match (the issue explains why that is
# unsafe: names and nations are themselves reconstructed upstream). A synthetic
# id is rewritten only when ALL of these hold:
#   1. the id is "CRS:"-prefixed -- native ids are never touched;
#   2. its name key maps to exactly ONE distinct athlete_id among the results
#      rows for `sport` -- two people sharing a key stay unresolved, because
#      fusing two athletes is worse than showing two thin profiles;
#   3. that athlete actually has a results row in the SAME event as the
#      prediction row -- a prediction field is a final's field, so this ties the
#      match to the race and not just to the spelling of a name.
# The name key is read off the id itself, not the `athlete` column, because the
# name is null on some prediction rows and the id suffix is not.
#
# Returns `pred` with `athlete_id` rewritten where resolved, plus
#   athlete_id_original  the id as the model script wrote it
#   id_unresolved        TRUE for a "CRS:" id that could not be resolved, so the
#                        blog can surface or suppress it deliberately
#
# `results` needs sport, event_id, athlete_id, athlete_name. `key_fn` is
# athlete_key() in the export; injectable so this can be tested without citius.
resolve_synthetic_ids <- function(pred, results, key_fn, sport_name = "Swimming",
                                  prefix = "CRS:") {
  pred <- data.table::copy(data.table::as.data.table(pred))
  pred[, athlete_id := as.character(athlete_id)]
  pred[, athlete_id_original := athlete_id]
  pred[, id_unresolved := FALSE]
  syn <- !is.na(pred$athlete_id) & startsWith(pred$athlete_id, prefix)
  if (!any(syn)) return(pred[])

  r <- data.table::as.data.table(results)[
    sport == ..sport_name & !is.na(athlete_id) & !is.na(athlete_name) &
      !startsWith(as.character(athlete_id), prefix),
    .(event_id, athlete_id = as.character(athlete_id), key = key_fn(athlete_name))]
  r <- unique(r[!is.na(key) & nzchar(key)])
  uniq <- r[, .(athlete_id = athlete_id[1L], n_ids = data.table::uniqueN(athlete_id)),
            by = key][n_ids == 1L, .(key, athlete_id)]
  in_event <- r[, .(event_id, key, athlete_id)]

  s <- pred[syn, .(.row = which(syn), key = substring(athlete_id, nchar(prefix) + 1L),
                   event_id)]
  s <- merge(s, uniq, by = "key", all.x = TRUE, sort = FALSE)
  s[, ok := FALSE]
  s[in_event, on = .(event_id, key, athlete_id), ok := TRUE]
  res <- s[ok == TRUE]
  if (nrow(res)) data.table::set(pred, res$.row, "athlete_id", res$athlete_id)
  data.table::set(pred, s[ok == FALSE]$.row, "id_unresolved", TRUE)
  cli::cli_alert_info(
    "Synthetic {prefix} prediction ids: {nrow(res)} of {nrow(s)} row{?s} resolved to a results id, {sum(!s$ok)} left flagged id_unresolved.")
  pred[]
}
