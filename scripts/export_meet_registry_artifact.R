# Export competition_catalogue.parquet to the JSON the meet-registry
# artifact embeds. Re-run this and republish the artifact (same file path,
# via the Artifact tool) whenever competition_catalogue.parquet changes in a
# way worth reflecting -- there is no automatic sync.
suppressMessages({library(data.table); library(arrow); library(jsonlite)})
OUT <- here::here("citiusdata", "data")

ct <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat(sprintf("total rows: %s\n", format(nrow(ct), big.mark = ",")))

# `first_date` so a meet can be looked up rather than just filtered -- year
# alone is not enough to find a specific edition. `strength_pb` alongside
# `strength` so the career-best and EW bases can be compared on the page: they
# disagree by ~5 on the median meet and by 20+ on the fast road courses, which
# is the whole point of the 2026-09-03 change.
.cols <- c("competition_id", "comp_name", "class", "strength", "strength_pb",
           "meet_tier", "first_date", "last_date", "year", "country",
           "athletes", "events", "finals", "is_major", "is_global")
.missing <- setdiff(.cols, names(ct))
if (length(.missing))
  stop("catalogue is missing: ", paste(.missing, collapse = ", "),
       " -- run apply_strength_ew.R, which adds strength_pb")
keep <- ct[, .SD, .SDcols = .cols]

# Never-scored, never-classified meets (strength NA AND class unclassified)
# add no browsing value and are the bulk of the row count -- trimming them
# keeps the embedded JSON well under the artifact's 16MB budget.
trimmed <- keep[!(is.na(strength) & class == "unclassified")]
cat(sprintf("rows after trim: %s (dropped %s never-scored/never-classified)\n",
            format(nrow(trimmed), big.mark = ","),
            format(nrow(keep) - nrow(trimmed), big.mark = ",")))

j <- toJSON(trimmed, na = "null", auto_unbox = TRUE, digits = 1)
cat(sprintf("JSON size: %s\n", format(object.size(j), units = "MB")))

writeLines(j, file.path(OUT, "meet_registry_artifact.json"))
cat("wrote meet_registry_artifact.json\n")
