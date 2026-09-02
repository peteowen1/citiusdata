# Export competition_catalogue.parquet to the JSON the meet-registry
# artifact embeds. Re-run this and republish the artifact (same file path,
# via the Artifact tool) whenever competition_catalogue.parquet changes in a
# way worth reflecting -- there is no automatic sync.
suppressMessages({library(data.table); library(arrow); library(jsonlite)})
OUT <- here::here("citiusdata", "data")

ct <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat(sprintf("total rows: %s\n", format(nrow(ct), big.mark = ",")))

keep <- ct[, .(competition_id, comp_name, class, strength, meet_tier, year,
               country, athletes, events, finals, is_major, is_global)]

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
