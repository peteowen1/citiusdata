# Compare variance-prior arms on the two things that matter, which are not the
# same thing and do not have to agree.
#
# WHY TWO METRICS. The prior sets how uncertain a THIN record is. Widening it
# barely moves the ordering, because a variance affects the learning rate rather
# than the rating itself, and the ordering metric is where this repo does most of
# its judging. So sealed weighted concordance is the guard - it says "this did
# not break anything" - and it is NOT the thing being optimised. The thing being
# optimised is whether the stated uncertainty is honest for an athlete with
# almost no evidence, which concordance cannot see at all.
#
# THE CALIBRATION METRIC IS ROBUST, DELIBERATELY. sd(z) is inflated by a fat tail
# that no variance can fix: |z| > 5 occurs 1.1% of the time against a normal's
# 0.00006%. Chasing sd(z) = 1 would therefore widen the prior far past honest.
# MAD is normal-consistent and ignores the tail, so it answers the question
# actually being asked - is the middle of the distribution the right width?
#
# And the residual is standardised by v_pre PLUS the per-event race-conditions
# variance, because `perf - r_pre = surprise + shock` and v only ever learned the
# surprise. Without that term every arm looks equally over-confident and the
# comparison measures nothing.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")
ARMS <- trimws(strsplit(Sys.getenv("ARMS", ""), ",")[[1]])
stopifnot("set ARMS to a comma-separated list of tags" = length(ARMS) >= 2)

# Provenance first. Arms built by different engine versions are not comparable,
# and this repo has drawn two wrong conclusions from exactly that.
meta <- rbindlist(lapply(ARMS, function(t) {
  f <- file.path(D, sprintf("seqv3_meta_%s.json", t))
  if (!file.exists(f)) return(data.table(arm = t, sha = NA_character_, written = NA_character_))
  j <- jsonlite::fromJSON(f); if (is.data.frame(j)) j <- as.list(j[1, ])
  data.table(arm = t, sha = substr(j$engine_sha, 1, 12), written = j$written)
}), fill = TRUE)
print(meta)
stopifnot("an arm is missing its meta file" = !any(is.na(meta$sha)))
if (uniqueN(meta$sha) > 1)
  stop("these arms were built by DIFFERENT engine versions - rebuild before comparing")
cat(sprintf("provenance: all %d arms on sha %s\n\n", nrow(meta), meta$sha[1]))

one <- function(tag) {
  h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", tag))))
  h <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre) & is.finite(v_pre) &
         v_pre > 0 & is.finite(shock) & is.finite(place) & place > 0]
  # per-event race-conditions variance, one value per race not per row
  rs <- unique(h[, .(race_key, event_id, shock)])
  vs <- rs[, .(v_shock = stats::var(shock), n = .N), by = event_id]
  vp <- stats::var(rs$shock); vs[n < 30, v_shock := vp]
  h <- merge(h, vs[, .(event_id, v_shock)], by = "event_id", all.x = TRUE)
  h[!is.finite(v_shock), v_shock := vp]
  h[, resid_c := (perf - r_pre) - stats::median(perf - r_pre), by = event_id]
  h[, z := resid_c / sqrt(v_pre + v_shock)]
  h[, band := cut(n_eff, c(-Inf, 1, 3, 8, Inf),
                  labels = c("cold <1", "thin 1-3", "mid 3-8", "deep 8+"))]
  cal <- h[, .(mad = round(stats::mad(z), 3)), by = band]
  cal <- dcast(cal, . ~ band, value.var = "mad")[, -1]
  setnames(cal, paste0("mad_", gsub("[^a-z0-9]+", "", tolower(names(cal)))))
  cbind(data.table(arm = tag, races = nrow(h)), cal)
}
cal <- rbindlist(lapply(ARMS, one), fill = TRUE)
cat("=== calibration: robust scale of z by evidence band (target 1.000) ===\n")
print(cal)
cat("\nThe COLD column is what this sweep is for. 1.000 means a debutant's stated\n")
cat("uncertainty is honest; above 1 means the prior is too tight, below 1 too wide.\n")
cat("The deep column should barely move - if it does, the prior is reaching\n")
cat("records it has no business affecting.\n")

f <- file.path(D, "prior_arm_calibration.json")
writeLines(jsonlite::toJSON(list(arms = meta, calibration = cal),
                            dataframe = "rows", auto_unbox = TRUE, na = "null"), f)
cat(sprintf("\nwrote %s\n", basename(f)))
cat("\nNow run score_by_event.R over the same ARMS for the ordering guard:\n")
cat(sprintf("  ARMS=\"%s\" Rscript citiusdata/scripts/score_by_event.R\n",
            paste(ARMS, collapse = ",")))
