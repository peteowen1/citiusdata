# Does a venue's effect depend on the meet type held there? (#2, 2026-09-19)
# Middle distance is the one family where the venue conditions model still
# trails the August file, and the Zurich/Monaco cells showed invitational
# fields inflating a venue offset. If the residual at a venue differs
# systematically between its Diamond League / championship races and the rest,
# the offset needs a venue x meet-type level; if not, the gap is elsewhere.
#
# Reads adjusted_marks_<tag>.parquet (default v8): the row's residual after
# conditions, venue and the athlete's own level, BEFORE the race shock, is
# adj_perf + race_shock + venue_off - level ... i.e. cleaned - level. Units:
# perf-log x100 = % of mark; positive = faster than the athlete's own level.
suppressMessages({ library(arrow); library(data.table) })
D <- here::here("citiusdata", "data")
TAG <- Sys.getenv("ADJ_TAG", "adjusted_marks_v8")
a <- setDT(read_parquet(file.path(D, paste0(TAG, ".parquet")),
                        col_select = c("event_id", "family", "venue_city", "date", "race_key", "adj_perf", "race_shock", "venue_off", "level", "covered")))
a <- a[covered == TRUE & is.finite(level)]
a[, resid := adj_perf + race_shock + venue_off - level]
# race_code is not in the adjusted file; take it from the store per race_key
rc <- setDT(open_dataset(file.path(D, "athletics_corpus_store")) |> dplyr::select(race_key, race_code) |> dplyr::collect())
rc <- unique(rc[!is.na(race_code)], by = "race_key")
a <- merge(a, rc, by = "race_key", all.x = TRUE)
a[, meet_type := fifelse(race_code %chin% c("OW", "GW", "GL", "A", "B"), "elite", fifelse(is.na(race_code), "unknown", "other"))]
cat(sprintf("rows %s; meet_type split: elite %.1f%%, other %.1f%%, unknown %.1f%%\n", format(nrow(a), big.mark = ","),
            100*mean(a$meet_type == "elite"), 100*mean(a$meet_type == "other"), 100*mean(a$meet_type == "unknown")))
# per (event, venue): mean residual by meet type, venues that host both
v <- a[meet_type != "unknown", .(n = .N, m = mean(resid)), by = .(event_id, family, venue_city, meet_type)]
w <- dcast(v[n >= 20], event_id + family + venue_city ~ meet_type, value.var = c("m", "n"))
w <- w[is.finite(m_elite) & is.finite(m_other)]
w[, gap := m_elite - m_other]
cat(sprintf("\n(event, venue) cells hosting both elite and other races with >= 20 rows each: %d\n", nrow(w)))
cat("mean residual gap (elite - other) at the SAME venue, % of mark; a positive gap means elite fields at a venue run above their own levels beyond what the venue explains:\n")
print(w[, .(cells = .N, mean_gap_pct = round(100*mean(gap), 2), median_gap_pct = round(100*median(gap), 2), share_pos = round(mean(gap > 0), 2)), by = family][order(-mean_gap_pct)])
cat("\nlargest gaps, middle distance:\n")
print(w[family == "middle"][order(-abs(gap))][1:12, .(event_id, venue_city, n_elite, n_other, elite_pct = round(100*m_elite, 2), other_pct = round(100*m_other, 2), gap_pct = round(100*gap, 2))])
cat("\nIf mean_gap is clearly positive and consistent, a venue x meet-type level (or a meet-type main effect applied before the venue estimate) is the missing term.\n")
