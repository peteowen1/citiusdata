# CAN THE DATED WA RANKING ACTUALLY BE JOINED TO OUR RACES? Probe before scoring.
#
# Two joins have to work and both can fail silently. (1) athlete_id: WA's numeric
# id against ours - if the types or the id space differ the join returns zero
# rows and a coverage figure of 0%, which reads as "we hold few of these
# athletes" rather than "the join is broken". (2) event: WA's url slug against
# our event_id, which are unrelated naming schemes, plus sex. A slug that maps to
# nothing drops that event entirely and the benchmark then silently covers 14
# events while claiming 19.
#
# Report both as counts, not as a verdict.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- here::here("citiusdata", "data")

w <- setDT(read_parquet(file.path(D, "wa_rankings_dated.parquet")))
cat(sprintf("WA ranking rows: %s | dates: %s\n", format(nrow(w), big.mark = ","),
            paste(sort(unique(as.character(w$rank_date))), collapse = ", ")))
cat(sprintf("WA athlete_id class: %s | example: %s\n",
            class(w$athlete_id)[1], paste(head(w$athlete_id, 3), collapse = ", ")))

h <- setDT(read_parquet(file.path(D, "seqv3_history_final.parquet")))
cat(sprintf("history rows: %s | athlete_id class: %s | example: %s\n",
            format(nrow(h), big.mark = ","), class(h$athlete_id)[1],
            paste(head(h$athlete_id, 3), collapse = ", ")))

# --- join 1: athletes -------------------------------------------------------
wa_ids <- unique(as.character(w$athlete_id))
our_ids <- unique(as.character(h$athlete_id))
hit <- sum(wa_ids %chin% our_ids)
cat(sprintf("\nof %s ranked athletes, %s appear in our scored history (%.1f%%)\n",
            format(length(wa_ids), big.mark = ","), format(hit, big.mark = ","),
            100 * hit / length(wa_ids)))

# --- join 2: events ---------------------------------------------------------
cat("\nWA event slugs held:\n"); print(w[, .N, by = .(event_slug, sex)][order(event_slug)])
cat("\nour event_id values, a sample:\n")
print(head(sort(unique(h$event_id)), 40))
