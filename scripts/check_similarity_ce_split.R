# Is an event-pair correlation real, or is it just decathletes?
#
# THE SUSPICION, and it is a good one. The similarity matrix correlates athletes
# rated in both events. For a pair like Shot Put and Long Jump, a large share of
# those shared athletes are decathletes and heptathletes - people who contest
# both because the combined event MAKES them, not because the two events share
# an ability that transfers. If the correlation is carried entirely by that
# group, the matrix is measuring "combined-event athletes are good at everything
# or bad at everything" and then applying it to specialists, for whom it may not
# hold at all.
#
# THE TEST. Split the shared athletes of every pair into:
#   CE-LINKED   - at least one of the two events is contested by that athlete
#                 mostly through combined events (majority of their marks in it
#                 carry a combined-event round code)
#   SPECIALIST  - both events contested mainly in their own right
# and correlate separately. A pair where the two differ sharply is a pair where
# a single number is the wrong model.
#
# WHY IT MATTERS CONCRETELY: the display blend now borrows across events for any
# athlete with a thin record. If Shot Put borrows from Long Jump on a correlation
# that only exists among decathletes, a thin specialist shot putter is being
# adjusted on evidence that does not describe them.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D      <- here::here("citiusdata", "data")
TAG    <- Sys.getenv("STATE_TAG", "base4")
MIN_NE <- as.numeric(Sys.getenv("MIN_NEFF", "1"))
MIN_SH <- as.integer(Sys.getenv("MIN_SHARED", "20"))  # need enough in BOTH arms
CE_EVENTS <- c("AT-Decathlon-M", "AT-Heptathlon-M", "AT-Heptathlon-W", "AT-Pentathlon-W")

# --- who contests what, and how -----------------------------------------------
cols <- c("event_id", "athlete_id", "round", "mark")
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"), col_select = cols))
c0[, athlete_id := as.character(athlete_id)]
c0 <- c0[is.finite(mark) & mark > 0 & !(event_id %chin% CE_EVENTS)]
c0[, is_ce := grepl("^(CE[0-9]*|Combined.*)$", round)]
ce_share <- c0[, .(marks = .N, ce_marks = sum(is_ce, na.rm = TRUE)), by = .(athlete_id, event_id)]
ce_share[, ce_frac := ce_marks / marks]
cat(sprintf("athlete-events: %s | contested mostly via a combined event: %s (%.1f%%)\n",
            format(nrow(ce_share), big.mark = ","),
            format(sum(ce_share$ce_frac > 0.5), big.mark = ","),
            100 * mean(ce_share$ce_frac > 0.5)))

# --- ratings, standardised within event, exactly as build_event_similarity does
s <- setDT(read_parquet(file.path(D, sprintf("seqv2_state_%s.parquet", TAG))))
s[, athlete_id := as.character(athlete_id)]
rcol <- intersect(c("R", "r", "rating"), names(s))[1]
setnames(s, rcol, "R")
s <- s[is.finite(R) & is.finite(n_eff) & n_eff >= MIN_NE & !(event_id %chin% CE_EVENTS)]
s[, z := (R - mean(R)) / stats::sd(R), by = event_id]
s <- s[is.finite(z)]
s <- merge(s[, .(athlete_id, event_id, z)], ce_share[, .(athlete_id, event_id, ce_frac)],
           by = c("athlete_id", "event_id"), all.x = TRUE)
s[is.na(ce_frac), ce_frac := 0]
cat(sprintf("rated athlete-events used: %s over %d events\n",
            format(nrow(s), big.mark = ","), uniqueN(s$event_id)))

# --- pairwise, split by how the athlete came to contest both ------------------
sm <- merge(s[, .(athlete_id, e1 = event_id, z1 = z, f1 = ce_frac)],
            s[, .(athlete_id, e2 = event_id, z2 = z, f2 = ce_frac)],
            by = "athlete_id", allow.cartesian = TRUE)
sm <- sm[e1 < e2]
sm[, grp := fifelse(f1 > 0.5 | f2 > 0.5, "ce_linked", "specialist")]
cat(sprintf("\nshared-athlete pair rows: %s | ce_linked %.1f%%\n",
            format(nrow(sm), big.mark = ","), 100 * mean(sm$grp == "ce_linked")))

sim <- sm[, .(n = .N, cor = if (.N >= MIN_SH) stats::cor(z1, z2) else NA_real_),
          by = .(e1, e2, grp)]
w <- dcast(sim, e1 + e2 ~ grp, value.var = c("n", "cor"))
setnames(w, c("n_ce_linked", "n_specialist", "cor_ce_linked", "cor_specialist"),
         c("n_ce", "n_sp", "cor_ce", "cor_sp"), skip_absent = TRUE)
w <- w[is.finite(cor_ce) & is.finite(cor_sp)]
w[, gap := cor_ce - cor_sp]
stopifnot("no pair has enough athletes in BOTH arms" = nrow(w) > 0)

reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]
lab <- function(x) reg$discipline[match(x, reg$event_id)]
fam <- function(x) reg$family[match(x, reg$event_id)]
w[, `:=`(d1 = lab(e1), d2 = lab(e2), f1 = fam(e1), f2 = fam(e2),
         same_family = fam(e1) == fam(e2))]

cat(sprintf("\npairs with >= %d athletes in BOTH arms: %d\n", MIN_SH, nrow(w)))
cat("\n=== overall: does the combined-event group correlate more? ===\n")
print(w[, .(pairs = .N,
            mean_ce = round(mean(cor_ce), 3),
            mean_specialist = round(mean(cor_sp), 3),
            mean_gap = round(mean(gap), 3),
            ce_higher = sum(gap > 0))])
cat("A positive mean gap means the shared correlation is carried disproportionately\n")
cat("by athletes whom a combined event forced into both events.\n")

cat("\n=== by whether the pair crosses a family boundary ===\n")
print(w[, .(pairs = .N, mean_ce = round(mean(cor_ce), 3),
            mean_specialist = round(mean(cor_sp), 3),
            mean_gap = round(mean(gap), 3)), by = same_family])

cat("\n=== the pairs where a single correlation is most misleading ===\n")
setorder(w, -gap)
print(head(w[, .(a = d1, b = d2, sex = reg$sex[match(e1, reg$event_id)],
                 n_ce, n_sp, ce = round(cor_ce, 3), specialist = round(cor_sp, 3),
                 gap = round(gap, 3))], 15))
cat("\nand where SPECIALISTS agree more than combined-event athletes do:\n")
setorder(w, gap)
print(head(w[, .(a = d1, b = d2, sex = reg$sex[match(e1, reg$event_id)],
                 n_ce, n_sp, ce = round(cor_ce, 3), specialist = round(cor_sp, 3),
                 gap = round(gap, 3))], 10))

f <- file.path(D, "event_similarity_ce_split.parquet")
write_parquet(w[, .(e1, e2, d1, d2, same_family, n_ce, n_sp, cor_ce, cor_sp, gap)], f)
cat(sprintf("\nwrote %s (%d pairs)\n", basename(f), nrow(w)))
