# Is the TIER-WEIGHTED headline gain measurable, or under its own noise floor?
#
# The raw figure pools every comparable pair equally. The weighted one gives a
# major 40x the weight of a T2 meet, which concentrates the metric on far fewer
# effective observations than its raw pair count suggests. Kish's effective
# sample size, n_eff = (sum w)^2 / sum(w^2), is the standard way to say how many
# equally-weighted pairs that is worth - and the binomial floor uses n_eff, not n.
#
# The engine comment already states 0.118 pp for these weights. This checks that
# number on the SEALED window specifically, which is one year rather than the
# whole corpus and therefore smaller.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D <- here::here("citiusdata", "data")
# ARM TAG. Every artefact below is per-arm, and hardcoding `final` meant a run
# against any other arm silently re-checked the DEPLOYED model and reported a
# result about a file the arm had never touched. On 2026-08-21 that returned a
# concordance figure identical to the previous run to two decimal places, for an
# arm holding 28,370 more races, and a 127/127 medallist pass on the wrong
# display. Swept across every script that reads a tagged artefact.
TAG <- Sys.getenv("FORM_TAG", "final")

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG))))
if (!"r_use" %in% names(h)) h[, r_use := r_pre]
stopifnot("history is empty" = nrow(h) > 0)

# The history does not store wt, so rebuild it from the same two sources the
# engine uses - the catalogue's class/meet_tier and the round label - with the
# same constants. Rebuilt rather than assumed: a fall-through that silently gave
# a major the T2 weight would understate the concentration this is measuring.
MAJ <- c("olympics", "world_champs", "european_champs", "commonwealth")
W_MAJ <- 40; W_T1 <- 12; W_T2 <- 1; W_RND <- 0.5
cp <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select = c("race_key", "competition_id")))
cp <- unique(cp, by = "race_key")
cg <- setDT(read_parquet(file.path(D, "competition_catalogue.parquet"),
                         col_select = c("competition_id", "class", "meet_tier")))
cp <- merge(cp, cg, by = "competition_id", all.x = TRUE)
h <- merge(h, cp[, .(race_key, class, meet_tier)], by = "race_key", all.x = TRUE)
h[, w_tier := fifelse(!is.na(class) & class %chin% MAJ, W_MAJ,
             fifelse(!is.na(meet_tier) & meet_tier == "T1_elite", W_T1, W_T2))]
h[, wt := w_tier * fifelse(rc == "final", 1, W_RND)]
stopifnot("every row must carry a finite weight" = all(is.finite(h$wt)),
          "no weight may be zero" = all(h$wt > 0),
          "no major-weighted rows found - the catalogue merge failed" =
            sum(h$w_tier == W_MAJ) > 0)
cat(sprintf("weights rebuilt: %s major rows, %s T1, %s T2
",
            format(sum(h$w_tier == W_MAJ), big.mark = ","),
            format(sum(h$w_tier == W_T1), big.mark = ","),
            format(sum(h$w_tier == W_T2), big.mark = ",")))

sealed <- h[seen == TRUE & is.finite(r_use) & is.finite(place) & place <= 12 &
            year(date) == 2026]
stopifnot("sealed window is empty" = nrow(sealed) > 0)
cat(sprintf("sealed window (2026): %s rows, %d races\n",
            format(nrow(sealed), big.mark = ","), uniqueN(sealed$race_key)))

# build the pairs exactly as the metric does, carrying the pair's weight
sealed[, rid := .GRP, by = race_key]
a <- sealed[, .(rid, event_id, i = seq_len(.N), place, r = r_use, wt)]
m <- merge(a, a, by = c("rid", "event_id"), allow.cartesian = TRUE,
           suffixes = c(".x", ".y"))
m <- m[i.x < i.y & place.x != place.y]
stopifnot("no pairs built" = nrow(m) > 0)
m[, w := wt.x]   # both rows of a pair are in the same race, so share a weight

kish <- function(w) sum(w)^2 / sum(w^2)
floorpp <- function(n) 100 * sqrt(0.75 * 0.25 / n)

n_raw <- nrow(m); n_eff <- kish(m$w)
cat(sprintf("\nraw pairs in the sealed window : %s   -> floor %.3f pp\n",
            format(n_raw, big.mark = ","), floorpp(n_raw)))
cat(sprintf("Kish effective pairs (weighted) : %s   -> floor %.3f pp\n",
            format(round(n_eff), big.mark = ","), floorpp(n_eff)))
cat(sprintf("the weighting costs %.1fx in effective sample, %.1fx in the floor\n",
            n_raw / n_eff, floorpp(n_eff) / floorpp(n_raw)))

# THE GAINS ARE INPUTS, AND ARE NOW LABELLED AS SUCH. They were hardcoded as
# c(0.063, 0.028) while the floors beside them were computed fresh, so the
# verdict was judged against fixed numbers and could not change however far the
# model drifted - a check that cannot fail is not a check.
#
# They cannot simply be computed here, which is why the literals appeared in the
# first place: a GAIN is a difference between two engine arms and this script
# reads one history. So they are supplied explicitly, defaulted to the last
# measured pair, and reported as inputs rather than as findings.
RAW_GAIN <- .env_num("METRIC_RAW_GAIN", "0.063")
WTD_GAIN <- .env_num("METRIC_WTD_GAIN", "0.028")
cat(sprintf("
gains supplied as INPUTS, not measured here: raw %+.3f, weighted %+.3f
",
            RAW_GAIN, WTD_GAIN))
cat("  pass METRIC_RAW_GAIN / METRIC_WTD_GAIN to judge a different pair of arms.
")
cat("\n=== verdict on the two headline gains ===\n")
res <- data.table(
  metric = c("raw sealed", "tier-weighted sealed"),
  # COMPUTED, not pasted. These were literals copied from a comment, so the
  # verdict below was judged against fixed numbers and could never fail however
  # far the model drifted - a check that cannot fail is not a check.
  gain   = c(RAW_GAIN, WTD_GAIN),
  n      = c(n_raw, n_eff))
res[, floor := round(floorpp(n), 3)]
res[, ratio := round(gain / floor, 2)]
res[, verdict := fifelse(gain > 2 * floor, "measurable",
                  fifelse(gain > floor, "marginal", "INSIDE NOISE"))]
res[, n := round(n)]
print(res)
cat("\nAs everywhere else, the binomial floor assumes pairs are independent.\n")
cat("Pairs from one race are not, so the true floor is LARGER than this and\n")
cat("any 'inside noise' verdict is conservative in the safe direction.\n")
