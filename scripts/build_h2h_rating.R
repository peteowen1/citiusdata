# A pure HEAD-TO-HEAD rating, as a benchmark for the form model.
#
# WHY. The form model is FITTED on times and SCORED on ordering, and every
# failure found today lives in that gap: a tactical championship win is a bad
# TIME and a good RESULT, and only the time reaches the rating. Almgren wins the
# European 10,000m in 27:23 off slow tactical wins in 28:53 and 28:49, and the
# model rates him 17th. A rating that only knows WHO BEAT WHOM cannot make that
# mistake - a win is a win however slow.
#
# So: fit one, and put it in the benchmark table beside season-best and the
# rest. If it beats the form model on correct pairs - especially in the middle
# distances and the hurdles, where the model's edge is thin or negative - that is
# an argument for blending the two rather than an opinion about it.
#
# This is Bradley-Terry fitted ONLINE, which is what Elo is. A full BT refit
# before every one of 96,681 races is not affordable and would not be more
# honest: the online form is strictly walk-forward, which is the property that
# matters. Ratings are per athlete-EVENT, like the form model's, so the two are
# comparable rather than one enjoying cross-event information the other lacks.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D   <- here::here("citiusdata", "data")
TAG <- Sys.getenv("FORM_TAG", "final")
K0  <- .env <- as.numeric(Sys.getenv("H2H_K0", "60"))
KH  <- as.numeric(Sys.getenv("H2H_KHALF", "5"))   # races at which K has halved
SC  <- as.numeric(Sys.getenv("H2H_SCALE", "400"))

h <- setDT(read_parquet(file.path(D, sprintf("seqv3_history_%s.parquet", TAG)),
                        col_select = c("race_key","date","event_id","athlete_id",
                                       "place","seen")))
h <- h[is.finite(place) & place > 0]
h[, athlete_id := as.character(athlete_id)]
setorder(h, date, race_key)
h[, key := paste0(athlete_id, "|", event_id)]
cat(sprintf("races %s | athlete-races %s\n",
            format(uniqueN(h$race_key), big.mark = ","),
            format(nrow(h), big.mark = ",")))

R <- new.env(hash = TRUE, parent = emptyenv())   # rating
N <- new.env(hash = TRUE, parent = emptyenv())   # races seen
h[, elo_pre := NA_real_]
h[, elo_n := NA_real_]

idx <- split(seq_len(nrow(h)), h$race_key)
# preserve date order: split() returns keys alphabetically, which would make the
# walk-forward claim false
ord <- unique(h$race_key)
idx <- idx[ord]
t0 <- Sys.time()
for (ii in idx) {
  kk <- h$key[ii]
  cur <- vapply(kk, function(k) { v <- R[[k]]; if (is.null(v)) 1500 else v }, numeric(1))
  nn  <- vapply(kk, function(k) { v <- N[[k]]; if (is.null(v)) 0 else v }, numeric(1))
  set(h, ii, "elo_pre", cur)
  set(h, ii, "elo_n", nn)
  pl <- h$place[ii]
  n <- length(ii)
  if (n >= 2) {
    # expected score against the field, and the score actually taken
    d <- outer(cur, cur, "-") / SC
    p <- 1 / (1 + 10^(-d)); diag(p) <- 0
    expd <- rowSums(p)
    act <- vapply(seq_len(n), function(i) sum(pl[i] < pl[-i]) + 0.5 * sum(pl[i] == pl[-i]),
                  numeric(1))
    kv <- K0 / (1 + nn / KH)
    new <- cur + kv * (act - expd)
    for (i in seq_len(n)) { R[[kk[i]]] <- new[i]; N[[kk[i]]] <- nn[i] + 1 }
  }
}
cat(sprintf("fitted in %.1f min | rated athlete-events %s\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")),
            format(length(ls(R)), big.mark = ",")))

f <- file.path(D, sprintf("h2h_history_%s.parquet", TAG))
write_parquet(h[, .(race_key, athlete_id, event_id, date, place, elo_pre, elo_n)], f)
cat(sprintf("wrote %s\n", basename(f)))

# ANCHOR: the rating must order a field better than chance, and the very first
# race of an athlete-event carries the default 1500 - if that is not true the
# walk-forward is broken and every number downstream is meaningless.
chk <- h[elo_n > 0]
cat(sprintf("\nrows with a prior rating: %s of %s\n",
            format(nrow(chk), big.mark = ","), format(nrow(h), big.mark = ",")))
stopifnot("no athlete ever accumulated a rating" = nrow(chk) > 0,
          "debut rows should carry the 1500 default" =
            all(abs(h[elo_n == 0, elo_pre] - 1500) < 1e-9))
