# What does each event borrow from, and in what proportion?
#
# Two weights, and they are easy to conflate:
#   1. SHARE AMONG SIBLINGS is proportional to r^2 x n_eff(sibling). r^2 sets
#      the share; the sibling's own evidence scales it, so a 0.71 correlation to
#      an event the athlete has raced twice counts less than the same 0.71 to
#      one they have raced ten times. The share column below holds n_eff equal
#      so the r^2 structure is visible on its own - a real athlete's shares move
#      with how much they have raced each sibling.
#   2. HOW MUCH IS BORROWED AT ALL is w = xb / (n_eff_own + xb), applied only
#      when n_eff_own <= FORM_XB_MAXN. That is per athlete, not per event.
suppressMessages(devtools::load_all(here::here("citius"), quiet = TRUE))
suppressMessages(library(arrow)); suppressMessages(library(data.table))
source(here::here("citiusdata", "scripts", "_env.R"))
D    <- here::here("citiusdata", "data")
SIMF <- Sys.getenv("SIM_FILE", "event_similarity_spec.parquet")
# the all-athletes matrix is carried alongside so the combined-event inflation is
# visible rather than merely corrected out of sight
SIMF_ALL <- Sys.getenv("SIM_FILE_ALL", "event_similarity_all.parquet")
MINCOR <- .env_num("FORM_XB_MINCOR", "0.30")
NSIB   <- .env_int("FORM_XB_NSIB", "6")
TOPN   <- .env_int("TOPN", "10")

sim <- setDT(read_parquet(file.path(D, SIMF)))
scol <- if ("cor_use" %chin% names(sim)) "cor_use" else "cor"
sim[, corv := as.numeric(get(scol))]
alls <- setDT(read_parquet(file.path(D, SIMF_ALL)))
acol <- if ("cor_use" %chin% names(alls)) "cor_use" else "cor"
alls[, r_all := as.numeric(get(acol))]
sim <- merge(sim, alls[, .(e1, e2, r_all)], by = c("e1", "e2"), all.x = TRUE)
reg <- as.data.table(citius::citius_events())[, .(event_id, discipline, sex, family)]
lab <- function(x) reg$discipline[match(x, reg$event_id)]
fam <- function(x) reg$family[match(x, reg$event_id)]
sx  <- function(x) reg$sex[match(x, reg$event_id)]

two <- rbindlist(list(sim[, .(ev = e1, sv = e2, shared, cor, corv, r_all)],
                      sim[, .(ev = e2, sv = e1, shared, cor, corv, r_all)]))
setorder(two, ev, -corv)
two[, rank := seq_len(.N), by = ev]
# share among the siblings the engine would actually keep
two[, used := rank <= NSIB & corv >= MINCOR]
two[, r2 := corv^2]
# `used` must never be NA: fifelse propagates it, and sum(r2[used]) then returns
# NA for the WHOLE event, blanking share for every otherwise-valid sibling.
two[!is.finite(corv), used := FALSE]
two[is.na(used), used := FALSE]
two[, share := fifelse(used, 100 * r2 / sum(r2[used]), NA_real_), by = ev]

out <- two[rank <= TOPN, .(
  event = lab(ev), sex = sx(ev), family = fam(ev), event_id = ev,
  neighbour = lab(sv), nb_family = fam(sv), shared,
  r = round(cor, 3), r_used = round(corv, 3), r2 = round(r2, 3),
  r_all = round(r_all, 3), share = round(share, 1), used)]
setorder(out, family, event, sex, -r_used)
f <- file.path(D, "event_siblings_report.parquet")
write_parquet(out, f)
cat(sprintf("wrote %s: %d rows over %d events (top %d each)\n",
            basename(f), nrow(out), uniqueN(out$event_id), TOPN))
cat(sprintf("gate %.2f, up to %d siblings used; column %s from %s\n",
            MINCOR, NSIB, scol, SIMF))

jf <- file.path(D, "event_siblings_report.json")
writeLines(jsonlite::toJSON(out, dataframe = "rows", auto_unbox = TRUE, na = "null"), jf)
cat(sprintf("wrote %s\n", basename(jf)))

for (probe in c("AT-10000Metres-M", "AT-1500Metres-M", "AT-Decathlon-M", "AT-ShotPut-W")) {
  x <- out[event_id == probe]
  if (!nrow(x)) {
    cat(sprintf("\n== %s == NO ROWS - this event produced no siblings at all\n", probe))
    next
  }
  cat(sprintf("\n== %s ==\n", probe))
  print(x[, .(neighbour, nb_family, shared, r = r_used, r2, `share%` = share, used)])
}
