# Does the new seed actually fix the collisions? Same measurement as before, on
# the same event, so the two numbers are comparable.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
.rk_seed <- function(k) {
  cp <- utf8ToInt(k); h <- 5381
  for (i in seq_along(cp)) h <- (h * 131 + cp[i]) %% 2147483647
  as.integer(h)
}
.old_seed <- function(k) sum(utf8ToInt(substr(k, 1, 20)))
x <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/data/athletics_corpus_store/event_id=AT-800Metres-W/part-0.parquet",
                        col_select = "race_key"))
k <- unique(x$race_key[!is.na(x$race_key)])
old <- vapply(k, .old_seed, numeric(1))
new <- vapply(k, .rk_seed, numeric(1))
cat(sprintf("distinct race keys        : %s\n", format(length(k), big.mark=",")))
cat(sprintf("distinct seeds, OLD hash  : %s  (%.1f%%)\n",
            format(uniqueN(old), big.mark=","), 100*uniqueN(old)/length(k)))
cat(sprintf("distinct seeds, NEW hash  : %s  (%.1f%%)\n",
            format(uniqueN(new), big.mark=","), 100*uniqueN(new)/length(k)))
cat(sprintf("largest collision group   : old %d, new %d races\n",
            max(table(old)), max(table(new))))
# and it must be deterministic across calls
stopifnot(identical(vapply(k[1:100], .rk_seed, numeric(1)), new[1:100]))
cat("seed is deterministic across calls: OK\n")
# a seed must be a valid set.seed argument
stopifnot(all(is.finite(new)), all(new == floor(new)))
cat("all seeds finite integers: OK\n")
