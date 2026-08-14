suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
# one event only — keep the footprint tiny while a sweep arm is running
x <- setDT(read_parquet(file.path(OUT, "athletics_corpus_store/event_id=AT-800Metres-W/part-0.parquet"),
                        col_select = c("race_key", "date", "round")))
x <- x[!is.na(race_key)]
k <- unique(x$race_key)
cat(sprintf("event AT-800Metres-W: %s rows, %s distinct race_key\n",
            format(nrow(x), big.mark=","), format(length(k), big.mark=",")))
cat("\nfive example keys:\n"); print(utils::head(k, 5))
cat(sprintf("\nkey length: median %.0f, min %d, max %d\n",
            stats::median(nchar(k)), min(nchar(k)), max(nchar(k))))

pre <- substr(k, 1, 20)
seed <- vapply(pre, function(s) sum(utf8ToInt(s)), numeric(1))
cat(sprintf("\ndistinct keys           : %d\n", length(k)))
cat(sprintf("distinct 20-char prefixes: %d  (%.1f%% of keys)\n",
            uniqueN(pre), 100*uniqueN(pre)/length(k)))
cat(sprintf("distinct SEEDS           : %d  (%.1f%% of keys)\n",
            uniqueN(seed), 100*uniqueN(seed)/length(k)))
cat(sprintf("seed range               : %d to %d\n", min(seed), max(seed)))

# worst collision group, and whether it merges rounds of one meet
tb <- data.table(k = k, pre = pre, seed = seed)
worst <- tb[, .N, by = seed][order(-N)][1]
cat(sprintf("\nlargest single seed collides %d distinct races; examples:\n", worst$N))
print(utils::head(tb[seed == worst$seed, k], 6))
