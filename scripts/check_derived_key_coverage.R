# Lean: two numeric-ish columns plus the key and source.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"
c0 <- setDT(read_parquet(file.path(D, "athletics_corpus.parquet"),
                         col_select=c("race_key","competition_id","source","scoreable","perf")))
c0 <- c0[scoreable==TRUE & is.finite(perf) & !is.na(race_key)]
c0[, derived := grepl("|AT-", race_key, fixed=TRUE)]
cat(sprintf("scoreable rows: %s | on a DERIVED key: %s (%.1f%%)\n",
            format(nrow(c0), big.mark=","), format(sum(c0$derived), big.mark=","),
            100*mean(c0$derived)))
cat("\n=== by harvest route ===\n")
print(c0[, .(rows=.N, derived=sum(derived), pct=round(100*mean(derived),1)), by=source])
# competitions where we hold ONLY career-route rows - those can never get an
# authoritative key, so their age divisions stay merged
cs <- c0[, .(rows=.N, has_comp = any(source=="competition")), by=competition_id]
cat(sprintf("\ncompetitions: %s | with NO competition-route coverage: %s (%.1f%%)\n",
            format(nrow(cs), big.mark=","), format(sum(!cs$has_comp), big.mark=","),
            100*mean(!cs$has_comp)))
cat(sprintf("rows inside those: %s (%.1f%% of scoreable)\n",
            format(sum(cs[has_comp==FALSE, rows]), big.mark=","),
            100*sum(cs[has_comp==FALSE, rows])/nrow(c0)))
cat("\nOur example 7230617:\n")
print(c0[competition_id=="7230617", .(rows=.N, derived=sum(derived)), by=source])
