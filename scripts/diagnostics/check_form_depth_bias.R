suppressMessages(library(arrow)); suppressMessages(library(data.table))
h <- setDT(read_parquet("C:/dev/citiusverse/citiusdata/data/seqv3_history_final.parquet"))
p <- h[seen == TRUE & is.finite(r_pre) & is.finite(perf)]
p[, resid := perf - r_pre]
p[, floored := (0.95 * 3 / (n_eff + 3)) < 0.32]
cat("bias %, by evidence depth WITHIN round class (the depth effect, unconfounded)\n\n")
print(dcast(p[, .(bias = round(100*mean(resid), 4), n = .N), by = .(rc, floored)],
            rc ~ floored, value.var = c("bias", "n")))
cat("\nfinals only, by n_eff band:\n")
p[, band := cut(n_eff, c(-Inf,2,4,8,16,Inf),
                labels = c("1-2","2-4","4-8","8-16","16+"))]
print(p[rc == "final", .(n = .N, bias_pct = round(100*mean(resid), 4)), by = band][order(band)])
