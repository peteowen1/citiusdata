# Is a CHAMPIONSHIP final systematically faster than an ordinary final, beyond
# what the round already captures?
#
# The display offset is fitted on rc == "final" pooled, so it answers
# "final vs heat" but not "championship vs Tuesday-night meet". The history has
# no class column, but race_key starts with competition_id, so join the
# catalogue back on.
suppressMessages(library(arrow)); suppressMessages(library(data.table))
OUT <- "C:/dev/citiusverse/citiusdata/data"
h <- setDT(read_parquet(file.path(OUT, "seqv3_history_final.parquet")))
cat0 <- setDT(read_parquet(file.path(OUT, "competition_catalogue.parquet")))
cat0[, competition_id := as.character(competition_id)]
h[, competition_id := tstrsplit(race_key, "|", fixed = TRUE)[[1]]]
h <- merge(h, cat0[, .(competition_id, class, meet_tier)], by = "competition_id", all.x = TRUE)
p <- h[seen == TRUE & is.finite(perf) & is.finite(r_pre)]
p[, resid := perf - r_pre]          # positive = ran BETTER than the rating

MAJ <- c("olympics","world_champs","european_champs","commonwealth","world_indoor",
         "continental","world_other")
p[, occasion := fifelse(class %chin% MAJ, "championship", "ordinary")]

cat("residual (%, positive = outran the rating) by ROUND and OCCASION\n")
cat("finals only rows are what the display offset is fitted on\n\n")
print(dcast(p[!is.na(class), .(n = .N, bias = round(100*mean(resid), 3)),
              by = .(rc, occasion)],
            rc ~ occasion, value.var = c("bias","n")))

cat("\nFINALS ONLY, by occasion:\n")
print(p[rc == "final" & !is.na(class),
        .(n = .N, bias_pct = round(100*mean(resid), 3),
          sd_pct = round(100*stats::sd(resid), 2)), by = occasion])

cat("\nand within championships, by class:\n")
print(p[rc == "final" & class %chin% MAJ,
        .(n = .N, bias_pct = round(100*mean(resid), 3)), by = class][order(-bias_pct)])

cat("\nTHE CONTENDERS specifically — championship finalists who placed top 3:\n")
print(p[rc == "final" & class %chin% MAJ,
        .(n = .N, bias_pct = round(100*mean(resid), 3)),
        by = .(medallist = place <= 3)][order(-medallist)])
