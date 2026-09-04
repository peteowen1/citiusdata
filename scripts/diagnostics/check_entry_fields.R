# Two questions, cheapest first.
# 1. Do we ALREADY hold prior results for athletes the corpus treats as cold
#    starts? If the corpus is a filtered subset (tier, date, event), a debut
#    prior costs a join, not a scraper.
# 2. Do the cached competition payloads carry a seed / PB / SB field at all?
suppressMessages(library(arrow)); suppressMessages(library(data.table))
D <- "C:/dev/citiusverse/citiusdata/data"

cat("=== 1. cached competition payload: what fields exist? ===\n")
f <- list.files(file.path(D, "ath_comp_cache"), full.names = TRUE)[1]
x <- readRDS(f)
cat("top-level class:", class(x), "\n")
str(x, max.level = 2, list.len = 25)
fl <- function(z) if (is.data.frame(z)) names(z) else if (is.list(z)) unlist(lapply(z, fl)) else NULL
nmz <- unique(unlist(lapply(x, fl)))
cat("\nall nested column names:\n"); print(nmz)
hit <- grep("best|seed|pb|sb|entry|rank", nmz, ignore.case = TRUE, value = TRUE)
cat("\nfields that look like a pre-race mark:\n"); print(hit)

cat("\n=== 2. do we hold results the corpus drops? ===\n")
st <- list.files(file.path(D, "athletics_careers_store"), pattern = "parquet$",
                 recursive = TRUE, full.names = TRUE)
cat("careers store files:", length(st), "\n")
if (length(st)) {
  ca <- setDT(read_parquet(st[1]))
  cat("careers columns:\n"); print(names(ca))
  cat(sprintf("sample file rows: %s\n", format(nrow(ca), big.mark = ",")))
}
