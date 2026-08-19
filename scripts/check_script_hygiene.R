# Hygiene pass over every script touched this session. Parse is necessary but
# not sufficient, so also flag the specific mistakes this session actually made,
# which are the ones most likely to recur in the files I was editing fastest.
files <- c("analyse_form_history.R","backtest_athletics.R","build_athletics_corpus.R",
           "check_form_anchors.R","check_form_depth_bias.R","check_form_seed_collisions.R",
           "check_race_key_contiguity.R","form_display_marks.R","form_ratings.R",
           "make_tail_df_control.R","size_form_mark_level.R","verify_form_engine.R")
D <- "C:/dev/citiusverse/citiusdata/scripts"
bad <- character()
for (f in files) {
  p <- file.path(D, f)
  if (!file.exists(p)) { cat(sprintf("%-34s MISSING\n", f)); bad <- c(bad, f); next }
  ok <- tryCatch({ invisible(parse(p)); TRUE }, error = function(e) { cat(sprintf("%-34s PARSE FAIL: %s\n", f, conditionMessage(e))); FALSE })
  if (!ok) { bad <- c(bad, f); next }
  txt <- readLines(p, warn = FALSE)
  notes <- character()
  # setwd() under Rscript is a documented segfault in this tree
  if (any(grepl("^\\s*setwd\\(", txt))) notes <- c(notes, "setwd()")
  # 2>nul creates an undeletable file on Windows
  if (any(grepl("2>nul", txt, fixed = TRUE))) notes <- c(notes, "2>nul")
  # as.numeric(Sys.getenv(...)) is NA when the var is set to ""  -- this session's bug
  if (any(grepl("as\\.(numeric|integer)\\(Sys\\.getenv", txt))) notes <- c(notes, "as.numeric(Sys.getenv())")
  # a scratchpad path in a committed script will break for anyone else
  if (any(grepl("AppData/Local/Temp|AppData\\\\Local\\\\Temp", txt))) notes <- c(notes, "scratchpad path")
  cat(sprintf("%-34s OK%s\n", f, if (length(notes)) paste0("  <- ", paste(notes, collapse=", ")) else ""))
}
cat(sprintf("\n%d of %d parse; %d failed\n", length(files) - length(bad), length(files), length(bad)))
