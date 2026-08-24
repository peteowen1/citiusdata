# Hygiene pass over EVERY script in the repo, with an exit code that means it.
#
# This file used to check a hardcoded list of twelve scripts "touched this
# session" - a session that ended months ago. Two things were wrong with that:
#
#   1. It reported "11 of 12 parse; 1 failed" and then exited 0, so the guard
#      suite recorded it as PASS. A check whose exit code does not follow its own
#      verdict cannot fail, which means it cannot protect anything. The one
#      "failure" was a script that had since been deleted, so the message was
#      noise - but a real parse error would have been reported exactly the same
#      way and equally ignored.
#   2. A frozen list stops covering the repo the moment anyone adds a file. It
#      was checking 12 of ~215 scripts.
#
# Now it globs the directory, so new scripts are covered automatically and a
# deleted one cannot produce a phantom failure.
#
# WHAT FAILS THE BUILD versus what is only reported. A script that does not PARSE
# is broken for everyone and exits 1. The pattern notes below are advisory: each
# has legitimate uses, and failing on them would make the check something people
# route around. Same principle as the CI footgun scan.
suppressWarnings(suppressMessages(library(data.table)))
D <- Sys.getenv("HYGIENE_DIR", here::here("citiusdata", "scripts"))
stopifnot("script directory not found" = dir.exists(D))
files <- sort(list.files(D, pattern = "[.][Rr]$", full.names = FALSE))
# A scan that finds nothing passes while proving nothing - the vacuous-guard
# failure this repo keeps hitting.
stopifnot("found fewer than 50 scripts - check HYGIENE_DIR" = length(files) >= 50)
cat(sprintf("hygiene: %d scripts under %s\n\n", length(files), D))

broken <- character()
notes_all <- list()
for (f in files) {
  p <- file.path(D, f)
  ok <- tryCatch({ invisible(parse(p)); TRUE },
                 error = function(e) { cat(sprintf("%-38s PARSE FAIL: %s\n", f,
                                                   conditionMessage(e))); FALSE })
  if (!ok) { broken <- c(broken, f); next }
  # This file contains every pattern it looks for, in the grepl calls below, so
  # it matches itself on all of them. Skipping it is not special-casing a
  # failure - there is nothing to fix here, and leaving the self-match in would
  # train the reader to ignore the output.
  if (identical(f, "check_script_hygiene.R")) next
  txt <- readLines(p, warn = FALSE)
  code <- txt[!grepl("^\\s*#", txt)]          # advisory patterns, comments excluded
  notes <- character()
  # setwd() under Rscript is a documented segfault in this tree
  if (any(grepl("^\\s*setwd\\(", code))) notes <- c(notes, "setwd()")
  # 2>nul creates an undeletable file on Windows (reserved device name)
  if (any(grepl("2>nul", code, fixed = TRUE))) notes <- c(notes, "2>nul")
  # as.numeric(Sys.getenv(x)) is NA when the var is set to "" - the repo has
  # .env_num() for exactly this and it exists because this bit us
  if (any(grepl("as\\.(numeric|integer)\\(Sys\\.getenv", code)))
    notes <- c(notes, "as.numeric(Sys.getenv())")
  # a scratchpad path in a committed script breaks for everyone, including its
  # author once that session ends
  if (any(grepl("AppData/Local/Temp|AppData\\\\Local\\\\Temp", code)))
    notes <- c(notes, "scratchpad path")
  if (length(notes)) notes_all[[f]] <- notes
}

if (length(notes_all)) {
  cat(sprintf("=== advisory patterns in %d of %d scripts ===\n",
              length(notes_all), length(files)))
  tab <- sort(table(unlist(notes_all)), decreasing = TRUE)
  for (n in names(tab)) {
    who <- names(notes_all)[vapply(notes_all, function(v) n %in% v, logical(1))]
    cat(sprintf("\n%-28s %d script(s)\n", n, length(who)))
    cat(sprintf("   %s\n", paste(utils::head(who, 6), collapse = ", ")))
    if (length(who) > 6) cat(sprintf("   ... and %d more\n", length(who) - 6))
  }
  cat("\nAdvisory only - each has legitimate uses. Reported so they appear in a\n")
  cat("diff rather than in a wrong number.\n")
}

cat(sprintf("\n%d of %d scripts parse\n", length(files) - length(broken), length(files)))
if (length(broken)) {
  cat(sprintf("\nFAILED: %d script(s) do not parse:\n  %s\n",
              length(broken), paste(broken, collapse = "\n  ")))
  quit(status = 1)     # the exit code must follow the verdict
}
cat("all scripts parse\n")
