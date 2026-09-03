# Numeric environment variables, read so that a mistake is LOUD.
#
# THE BUG THIS EXISTS TO STOP. `as.numeric(Sys.getenv("X", "5"))` looks safe and
# is not. Sys.getenv returns the default only when X is UNSET; when X is set to
# the empty string it returns "", and as.numeric("") is NA - silently. The script
# then runs with NA where a threshold should be, and in R almost every comparison
# against NA yields NA, so filters match nothing and guards stop guarding without
# erroring.
#
# It is not a hypothetical. PowerShell's `$env:X = ''` sets a variable to empty
# rather than unsetting it, and this repo drives its sweeps with exactly that
# idiom - so a sweep that cleared a knob between arms would have silently run the
# rest with NA. form_ratings.R and form_display_marks.R each grew their own copy
# of this helper after being bitten; the other 48 scripts had not.
#
# The contract:
#   unset or empty  -> the default, which is the intent in both cases
#   set and numeric -> that value
#   set and NOT numeric -> stop() naming the variable and what it was set to,
#                          because a typo should not become a default
.env_num <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(trimws(v))) return(as.numeric(default))
  x <- suppressWarnings(as.numeric(v))
  if (is.na(x))
    stop(sprintf("%s='%s' is not a number - unset it to use the default (%s)",
                 name, v, format(default)), call. = FALSE)
  x
}

.env_int <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(trimws(v))) return(as.integer(default))
  x <- suppressWarnings(as.numeric(v))
  if (is.na(x))
    stop(sprintf("%s='%s' is not a number - unset it to use the default (%s)",
                 name, v, format(default)), call. = FALSE)
  if (x != as.integer(x))
    stop(sprintf("%s='%s' must be a whole number", name, v), call. = FALSE)
  as.integer(x)
}


# ---------------------------------------------------------------------------
# THE BUG THIS EXISTS TO STOP. `library(citius)` loads the INSTALLED package,
# not the source tree, and nothing anywhere compared the two. On 2026-09-03 the
# installed build was 0.1.0 while the source was 0.1.1, so
# build_athletics_corpus.R's DuckDB write-through called a function that did not
# exist in the build it was talking to. Its tryCatch downgraded that to a
# warning, the script exited 0, and build_stores.R's .stale_check then found
# DuckDB behind and silently fell back to RDS. Three layers, all reporting
# success, and citius.duckdb simply stopped being updated.
#
# Why this is a MIGRATION blocker and not a one-off: the RDS->DuckDB plan's step
# 4a is "stop writing RDS". Do that while this failure mode is live and a failed
# DuckDB write has no fallback left -- the shipping store would be built from
# data that never updated, with nothing failing anywhere to say so.
#
# Call this at the top of any script that writes to citius.duckdb or relies on a
# recently-added citius function.
#
#   strict = TRUE  -> stop(). Use in anything that WRITES.
#   strict = FALSE -> warn. Use in read-only diagnostics.
citius_version_guard <- function(strict = TRUE) {
  desc <- here::here("citius", "DESCRIPTION")
  if (!file.exists(desc)) {
    # SAY SO. Returning quietly here is the same vacuous-guard pattern this
    # function exists to catch: run from the wrong working directory,
    # here::here() resolves elsewhere, DESCRIPTION is not found, and the check
    # silently passes on nothing. A legitimate installed-only deployment and a
    # misresolved path are indistinguishable, so announce which one this is
    # rather than letting a no-op look like a pass.
    message(sprintf("citius_version_guard: no DESCRIPTION at %s -- version NOT checked (installed-only deployment, or here::here() resolved to the wrong root)", desc))
    return(invisible(NA))
  }
  src <- read.dcf(desc, fields = "Version")[[1]]
  inst <- tryCatch(as.character(utils::packageVersion("citius")),
                   error = function(e) NA_character_)
  if (is.na(inst)) {
    msg <- "citius is not installed, but scripts call it via citius::"
  } else if (!identical(src, inst)) {
    msg <- sprintf(paste0("citius INSTALLED %s but SOURCE is %s. `library(citius)` ",
                          "loads the installed build, so any function added since ",
                          "%s is missing at runtime. Run: Rscript -e \"devtools::install('citius', quick=TRUE)\""),
                   inst, src, inst)
  } else return(invisible(TRUE))
  if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  invisible(FALSE)
}
