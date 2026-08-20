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
