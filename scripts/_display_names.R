# One display form for athlete names on the blog exports (citiusdata#55).
#
# The Games feeds write the SURNAME in capitals ("Edwin KURGAT") and leave the
# rest in title case, but not for everyone: "Ky Robinson" arrives plain. So the
# same person showed as "Edwin KURGAT" on the favourites table and as "Edwin
# Kurgat" in the results, and a join on the displayed name missed them.
#
# The capitals ARE the source's statement of which tokens are the surname, so
# the fix is to title-case exactly the all-caps tokens, in place, and leave
# everything else alone. That is deliberately not a render-side title-case of
# the whole string: a multi-part surname ("HOYE YENDA") is kept together because
# both of its tokens are caps, and no token order is ever changed. A name the
# source already wrote in title case, or in mixed case ("McLeod"), is untouched,
# so applying this twice gives the same answer as once.
#
# Known limit: it cannot restore internal capitals the source never sent
# ("MCLEOD" -> "Mcleod"), and a given name sent in caps ("JJ") is read as a
# surname. Both are rare, and both come out the same on every page.

#' Title-case the all-caps tokens of athlete names.
#'
#' @param x Character vector of names. NA stays NA.
#' @return Character vector, same length.
citius_display_name <- function(x) {
  x <- as.character(x)
  one <- function(nm) {
    if (is.na(nm) || !nzchar(nm)) return(nm)
    toks <- strsplit(nm, " ", fixed = TRUE)[[1L]]
    # "All caps" = has at least one cased letter and no lowercase one. Initials
    # ("J.") and tokens with no letters ("{", "-") are returned as they came.
    caps <- toks == toupper(toks) & toks != tolower(toks)
    toks[caps] <- gsub("(^|[-'’({])([[:alpha:]])", "\\1\\U\\2",
                       tolower(toks[caps]), perl = TRUE)
    paste(toks, collapse = " ")
  }
  vapply(x, one, character(1), USE.NAMES = FALSE)
}
