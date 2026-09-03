# Publish citiusdata artifacts to GitHub Releases -- the release-as-data-bus
# pattern the other three verses already use, and which citiusdata's README has
# described since day one while having ZERO releases.
#
# THE PATTERN, copied from torpdata/torp rather than invented:
#   * one release per logical DATASET, not per file
#   * the release TAG is the dataset name; torpdata suffixes "-data", pannadata
#     and bouncerdata mostly do not. Following torpdata, since torp's loader is
#     the one that reads a constructed URL and is the model for citius's
#     read side:
#         https://github.com/<repo>/releases/download/<tag>/<file>
#     (torp/R/load_utils.R:252)
#   * upload with piggyback::pb_upload(), then VERIFY -- torp/R/load_data.R
#     deliberately re-checks after upload rather than trusting a 2xx, because a
#     success code does not prove the asset is retrievable.
#
# NOT YET DONE, and deliberately out of scope here: the READ side. Nothing in
# citius/R downloads from releases -- the loaders read local files only. So
# these releases are backup and distribution today, not a live dependency.
# Adding a download path is a separate change, and should come with a decision
# about partitioning: athletics_corpus.parquet is 320 MB as one file, where torp
# splits by season precisely so a single round does not pull the whole thing.
#
# Usage:
#   Rscript citiusdata/scripts/publish_release.R            # validate + small files
#   CITIUS_PUBLISH_ALL=1 Rscript ...                        # include the big corpus
suppressMessages({library(piggyback); library(data.table)})
D <- here::here("citiusdata", "data")
REPO <- "peteowen1/citiusdata"

# tag = dataset, file = the artifact. Smallest first: the mechanism is being
# proven here, and proving it with a 1 MB file costs seconds rather than
# uploading 320 MB to discover the tag was wrong.
ARTIFACTS <- list(
  list(tag = "catalogue-data", file = "competition_catalogue.parquet",   big = FALSE),
  list(tag = "catalogue-data", file = "competition_name_lookup.parquet", big = FALSE),
  list(tag = "corpus-data",    file = "athletics_corpus.parquet",        big = TRUE)
)
INCLUDE_BIG <- nzchar(Sys.getenv("CITIUS_PUBLISH_ALL"))

# LIST AND CREATE VIA THE RAW API, NOT PIGGYBACK.
#
# piggyback MEMOISES the release list per session. Calling pb_releases() before
# creating anything caches the empty result, and pb_upload() then fails with
# 'Release "catalogue-data" not found' having just created it -- the script
# looks broken on a first run against a repo with no releases, which is exactly
# the case it exists to handle. Nothing clears that cache from inside the run
# (pb_cache_clear() does not exist in 0.1.5; calling it under try() silently did
# nothing, which is a vacuous guard of the kind this repo keeps finding).
#
# So: do listing and creation through gh::gh, and let piggyback's cache be
# populated for the first time only when pb_upload() needs it, by which point
# every release already exists.
.owner <- sub("/.*", "", REPO); .name <- sub(".*/", "", REPO)
gh_releases <- function() {
  r <- tryCatch(gh::gh("/repos/{owner}/{repo}/releases", owner = .owner, repo = .name,
                       .limit = Inf), error = function(e) list())
  vapply(r, function(x) x$tag_name %||% NA_character_, character(1))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
existing <- gh_releases()
cat(sprintf("existing releases: %s\n",
            if (length(existing)) paste(existing, collapse = ", ") else "(none)"))

# Decide what is in scope first, so PASS 1 creates every release this run will
# need before PASS 2 touches piggyback at all.
todo <- Filter(function(a) {
  p <- file.path(D, a$file)
  if (!file.exists(p)) { cat(sprintf("SKIP %-36s (missing)\n", a$file)); return(FALSE) }
  if (a$big && !INCLUDE_BIG) {
    cat(sprintf("SKIP %-36s %7.1f MB (set CITIUS_PUBLISH_ALL=1)\n",
                a$file, file.size(p)/1048576)); return(FALSE)
  }
  TRUE
}, ARTIFACTS)

# PASS 1 -- create every missing release, BEFORE any pb_upload() call.
#
# Doing this per-artifact was still wrong: piggyback populates its cache on the
# FIRST pb_upload(), so a release created after that point is invisible for the
# rest of the run. The first two files uploaded fine and `corpus-data`, created
# afterwards, failed with 'Release not found' despite existing on GitHub. The
# cache is only safe once every release already exists when it is built.
# CREATION IS IDEMPOTENT. GitHub's own list endpoint returned a STALE read on
# 2026-09-03 -- it omitted `corpus-data` while simultaneously rejecting the
# create with 422 already_exists, so the script aborted over a release that was
# already there. Three caching layers were in play by then (piggyback's release
# list, piggyback's asset list, and GitHub's listing), so the fix is to stop
# trusting any of them for control flow: try to create, treat "already exists"
# as success, and confirm the release afterwards by fetching it BY TAG, which
# is a different endpoint from the list and was consistent throughout.
for (tg in unique(vapply(todo, function(a) a$tag, character(1)))) {
  if (tg %in% existing) next
  cat(sprintf("creating release %s ... ", tg))
  res <- tryCatch({
    gh::gh("POST /repos/{owner}/{repo}/releases", owner = .owner, repo = .name,
           tag_name = tg, name = tg,
           body = paste0("citiusdata artifacts for `", tg,
                         "`. Published via scripts/publish_release.R."))
    "created"
  }, error = function(e)
    if (grepl("already_exists|already exists", conditionMessage(e))) "already exists"
    else stop(e))
  cat(res, "\n")
  existing <- c(existing, tg)
}
# Confirm every needed release resolves by tag before uploading to it.
for (tg in unique(vapply(todo, function(a) a$tag, character(1)))) {
  ok <- !is.null(tryCatch(gh::gh("/repos/{owner}/{repo}/releases/tags/{tag}",
                                 owner = .owner, repo = .name, tag = tg),
                          error = function(e) NULL))
  if (!ok) stop("release ", tg, " does not resolve by tag; refusing to upload")
}
if (length(todo)) Sys.sleep(2)

# PASS 2 -- upload.
for (a in todo) {
  p <- file.path(D, a$file)
  mb <- file.size(p) / 1048576
  cat(sprintf("uploading %-36s %7.1f MB -> %s ... ", a$file, mb, a$tag))
  piggyback::pb_upload(p, repo = REPO, tag = a$tag, overwrite = TRUE)

  # VERIFY WITH AN INDEPENDENT SOURCE. torp re-checks after upload because a
  # success code does not prove the asset is retrievable -- but the first
  # version of this check used pb_list(), which piggyback memoises exactly like
  # pb_releases(). It reported "FAILED - asset not listed" for two uploads that
  # had in fact succeeded: a false negative produced by verifying with the same
  # cached library that caused the original bug. A check has to read something
  # the thing being checked cannot have poisoned.
  Sys.sleep(2)
  rel <- tryCatch(gh::gh("/repos/{owner}/{repo}/releases/tags/{tag}",
                         owner = .owner, repo = .name, tag = a$tag),
                  error = function(e) NULL)
  hit <- if (is.null(rel)) NULL else
    Filter(function(x) identical(x$name, basename(a$file)), rel$assets)
  if (!length(hit)) {
    cat("FAILED - asset not on the release\n")
  } else {
    sz <- hit[[1]]$size
    cat(sprintf("%s (remote %.1f MB, state %s)\n",
                if (abs(sz - file.size(p)) < 1024) "ok" else "SIZE MISMATCH",
                sz / 1048576, hit[[1]]$state))
  }
}

cat("\nfinal state (read from the API, not piggyback's cache):\n")
rr <- tryCatch(gh::gh("/repos/{owner}/{repo}/releases", owner = .owner, repo = .name,
                      .limit = Inf), error = function(e) list())
for (x in rr) {
  n <- length(x$assets)
  mb <- if (n) sum(vapply(x$assets, function(a) a$size, numeric(1))) / 1048576 else 0
  cat(sprintf("  %-18s %d asset(s), %.1f MB\n", x$tag_name, n, mb))
}
