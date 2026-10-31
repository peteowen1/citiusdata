# Upload a file to a GitHub release without ever deleting the old asset first.
#
# piggyback::pb_upload(overwrite = TRUE) DELETES the existing asset and then
# uploads. If the upload then fails, the release is left without the file. On
# 2026-09-17 that emptied wheather's cache release for three weeks, and here the
# asset at stake is the 353 MB corpus, whose only other copy is the laptop's
# (vault/plans/CLOBBER-AUDIT-2026-10-08.md, MED row). piggyback 0.1.5 also only
# warns when the upload POST fails, so "pb_upload() returned" has never meant
# "the file is on the release".
#
# The scheme is torp's safe_release_upload() (torp/R/release_upload.R), cut down
# to what a script needs, with the same temp-name prefix so a stranded copy reads
# the same way in every verse:
#   1. upload under a fresh name (nothing to delete, so nothing can be lost);
#   2. wait until GitHub's own record says that name is `uploaded` at the local
#      byte size;
#   3. only then delete the old asset (and temp copies stranded by earlier runs);
#   4. rename the new asset into place.
# A failure before step 3 leaves the old asset untouched. A failure of step 4
# leaves the data on the release under its temp name, and the error says so.
#
# Every read bypasses gh's HTTP cache, which otherwise served a pre-upload
# listing for up to 60 s (torp#271).

SAFE_UPLOAD_TMP_PREFIX <- "vbnew-"

.su_gh <- function(...) {
  op <- options(gh_cache = FALSE)
  on.exit(options(op), add = TRUE)
  gh::gh(...)
}

.su_assets <- function(owner, name, tag) {
  rel <- .su_gh("GET /repos/{owner}/{repo}/releases/tags/{tag}",
                owner = owner, repo = name, tag = tag)
  a <- if (is.null(rel$assets)) list() else rel$assets
  data.frame(
    id    = vapply(a, function(x) as.numeric(x$id), numeric(1)),
    name  = vapply(a, function(x) x$name, character(1)),
    size  = vapply(a, function(x) as.numeric(x$size), numeric(1)),
    state = vapply(a, function(x) if (is.null(x$state)) NA_character_ else x$state, character(1)),
    stringsAsFactors = FALSE
  )
}

safe_release_upload <- function(path, repo, tag, name = basename(path),
                                poll_delays = c(2, 3, 5, 10, 15, 30)) {
  size <- as.numeric(file.size(path))
  if (is.na(size)) stop("safe_release_upload: ", path, " does not exist", call. = FALSE)
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  owner <- parts[1]; rname <- parts[2]
  # Unique per call from the pid and a microsecond clock, not sample.int(),
  # which would move .Random.seed for a seeded caller.
  tmp <- sprintf("%s%s-%s-%d%s--%s", SAFE_UPLOAD_TMP_PREFIX,
                 Sys.getenv("GITHUB_RUN_ID", "local"), Sys.getenv("GITHUB_RUN_ATTEMPT", "0"),
                 Sys.getpid(), sprintf("%.0f", as.numeric(Sys.time()) * 1e6), name)

  # 1. A fresh name.
  tryCatch(
    piggyback::pb_upload(path, repo = repo, tag = tag, name = tmp,
                         overwrite = FALSE, show_progress = FALSE),
    error = function(e) stop("upload of ", name, " (as ", tmp, ") failed; the existing asset is untouched: ",
                             conditionMessage(e), call. = FALSE))

  # 2. GitHub's record decides whether it landed.
  assets <- NULL
  new_id <- NA_real_
  last_err <- "none"
  for (d in c(0, poll_delays)) {
    if (d > 0) Sys.sleep(d)
    assets <- tryCatch(.su_assets(owner, rname, tag), error = function(e) {
      last_err <<- conditionMessage(e)
      NULL
    })
    if (is.null(assets)) next
    hit <- assets[assets$name == tmp & assets$state %in% "uploaded" & assets$size == size, , drop = FALSE]
    if (nrow(hit) > 0L) { new_id <- hit$id[1L]; break }
  }
  if (is.na(new_id)) {
    stop(tmp, " never reported as uploaded at ", format(size, scientific = FALSE),
         " bytes (last read error: ", last_err, "); the existing ", name, " is untouched", call. = FALSE)
  }

  # 3. Only now remove the old asset and stranded temp copies of it. A failed
  #    delete of the real name stops here, because the rename would collide.
  #    Concurrent publishes of the same asset are unsupported: this deletes
  #    every stranded temp copy of `name`.
  stale <- startsWith(assets$name, SAFE_UPLOAD_TMP_PREFIX) & endsWith(assets$name, paste0("--", name)) &
    assets$name != tmp
  old <- assets[assets$name == name | stale, , drop = FALSE]
  for (i in seq_len(nrow(old))) {
    err <- tryCatch({
      .su_gh("DELETE /repos/{owner}/{repo}/releases/assets/{id}",
             owner = owner, repo = rname, id = format(old$id[i], scientific = FALSE))
      NULL
    }, error = function(e) e)
    if (!is.null(err)) {
      if (identical(old$name[i], name)) {
        stop("could not delete the old ", name, " (", conditionMessage(err),
             "); the new copy stays on the release as ", tmp, call. = FALSE)
      }
      warning("could not delete stale temp asset ", old$name[i], ": ", conditionMessage(err), call. = FALSE)
    }
  }

  # 4. Rename into place, with retries: the old asset is gone, so this is the
  #    one step whose failure leaves the release without `name`.
  ok <- FALSE
  for (w in c(0, 5, 10)) {
    if (w > 0) Sys.sleep(w)
    ok <- !inherits(tryCatch(
      .su_gh("PATCH /repos/{owner}/{repo}/releases/assets/{id}",
             owner = owner, repo = rname, id = format(new_id, scientific = FALSE), name = name),
      error = function(e) e), "error")
    if (ok) break
  }
  if (!ok) {
    stop("rename of ", tmp, " to ", name, " failed after the old asset was deleted; the data is on the release as ",
         tmp, ". Recover by renaming it: gh api -X PATCH repos/", repo, "/releases/assets/",
         format(new_id, scientific = FALSE), " -f name=", name, call. = FALSE)
  }
  invisible(new_id)
}
