# publish_deployed_inputs.R
# Put the forecast chain's inputs somewhere a GitHub runner can reach them.
#
# WHY THIS EXISTS. The athletics forecast has only ever run on one laptop.
# `data/` is 8.2 GB and four small files of it are tracked in git, so a runner
# starts with nothing to run against: no corpus, no calibration, no
# championship results. That is the whole reason the chain is manual, and the
# reason four of seven meets on the 2026 calendar ran with no forecast — a card
# only exists if somebody remembers to build it on the right day.
#
# The other three verses solved this years ago with the same pattern: attach the
# data to a GitHub Release and let workflows download it. This is that, for
# citiusdata.
#
# THE FILE LIST IS NOT WRITTEN DOWN HERE. It is read from `_deployed.R`, which
# is already the single place that says which corpus, calibration and aging
# artefacts are live. A second hardcoded list would drift the first time
# something is promoted, and publishing the wrong vintage under a fresh
# timestamp is exactly the class of failure this verse's siblings spent
# September fixing.
#
#   Rscript scripts/publish_deployed_inputs.R --dry-run   # list, upload nothing
#   Rscript scripts/publish_deployed_inputs.R             # upload
#
# Needs `gh` authenticated. Uploads are --clobber: the tag holds one current
# vintage, never a history.

suppressMessages({
  library(cli)
})

args    <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

REPO <- "peteowen1/citiusdata"
TAG  <- "deployed-latest"

root <- tryCatch(here::here(), error = function(e) getwd())
# here() resolves to the verse root when run from the workspace, and to the
# repo when run from inside it. Accept both rather than assuming one.
data_dir <- if (dir.exists(file.path(root, "citiusdata", "data"))) {
  file.path(root, "citiusdata", "data")
} else {
  file.path(root, "data")
}
if (!dir.exists(data_dir)) stop("Could not locate citiusdata/data from ", root)

deployed_r <- file.path(dirname(data_dir), "scripts", "_deployed.R")
if (!file.exists(deployed_r)) stop("Missing ", deployed_r)
source(deployed_r)

# The chain's inputs, in two groups.
#
#   from DEPLOYED — whatever is currently promoted, so this follows a promotion
#     automatically instead of needing an edit here.
#   fixed — inputs the chain reads that are not part of the deployed-model
#     manifest: the championship results the athlete resolver falls back to when
#     no database is reachable (a runner never has one), and the catalogue the
#     history rescue-rebuild needs.
from_deployed <- c(DEPLOYED$history_rds, DEPLOYED$calibration, DEPLOYED$aging)
fixed <- c(
  "championship_results.rds",
  "competition_catalogue.parquet",
  # deployed_history() has no athletics_corpus_store on a runner, so it always
  # takes the rescue-rebuild path, which needs the catalogue above.
  #
  # And the export step hard-stops without this one. It is written by
  # form_display_marks.R, which is NOT part of the meet chain — a separate,
  # occasionally-run calibration step — so a runner can never produce it and
  # must be handed it. Without it the chain runs all five earlier steps and
  # then aborts at the last, which is the partial run this whole design exists
  # to avoid, reached through a gap in the input list instead of a failure.
  "form_display_final_calib.json"
)

files <- unique(c(from_deployed, fixed))
paths <- file.path(data_dir, files)

missing <- files[!file.exists(paths)]
if (length(missing)) {
  # Refuse rather than publish a partial set. A runner that downloads four of
  # five inputs fails deep inside the chain with a confusing error; failing
  # here names the file.
  cli_alert_danger("Missing input(s), nothing published:")
  for (m in missing) cli_alert("  {m}")
  quit(save = "no", status = 1)
}

sizes <- file.info(paths)$size
cli_h1("Deployed inputs for {REPO} @ {TAG}")
for (i in seq_along(files)) {
  cli_alert_info("{files[i]}  {prettyunits::pretty_bytes(sizes[i])}")
}
cli_alert_info("total {prettyunits::pretty_bytes(sum(sizes))}")

if (dry_run) {
  cli_alert_warning("--dry-run: nothing uploaded")
  quit(save = "no", status = 0)
}

# Create the release if it is not there yet. `gh release view` exits non-zero
# when absent, which is the check.
exists_rc <- system2("gh", c("release", "view", TAG, "--repo", REPO),
                     stdout = FALSE, stderr = FALSE)
if (exists_rc != 0) {
  cli_alert_info("Creating release {TAG}")
  rc <- system2("gh", c("release", "create", TAG,
                        "--repo", REPO,
                        "--title", shQuote("Deployed forecast inputs"),
                        "--notes", shQuote(paste(
                          "Inputs the athletics forecast chain reads, published so CI can run it.",
                          "Written by scripts/publish_deployed_inputs.R; the file list comes from _deployed.R."))))
  if (rc != 0) stop("Could not create release ", TAG)
}

ok <- TRUE
for (i in seq_along(files)) {
  cli_alert_info("Uploading {files[i]} ...")
  rc <- system2("gh", c("release", "upload", TAG, shQuote(paths[i]),
                        "--repo", REPO, "--clobber"))
  if (rc == 0) {
    cli_alert_success("uploaded {files[i]}")
  } else {
    cli_alert_danger("FAILED {files[i]}")
    ok <- FALSE
  }
}

# A manifest, so the consumer does not keep its own copy of this list.
#
# The calibration artefact is renamed on almost every promotion
# (calibration_corpus_wac_coast_0904 -> ..._full2 -> next), so a filename typed
# into the workflow would drift the first time one is promoted, and the check
# that is supposed to catch a missing input would be checking for a file nobody
# publishes any more. The workflow reads this instead and fetches exactly what
# is named here.
manifest_path <- file.path(tempdir(), "deployed_inputs.json")
writeLines(jsonlite::toJSON(list(
  published_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  files = files
), auto_unbox = TRUE, pretty = TRUE), manifest_path)
if (system2("gh", c("release", "upload", TAG, shQuote(manifest_path),
                    "--repo", REPO, "--clobber")) != 0) {
  cli_alert_danger("FAILED deployed_inputs.json — the consumer cannot tell what to fetch")
  ok <- FALSE
} else {
  cli_alert_success("uploaded deployed_inputs.json")
}

# Drop assets that are no longer part of the set. --clobber only replaces a
# file of the SAME name, so every superseded calibration vintage would sit on
# the tag for ever and the consumer's `gh release download` would pull all of
# them, every run.
current <- c(files, "deployed_inputs.json")
listed <- suppressWarnings(system2("gh", c("release", "view", TAG, "--repo", REPO,
                                           "--json", "assets", "-q", ".assets[].name"),
                                   stdout = TRUE))
stale <- setdiff(listed[nzchar(listed)], current)
for (s in stale) {
  cli_alert_info("Removing superseded asset {s}")
  if (system2("gh", c("release", "delete-asset", TAG, shQuote(s),
                      "--repo", REPO, "--yes")) != 0) {
    cli_alert_warning("could not remove {s} — harmless, but it will keep being downloaded")
  }
}

if (!ok) {
  cli_alert_danger("At least one upload failed — the tag is now a MIXED vintage.")
  quit(save = "no", status = 1)
}
cli_alert_success("All {length(files)} input(s) published to {TAG}")
