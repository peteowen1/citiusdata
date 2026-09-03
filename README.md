# citiusdata

Cached competition result histories and scrapers for [`citius`](https://github.com/peteowen1/citius).

Follows the ecosystem release-as-data-bus pattern: processed parquet is
published to GitHub Releases via `piggyback`. Raw data stays out of git.

**Status, corrected 2026-09-03.** This paragraph described the intended design
as though it were built. It was not: citiusdata had **zero releases** until
2026-09-03, so every artifact existed only on the machine that produced it, and
the sentence "read back by `citius` loaders" was never true — nothing in
`citius/R` downloads from a release. Half of it is now real:

| | state |
|---|---|
| write side | **done** — `scripts/publish_release.R`, one release per dataset |
| read side | **not built** — `citius` loaders read local files only |

So the releases are backup and distribution today, not a live dependency.

Releases follow torpdata's convention (`<dataset>-data`), because `torp` is the
only package in the ecosystem with a working read side and its loader builds
`releases/download/{tag}/{file}` — matching it is what makes copying that loader
straightforward. Note the three data repos are NOT consistent with each other:
torpdata suffixes `-data`, pannadata and bouncerdata mostly do not.

Building the read side needs a partitioning decision first: `torp` splits assets
by season so one round does not pull 484 MB, whereas `athletics_corpus.parquet`
is a single 320 MB file.

## Why this repo exists

The athletics feed is one-athlete-per-request. Pulling a full entry list live
during a Games is too slow and too rude to the upstream host, so histories are
harvested ahead of time, cached here, and refreshed on a schedule.

## Layout

| Path | Contents |
|------|----------|
| `scripts/` | Harvest and export scripts |
| `data/` | Local cache (gitignored) |
| `blog/` | Pre-built parquet for R2 upload to ITG |

## Status

Scaffolded. Nothing harvested yet.
