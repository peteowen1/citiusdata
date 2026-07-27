# citiusdata

Cached competition result histories and scrapers for [`citius`](../citius).

Follows the ecosystem release-as-data-bus pattern: processed parquet is
published to GitHub Releases via `piggyback` and read back by `citius` loaders
with local caching. Raw data stays out of git.

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
