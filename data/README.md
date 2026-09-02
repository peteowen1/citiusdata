# citiusdata/data — what's here

`data/` is gitignored except three hand-maintained reference CSVs
(`world_records.csv`, `athletics_calendar.csv`, `diamond_league_2026.csv`).
Everything else is either a canonical table, a derived store, a live
harvest cache, or disposable experiment output — moving or deleting
anything else here never touches git history.

## Canonical tables (the ones that matter)

| File | What it is | Built by |
|---|---|---|
| `championship_results.rds` | one row per athlete per race, ~4.5M rows | harvest + merge pipeline |
| `competition_catalogue.parquet` | one row per competition, ~32k rows (tier, class, measured strength) | `build_competition_catalogue.R`, then `augment_catalogue_coverage.R` |
| `athletics_corpus.rds` / `.parquet` | the ability-estimation corpus | `build_athletics_corpus.R` |
| `athletics_history.rds` | career-route history sweep | harvest pipeline |
| `calibration_corpus_csigma_coast.rds` | **the deployed calibration** — read via `_deployed.R`, never rename/move this one | `recalibrate_corpus.R` |
| `family_pool_offsets.rds`, `elite_cohort.rds`, `aging.rds` | model input tables | see `scripts/README.md` "Shipping path" |
| `citius.duckdb` | DuckDB mirror of the three RDS tables above (championship_results/athletics_corpus/athletics_history) | `bootstrap_citius_duckdb.R` |
| `athlete_meta.parquet`, `athlete_pbs.parquet`, `athlete_sbs.parquet`, `athlete_honours.parquet`, `athlete_wa_rankings*.parquet`, `athlete_crosswalk_*.parquet` | reference/lookup tables, each scoped to one purpose (bio, personal bests, season bests, honours, rankings, cross-source identity linking) — none of these is a single "master" athlete table | harvest_* scripts |

For column-level detail (types, NA rates, known issues, what's expected vs
wrong) see `docs/reference/data-audit-2026-09-02.md` in the parent
citiusverse repo — that document is the data dictionary, this file is just
"where things live."

## `.rds` vs DuckDB vs parquet — which is authoritative

`.rds` is currently the WRITE-source for the three big tables
(championship_results/athletics_corpus/athletics_history) — fixes go there
first, then get bootstrapped into `citius.duckdb` via
`bootstrap_citius_duckdb.R` (mode="replace"), which `build_stores.R` then
rebuilds the partitioned parquet stores from. If you fix something in the
`.rds`, that sequence has to run before the fix is visible anywhere else.
A full retirement roadmap (which scripts still need migrating off direct
`readRDS()`, and the recommendation to eventually flip DuckDB to be the
write-source with `.rds` becoming a read-only export) exists — see
NEXT-STEPS.

## Derived stores — safe to delete and rebuild

`athletics_store/`, `athletics_corpus_store/`, `athletics_careers_store/`,
`swimming_*_store/` are partitioned-parquet rebuilds of the tables above,
written by `build_stores.R`. Not source data. If one looks wrong or stale,
just re-run `build_stores.R` rather than debug the store itself.

## Live harvest caches — do not delete

`ath_athlete_cache/`, `ath_comp_cache*/`, `ath_profile_cache/`,
`wiki_cache*/`, `swim_*cache*/`, `se_rankings_cache/` cache scraped results
from upstream hosts. Each is read/written by a specific `harvest_*.R`
script — see `scripts/README.md` for which. Slow and rate-limited to
rebuild (real network calls against upstream hosts) — treat as expensive,
not disposable, unlike the backtest caches below.

## Experiment output — disposable, archived on a schedule

`backtest_cache_*/`, loose `backtest_*.rds`, and their matching
`calibration_corpus_<arm>.rds` are per-arm outputs of `backtest_athletics.R`
(`CITIUS_BT_CACHE=<arm>`). Reproducible by re-running the backtest.
Anything older than ~7 days with no live script reference gets moved to
`_archive/<date>-<reason>/` on a periodic sweep, then deleted by a human
after 90 days there — never auto-deleted. See the dated subfolder READMEs
under `_archive/` for what's currently archived and why. Four sweeps done so
far: `2026-08-30-data-cleanup/`, `2026-09-02-backtest-cache-sweep/`,
`2026-09-02-seqv-sweep-grid/` (683 files, 13.2GB — `form_ratings.R`'s
per-arm `seqv2_state`/`seqv3_history`/`seqv3_majors` knob-sweep outputs, a
family the second sweep missed because the names don't start `backtest_*`),
`2026-09-02-timestamped-prediction-snapshots/` (35 files — one-off
`<meet>_pretournament_<TIMESTAMP>.parquet` run snapshots nothing reads back;
the live path is the matching `.rds`).

**This mess regrows fast** — the second sweep found the clutter had
already regrown substantially in the 3 days since the first. There's no
periodic enforcement yet, only manual sweeps; if this file's `backtest_cache_*`
count looks large again, it's time for another one, not evidence the
convention failed.

## Naming conventions

- Backups: `<file>.bak-<YYYYMMDD>[-<reason>]`, never left loose — move to
  `_archive/` once the investigation that needed the backup is closed.
- Experiment arms: `backtest_cache_$arm` / `backtest_$arm.rds` /
  `calibration_corpus_$arm.rds`, one name per arm, matching the
  `CITIUS_BT_CACHE` value used to produce it. The `bt_*` prefix is
  retired — don't add to it (all 34 were confirmed dead and deleted
  2026-09-02, not archived, since the repo's own README had already
  declared the prefix retired weeks earlier).
