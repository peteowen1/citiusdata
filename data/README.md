# citiusdata/data — what's here

`data/` is gitignored except three hand-maintained reference CSVs
(`world_records.csv`, `athletics_calendar.csv`, `diamond_league_2026.csv`).
Everything else is either a canonical table, a derived store, a live
harvest cache, or disposable experiment output — moving or deleting
anything else here never touches git history.

## Canonical tables (the ones that matter)

| File | What it is | Built by | Dictionary |
|---|---|---|---|
| `championship_results.rds` | one row per athlete per race, ~4.5M rows | harvest + merge pipeline | [data-dictionary-championship-results.md](../../docs/reference/data-dictionary-championship-results.md) |
| `competition_catalogue.parquet` | one row per competition, ~32k rows (tier, class, measured strength) — this **is** the meet registry. Browsable at https://claude.ai/code/artifact/73f3a002-2f5d-4cec-a248-364724a9f213 (filterable by tier/class/year/strength; re-export + republish via `scripts/export_meet_registry_artifact.R` + `scripts/meet_registry_artifact_template.html` when the catalogue changes meaningfully) | `build_competition_catalogue.R`, then `augment_catalogue_coverage.R` | [data-dictionary-competition-catalogue.md](../../docs/reference/data-dictionary-competition-catalogue.md) |
| `athletics_corpus.rds` / `.parquet` | the ability-estimation corpus | `build_athletics_corpus.R` | [data-dictionary-athletics-corpus.md](../../docs/reference/data-dictionary-athletics-corpus.md) |
| `athletics_history.rds` | career-route history sweep | harvest pipeline | same shape as the competition-route columns in the corpus dictionary above |
| `calibration_corpus_csigma_coast.rds` | **the deployed calibration** — read via `_deployed.R`, never rename/move this one | `recalibrate_corpus.R` | [data-dictionary-model-artifacts.md](../../docs/reference/data-dictionary-model-artifacts.md) |
| `family_pool_offsets.rds`, `elite_cohort.rds`, `aging.rds` | model input tables | see `scripts/README.md` "Shipping path" | same, `data-dictionary-model-artifacts.md` |
| `citius.duckdb` | DuckDB mirror of the three RDS tables above (championship_results/athletics_corpus/athletics_history) | `bootstrap_citius_duckdb.R` | same schema as the `.rds` dictionaries |
| `athlete_meta.parquet`, `athlete_pbs.parquet`, `athlete_sbs.parquet`, `athlete_honours.parquet`, `athlete_wa_rankings*.parquet`, `athlete_crosswalk_*.parquet` | reference/lookup tables, each scoped to one purpose (bio, personal bests, season bests, honours, rankings, cross-source identity linking) — none of these is a single "master" athlete table | harvest_* scripts | [athlete-tables-audit-2026-09-03.md](../../docs/reference/athlete-tables-audit-2026-09-03.md) |

This file is the **registry** — every table, one line, where it lives. For
column-level detail (types, meanings, known gaps) follow the Dictionary
links above. `docs/reference/data-audit-2026-09-02.md` is the bug/fix
history on this pipeline, not a dictionary. `citiusverse/DICTIONARY.md` (the
repo root, one level up again) is a *vocabulary* glossary — meet/event/
phase/race terminology — a different thing from the column dictionaries
linked above; don't conflate the two.

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
under `_archive/` for what's currently archived and why. Seven sweeps done so
far: `2026-08-30-data-cleanup/`, `2026-09-02-backtest-cache-sweep/`,
`2026-09-02-seqv-sweep-grid/` (761 files, 13.2GB — `form_ratings.R`'s
per-arm `seqv2_state`/`seqv3_history`/`seqv3_majors`/`seqv3_meta`
knob-sweep outputs, a family the second sweep missed because the names
don't start `backtest_*`), `2026-09-02-timestamped-prediction-snapshots/`
(35 files — one-off `<meet>_pretournament_<TIMESTAMP>.parquet` run
snapshots nothing reads back; the live path is the matching `.rds`),
`2026-09-02-root-run-logs/` + `2026-09-02-data-run-logs/` (196 files, ~2.5MB
— stray stdout/stderr redirects, one at `citiusdata/` root, one here),
`2026-09-02-dead-calibration-arms/` (39 files, 334MB — old
`calibration_corpus_*` arm variants confirmed dead by a stronger
zero-any-reference grep, not just zero-read), `2026-09-02-diagnostic-outputs/`
(one-off `check_*.R`/`score_*.R` output files, write-only confirmed per file).
**Every sweep's README documents which `TAG`/`MEET`-variable-based files it
deliberately did NOT touch** — read one before assuming a zero-grep-hit file
is safe to archive; several genuinely-live files (e.g. `seqv3_majors_baseline.parquet`,
`brussels2026_pretournament.rds`) have zero literal-string references because
they're built from a runtime variable, not a proven-dead filename.

**A per-file reference grep is only as good as the file list it's run
against** — the 2026-09-02 backtest-cache sweep's own check missed
`backtest_combined_full.rds` and `backtest_ctrl_now.rds` being read by name
in two shipping export scripts and 10+ diagnostics respectively, found and
restored 2026-09-03 (see that sweep's own README for the correction). When
sweeping a whole family (`backtest_*.rds`, `seqv*_$TAG.*`), grep for every
bare filename actually present on disk, not a sample of the ones that look
disposable.

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
