# scripts/ — what ships, what measures, what is dead

This directory mixes the shipping path with one-off experiment arms, and that
mix has already cost real money: `score_meet.R` (2026-08-12) was written by
copying a pre-audit block out of `score_glasgow2026.R` and inherited three
stale-config faults verbatim. There is no test suite over these scripts, so
classification is the guard rail.

## The one rule

**A script that ships a number sources `_deployed.R` and takes every model
input from `DEPLOYED`.** If you are writing or editing a script and typing a
filename like `calibration*.rds`, a `half_life =` literal, or a store path,
stop — either it belongs in `_deployed.R`, or the script is an experiment and
should say so in its header. The fastest audit is:

```
grep -L "_deployed" scripts/predict_*.R scripts/score_*.R scripts/export_*.R
```

## Shipping path (sources `_deployed.R`, output is published or quoted)

| script | role |
|---|---|
| `_deployed.R` | THE deployed configuration; every accessor lives here |
| `run_meet.ps1` | build + publish a meet card (4.3 min, aborts on first failure) |
| `score_meet.R` | score any calendar meet the morning after |
| `predict_birmingham2026.R`, `predict_glasgow_entries.R`, `predict_glasgow_live.R` | meet cards |
| `score_glasgow2026.R`, `score_pretournament.R` | Games scorers |
| `export_blog_data.R`, `export_athletics_blog.R`, `refresh_blog.cmd` | blog export |
| `sanity_birmingham_card.R`, `sanity_glasgow_card.R` | card assertions, run before anyone reads a card |
| `audit_anchors.R` | anchor checks on the deployed model |

## Data pipeline (rebuilds inputs; not model-config-sensitive)

`build_athletics_corpus.R`, `build_stores.R`, `build_competition_catalogue.R`,
`build_crosswalk.R`, `rebaseline_chain.R` (writes the calibration chain),
`recalibrate_corpus.R` / `recalibrate.R` (fit + diff against previous),
`harvest_*.R` (feed harvesters; `harvest_partial_races.R` is resumable),
`watch_glasgow2026.R`, `merge_*.R`, `assemble_*.R`, `parse_birmingham_entries.R`,
`resolve_birmingham_athletes.R`, `build_glasgow_gapfill.R`.

## Measurement harness

`backtest_athletics.R` (the arm runner; config via `CITIUS_BT_*` env vars),
`score_arm.R` (six metrics, three populations, aborts on vintage mismatch),
`quick_compare.R`, `model_scoreboard.R`, `diagnose_marks.R`,
`diagnose_backtest.R`, `evaluate_prereg.R`, `audit_evidence.R`,
`audit_history.R`, `audit_coverage.R`, `audit_feed_coverage.R`,
`audit_data_integrity.R` (duplicates, schema/coverage completeness,
implausible-mark rate, systematic tier-vs-strength mislabeling, RDS-vs-
citius.duckdb cross-store consistency, known-athlete spot check — run
routinely, not just after an incident), `validate_*.R`,
`verify_season_arm_fired.R`, `harvest_status.R`.

## Experiment arms (one-off; NOT the deployed model; keep for provenance)

Every `build_calibration_*.R` builds one named arm's calibration — the arm
verdicts live in `MODEL-LOG.md` and `docs/reference/refuted-hypotheses.md`, not
here. `make_season_arm_calibrations.R`, `refit_scale.R`, `refit_stale_effects.R`,
`fit_half_life_*.R`, `evaluate_half_life_mae.R`, `measure_course_offset.R`,
`extract_corpus_findings.R`, `test_xgb_residual_v2.R`, and the `*.ps1` queue
runners (`overnight_*`, `queue_*`, `resume_run`, `grind_season_ab`,
`run_season_ab`) drive batches of them.

## Superseded (warn at runtime; kept because something cites them)

- `predict_glasgow2026.R` — superseded by `predict_glasgow_entries.R`; cited by
  METHODOLOGY.md as the only home of `momentum`.
- `backtest_championships.R` — superseded by `backtest_athletics.R`; read as
  documentation by `backtest_swimming.R`.
- `tune_half_life.R` — its 365 is now `DEPLOYED$half_life`.
- `rebuild_athletics.R` — writes the pre-corpus `calibration.rds` vintage.

## Medal-table / dominance track (blog research, not the forecasting model)

`analyse_*.R`, `harvest_multisport_medal_tables.R`, `harvest_sport_*.R`,
`harvest_team_*.R`, `harvest_participating_nations.R`, `harvest_gdp_population.R`,
`merge_team_podiums.R`, `games_reference.R`.

## Swimming (parallel track, less built out)

`backtest_swimming.R`, `build_swimming_corpus.R`, `harvest_swim*.R`,
`harvest_swimming_*.R`, `assemble_swim*.R`, `predict_glasgow_swimming.R`.

## Form ratings (`seqv2`/`seqv3`) — live, and until 2026-08-30 undocumented here

Found during a 2026-08-30 data-directory survey: this pipeline is **65% of
`citiusdata/data/` by size** (the arm-tagged files below) and feeds the
shipping path directly, but had never been added to this file. It has two
tags, not one, and they mean different things:

- `SEQ_TAG` (default `"baseline"`) — `form_ratings.R` / `form_ratings_swimming.R`
  write `seqv2_state_$SEQ_TAG.parquet`, `seqv3_history_$SEQ_TAG.parquet`,
  `seqv3_majors_$SEQ_TAG.parquet`, `seqv3_meta_$SEQ_TAG.json`. Run once per
  arm being evaluated — this is the experiment-arm layer, ~97
  `check_*`/`score_*`/`build_*`/`optimise_*` scripts read these by tag.
- `FORM_TAG` (default `"final"`) — `form_display_marks.R` /
  `form_display_marks_swimming.R` READ `seqv2_state_$FORM_TAG.parquet` /
  `seqv3_history_$FORM_TAG.parquet` (i.e. someone must have run `form_ratings.R`
  with `SEQ_TAG=final` first) and WRITE `form_display_$FORM_TAG.parquet` /
  `form_display_$FORM_TAG_calib.json`. **`export_athletics_blog.R` hard-requires
  `form_display_final_calib.json` and aborts without it** — this is the
  shipping-path end of the chain, belongs in "Shipping path" above by
  function, listed here instead so the whole chain reads as one story.

Shipping chain: `form_ratings.R` (`SEQ_TAG=final`) → `seqv2_state_final.parquet`
+ `seqv3_history_final.parquet` + `seqv3_majors_final.parquet` +
`seqv3_meta_final.json` → `form_display_marks.R` (`FORM_TAG=final`) →
`form_display_final.parquet` + `form_display_final_calib.json` →
`export_athletics_blog.R`.

## Data directory hygiene (added 2026-08-30, after a week that needed it)

`citiusdata/data/` has no retention policy beyond `.gitignore`'s `data/*` (it
is not git-tracked at all, except three hand-maintained CSVs — `git ls-files
data/` to confirm which). A 2026-08-30 survey found 1,424 top-level entries,
22 GB, with five different naming idioms for "this is a backup" and zero
cleanup mechanism — directly implicated in how long that week's
`championship_results.rds` recovery took, since nothing on disk said which of
14+ backup variants was current. Going forward:

- **Backups get one pattern**: `<file>.bak-<YYYYMMDD>[-<reason>]`, and they
  live in `citiusdata/_archive/`, never in `data/` itself.
- **Experiment arms get one naming scheme**: `backtest_cache_$arm` (env-driven
  via `CITIUS_BT_CACHE`, already what the six queue-runner `.ps1` scripts
  write). The older `bt_*` prefix is retired — do not add new `bt_*`
  directories.
- **Retention is 90 days, deleted by a human, never by a script** — matching
  this repo's standing caution around destructive/irreversible actions.
- **New scripts should resolve data file paths through
  `citiusdata/scripts/_paths.R`'s `citius_data_path()`**, not a literal
  filename spliced onto `D`/`OUT`. Existing scripts are not being
  mass-migrated (see the DuckDB migration plan's own phased-adoption
  precedent) — but a literal `citiusdata/data/<filename>` string in a *new*
  script is exactly the pattern that made a future reorg cost 43-51 script
  edits per file this time; don't add to that bill.
