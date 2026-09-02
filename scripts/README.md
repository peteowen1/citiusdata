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

## Layout

`diagnostics/` holds the 150 one-off `check_*.R`/`score_*.R` (the diagnostic
kind, not the 3 shipping `score_*.R` below)/`probe_*.R` investigation
scripts — moved there 2026-09-02 purely for browsability (341 top-level `.R`
files was unmanageable). Every one of them resolves `citiusdata/data` via
either a hardcoded absolute path or `here::here("citiusdata", "data")`,
neither of which depends on the script's own location, so the move changed
nothing about how they run. Everything else in this file lives at the top
level of `scripts/`.

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
  always write `seqv2_state_$SEQ_TAG.parquet` and `seqv3_majors_$SEQ_TAG.parquet`,
  but `seqv3_history_$SEQ_TAG.parquet` and `seqv3_meta_$SEQ_TAG.json` are
  ADDITIONALLY gated behind `SEQ_HIST=1` (default off) — found 2026-09-02 after
  it silently produced an incomplete quartet and broke `check_dominance.R`/
  `check_merged_sections.R` with an opaque Arrow IOError rather than a clear
  "set SEQ_HIST" message. Run once per arm being evaluated — this is the
  experiment-arm layer, ~97 `check_*`/`score_*`/`build_*`/`optimise_*` scripts
  read these by tag (now in `scripts/diagnostics/`, see "Layout" above).
- `FORM_TAG` (default `"final"`) — `form_display_marks.R` /
  `form_display_marks_swimming.R` READ `seqv2_state_$FORM_TAG.parquet` /
  `seqv3_history_$FORM_TAG.parquet` (i.e. someone must have run `form_ratings.R`
  with `SEQ_TAG=final` first) and WRITE `form_display_$FORM_TAG.parquet` /
  `form_display_$FORM_TAG_calib.json`. **`export_athletics_blog.R` hard-requires
  `form_display_final_calib.json` and aborts without it** — this is the
  shipping-path end of the chain, belongs in "Shipping path" above by
  function, listed here instead so the whole chain reads as one story.

Shipping chain: `form_ratings.R` (`SEQ_TAG=final SEQ_HIST=1`) →
`seqv2_state_final.parquet` + `seqv3_history_final.parquet` +
`seqv3_majors_final.parquet` + `seqv3_meta_final.json` → `form_display_marks.R`
(`FORM_TAG=final`) → `form_display_final.parquet` +
`form_display_final_calib.json` → `export_athletics_blog.R`.

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

### Cleanup status, 2026-09-03

The `scripts/` reorg (150 diagnostic one-offs into `scripts/diagnostics/`,
see "Layout" above) is done. The remaining ~26 `.ps1`/`.cmd` runners at the
top level were surveyed but mostly left alone: several look like dead
one-off arm runners (their target `backtest_$arm.rds` files are already
archived), but their VALUE is being the reproduction path for that exact
file if it's ever needed again, and archiving the script costs nothing to
defer. Given the same-day discovery that an earlier archiving pass had
broken two live shipping scripts by misjudging a file as disposable (see
`data/README.md`'s "per-file reference grep" note), this pass erred
conservative rather than repeat that mistake for a low-value cleanup.

**RDS retirement (the "flip DuckDB to be the write-source" idea mentioned
in "`.rds` vs DuckDB vs parquet" below): not pursued, and shouldn't be
without a real reason.** `audit_data_integrity.R` reads `.rds` directly BY
DESIGN — it's the independent copy DuckDB gets checked against, so
migrating it to read DuckDB would defeat its own purpose. The other
candidates (`fit_family_pool_offsets.R`, `merge_t3_full_checkpoint.R`,
`merge_t3_pilot_2026.R`) read `.rds` because `.rds` is still the documented
write-source of truth — reading DuckDB instead would mean trusting a
derived copy over the source, which is backwards while that stays true.
Revisit only if `.rds` actually stops being the write-source.

## DuckDB store (`citius.duckdb`) — added 2026-08-30

`citius/R/duckdb_store.R` + `citius/R/db_schema.R` (in the `citius` package,
not here) give `championship_results` and `athletics_corpus` a transactional,
schema-guarded home: `store_championship_results()` / `store_athletics_corpus()`
to write, `load_championship_results()` / `load_athletics_corpus()` to read,
both wrapping `citius.duckdb` via `with_citius_db_connection()`. The schema
guard aborts on an unexpected extra column (would silently drop data) and
warns on a missing one; merge mode drops whole competitions already present
rather than row-level dedup, matching `merge_referenced.R`'s pre-existing
correct pattern. Built after a 2026-08-29 incident where a hand-rolled dedup
key silently collapsed 21,440 rows — see git history on both repos for the
full incident chain and the two follow-on bugs the store itself turned up
under real use (a schema-guard hole for a missing dedup key, and a
replace-mode NA-sentinel guard that broke `athletics_corpus` — both fixed
and tested).

**RDS is a kept compat export, not a legacy leftover.** Every migrated
script tries `citius.duckdb` first and falls back to the RDS file, warning,
if the DB is unavailable, stale, or a table is missing — same discipline
`build_stores.R` established first. Migration status as of 2026-08-30:

- **Write side**: `merge_referenced.R`, `build_athletics_corpus.R` write to
  `citius.duckdb` alongside their existing RDS write. `harvest_gap_20260818.R`
  deliberately not touched — a dated, already-executed one-off, not part of
  the recurring chain.
- **Shipping path**: all 14 scripts above either already went through
  `deployed_history()`/the Arrow parquet store (9 of them — no change
  needed) or were migrated directly (`predict_glasgow_entries.R`,
  `predict_glasgow_live.R`, `score_glasgow2026.R`, `export_blog_data.R`,
  `audit_anchors.R`).
- **Data pipeline + Measurement harness**: 16 scripts migrated —
  `build_competition_catalogue.R`, `build_crosswalk.R`, `rebaseline_chain.R`,
  `resolve_birmingham_athletes.R`, `score_arm.R`, `quick_compare.R`,
  `model_scoreboard.R`, `diagnose_marks.R`, `evaluate_prereg.R`,
  `audit_coverage.R`, `validate_world_records.R`, `harvest_athlete_histories.R`,
  `harvest_gap.R`, `harvest_missing_majors.R`, `harvest_partial_races.R`,
  `harvest_referenced.R`.
- **Deliberately still RDS-only**, not an oversight: `backtest_athletics.R`
  and `validate_data.R` take `Sys.getenv()`-configurable input filenames to
  compare frozen arm snapshots against each other — DuckDB's single
  current-state table can't represent that, and these scripts specifically
  want a frozen input. `audit_history.R` by design audits `flag_implausible()`
  across several raw files side by side, including `athletics_history.rds`/
  `swimming_history_full.rds`, which this migration doesn't touch. Every
  **Experiment arms** script — per this file's own existing convention that
  arm output is one-off/disposable — is untouched.

## World Athletics discovery functions — built 2026-08-30, NOT yet in the harvest workflow

`athletics_find_competition()` (the existing wrapper route) and
`harvest_missing_majors.R`'s discovery list are a **keyword search sweep** —
they can only find a competition someone already thought to search a name
for. Found live 2026-08-30: this missed two current Diamond League legs
(Xiamen, Shanghai — later confirmed both are 2027-dated, not actually a gap,
but the discovery method itself had no way to know that without a manual
check) and had real, measured gaps against the calendar `worldathletics.org`
itself serves.

Four new `citius` functions, all fetching data embedded in
`worldathletics.org`'s own server-rendered pages (`window.__NEXT_DATA__`),
confirmed fetchable via plain `httr2`, no browser needed:

- `athletics_calendar()` / `athletics_calendar_all()` — a real,
  date-range-queryable, paginated competition list (39,290 measured
  2026-08-30, spanning recorded history through scheduled future events).
- `athletics_athlete_official_profile()` — authoritative registered name/
  birthdate/country by `athlete_id`, sidestepping name-string guessing
  (the corpus stores "Armand Duplantis", not "Mondo Duplantis"; "Sydney
  Mclaughlin", not "McLaughlin" — a name-grep sanity check missed both
  before this existed).
- `athletics_calendar_results(competition_id)` — full competition results
  direct from `worldathletics.org`, bypassing the `worldathletics.nimarion.de`
  wrapper entirely. Used to recover 22 of 26 competitions the wrapper
  consistently 500'd on.
- `map_calendar_results_to_championship_schema()` — translates that output
  onto `CITIUS_DB_SCHEMA$championship_results`'s shape. **Not a drop-in
  replacement** — its own roxygen `@section` documents real, irreducible
  gaps (no `race_key` equivalent, no structured discipline/venue split, none
  of this package's derived columns, race-level dates frequently missing).

**None of these four are wired into any routine harvest script.** They were
used today for a one-off backfill (26 competitions the wrapper couldn't
serve; 22 recovered and merged into `championship_results.rds`) and proven
against real data, but `harvest_missing_majors.R`'s keyword sweep is still
the standing discovery method. Wiring them in as the default is a real,
separate follow-on decision, not something this work already did.

## Known permanent gaps (2026-08-30)

- **4 competitions are broken on World Athletics' own backend, not just our
  access to it**: 7189491, 7189494, 7204909, 7213160. Confirmed via a real,
  branded WA "Error 500" page in a browser, not a bot-block — retrying
  further will not fix this from citius's side.
- **T3_development (minor/local meets) has a ~20,000-competition gap**
  against its own catalogue population (28,269 total, only 8,263 confirmed
  present as of 2026-08-30). A bounded 2026-only pilot (1,262 competitions,
  ≥20 results) ran at ~7.5 competitions/minute with a ~0.4% failure rate —
  the full gap would take roughly 44 hours of cumulative fetching, in
  chunks (nothing here survives longer than about an hour unattended). Not
  started; a real time-cost decision, not a blocker.
