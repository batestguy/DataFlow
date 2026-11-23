# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Status

Package `DataFlow` (MIT), public at github.com/batestguy/DataFlow; CI (R CMD check, 5 platforms) runs on every push to `main`. Milestones 0 (scaffold), 1 (lab design), 2 (randomised trial: `flow_assign`, `flow_exclude`), 3 (survey: summary rows, `flow_cluster`, `flow_select`) and 4 (several starting streams, `flow_pool`, counts from data, `flow_edit`) are done and approved by the user. Milestone 5 (machine-learning designs including time series: `flow_partition`, outcome events, fold grids with `flow_folds`) is in progress on branch `m5`. Principle: DataFlow documents splits the user made; it never splits data at random or fits models. The plan was revised on 2026-11-22 (milestones 4 to 9 plus a later time-based track). Phases, locked decisions, status, known limitations and the progress log are in `ROADMAP.md`; verbs are prefixed `flow_`. Session handover notes (transient, git-ignored) are in `CLAUDE.local.md`.

## Pinned environment

`renv` (1.3.1), explicit snapshot mode (dependencies come from DESCRIPTION). The project library lives in renv's cache, not in the folder; `.Rprofile` activates it. CI turns it off with `RENV_ACTIVATE_PROJECT: false` and installs from DESCRIPTION instead. After adding a dependency: `Rscript -e "renv::snapshot()"`.

## Design inputs

- `StudyDesign R package.txt` — earlier chat setting out the package idea (working name `flowdeskR`), phases, and time estimates. Treat it as advice, not a spec; its claims about third-party packages (e.g. `ggbracket`) are unverified.
- `RDT_20261115_1640158188107800537506259.jpg` — hand-drawn target diagram. It is ~6 MB: keep it out of git (`.gitignore` it, and keep it out of `inst/`); use a downscaled copy if a vignette needs it.

## What the package is for

An R package that builds and renders hierarchical study / experimental-design flow diagrams (publication-quality, via ggplot2) from a small pipe-friendly API.

Reference case (from the photo) — the diagram the MVP must reproduce:

| Level | Left branch | Right branch |
|---|---|---|
| Root | 100 cultures | |
| Experiment ID | Ex. 1 (50) | Ex. 2 (50) |
| Culture type | Type A (50) | Type B (50) |
| Batch | Batch 1 (25), Batch 2 (25) | Batch 1 (25), Batch 2 (25) |
| Cell culture composition | A, B, C under each batch | A, B, C under each batch |
| Condition | 9 conditions each (bracket spans all six A/B/C leaves) | same |
| Task | 2 tasks each (per condition) | same |
| Number of trials | 600 each | same |

Key layout features: row labels down the left margin; one-to-many splits with arrows; a horizontal bracket that merges many leaves back into a single node; counts in parentheses. The 600-trials figure is the user's own number — do not recompute or "correct" it without asking.

## Architecture (as built)

- `R/flow.R` — S3 class `dataflow`: `stages` (id, name, row_label, type, display, over, method), `nodes` (id, stage, label, n as text, note, anchor), `edges` (from, to, kind = arrow/bracket/pool), `side` (exclusion boxes: id, stage, parent, label, n, note), `reasons` (side, label, n), `leaves`; optional `data` (see `R/data.R`) and `edited` (counts changed by `flow_edit()`), present only when used, so typed flows are unchanged. `anchor` = parent for single-parent boxes, lowest common ancestor of the sources for brackets/pools; NA for top boxes (all on the first stage, several allowed) and for pools joining streams. The layout tree is built from it; `node_upstream()` follows the drawn lines instead (used to refuse a group holding a box and a box below it). `validate_dataflow()` checks structure only, never counts.
- `R/verbs.R` — `flow_start`, `flow_split`, `flow_apply` (`over =` draws brackets), `flow_outcome`. Shared engine `attach_per_group()`. Every verb takes `from =` (box ids or leaf labels) and `row_label =` (`""` hides it). `display = "summary"` (one box per parent, label from a `summary` template with `{k}`/`{stage}`; helper `summary_counts()`) works for the split engine `split_boxes()` and is refused for apply/outcome.
- `R/trial.R` — `flow_assign` (a split of type "assign" with `method`, shared engine `split_boxes()`) and `flow_exclude` (side boxes on their own "exclude" stage row, hanging off current end boxes only; never changes `leaves`).
- `R/survey.R` — `flow_cluster` (split of type "cluster", summary by default, `method`) and `flow_select` (one box per current box via `attach_per_group(method =)`; `where =` in data mode).
- `R/pool.R` — `flow_pool` (default / `over =` / `groups =`; `attach_per_group(groups =, join = "pool")`); at least two sources per pool, a box in at most one pool per call.
- `R/data.R` — data mode: `x$data` holds the data frames once (global row numbers via `offsets`), `count` column, and `rows$box` / `rows$left` (rows still in the flow after exclusions) / `rows$side` per box id. `by =` (engine `split_by()`), `where =` and formula `reasons` compute counts; apply/outcome/pool carry rows and compute a missing `n`; boxes made without data have no rows (counts below stay blank). `flow_data()`, `flow_from_data()`, `describe_data()`.
- `R/edit.R` — `flow_edit()` (labels, counts, events, notes, row labels; never rows or structure; for grids, block labels and counts via `grid_edit()`); `typed_counts()` lists counts and events not computed from data.
- `R/ml.R` — `flow_partition` (split of stage type "partition"; data mode: `by =` set column, `group =` checked so no group is in two sets, `time =` checked so sets are in time order, date ranges as notes; typed `group`/`time` described as not checked; recorded in `x$partition`). Outcome events: `x$events` (text per box id, only when used), from `flow_start(events = ~ ...)` in data mode (computed in `set_rows()`) or typed `events =` on start/split/partition/apply/outcome/pool; typed events in data mode listed in `x$events_typed`.
- `R/grid.R` — second diagram type, S3 class `dataflow_grid` built by `flow_folds()`: `type` kfold / blocked / expanding / rolling, `axis` folds or time, `blocks` (id, split, role Training/Validation/Test/Gap/Not used, start, end in fold or period units, n, label), rows behind blocks in `x$data$rows`, `times` (distinct time values in data mode). Windows follow rsample's rolling-origin vocabulary (`initial`, `assess`, `skip`, `gap`). `grid_edit()`, `grid_data()`.
- `R/grid_draw.R` — `grid_layout()` (pure, inches; block text picked from "Test\n(n = 18)", "Test\n18", "Test", "Te", none by measured width; legend when any block is abbreviated or blank; greyscale fills, dashed gaps; date ticks from `pretty()`), `autoplot.dataflow_grid()` / `plot()`, `describe_grid()` (called by `flow_describe()`).
- `R/layout.R` — `flow_layout()`: pure function, coordinates in inches. Box text with events via `box_text_events()` (only when events exist); `merge_segments()` draws each stretch of line once. Text measured on an off-screen `pdf(NULL)` device. Contour packing over the anchor tree; bracket/pool boxes centred under their sources. `group_gap` separates groups, `hgap` single boxes. Side boxes sit at parent x + `side_gap` and are reserved in the parent's contour; `crossed_rows()` decides where a line still runs down the parent's centre (past its exclusion boxes) before branching, for elbows, brackets and pools alike. Several top boxes are packed side by side with `group_gap`; pools that join streams are placed afterwards by `place_joined()` (centred under source centres) and drawn as converging lines.
- `R/render.R` — `autoplot.dataflow()` / `plot.dataflow()` / `theme_dataflow()` / `flow_save()` (natural size from the layout; also takes fold grids). Alt text from `flow_describe()` via `labs(alt =)`.
- `R/describe.R` — `flow_describe()`. Repeated box names are qualified by the parent's full name (`unique_names()`); cluster/select stages read "<stage>, selected by <method>: ... from <box>".
- `R/examples.R` — `flow_example("lab")`, `"trial"` (invented CONSORT 2025-style trial), `"survey"` (invented DHS-style household survey), `"review"` (invented PRISMA 2020-style, two streams), `"factorial"` (invented 2 x 2 with pools), `"trial_data"` (counts from an invented data frame built with a fixed seed), `"ml"` (prediction-model development and external validation, events), `"cv"` and `"forecast"` (fold grids from invented data); invented data are built with `with_example_seed()`, which restores the caller's random state; each example records its source in `attr(, "source")`.

Scope for MVP: vertical top-down flows and the bracket merge. Horizontal/crossing flows and CRAN submission come later.

## Commands

R 4.5.2 is on PATH as `Rscript`.

- Load for interactive work: `Rscript -e "devtools::load_all()"`
- Regenerate docs/NAMESPACE (roxygen2): `Rscript -e "devtools::document()"`
- All tests: `Rscript -e "devtools::test()"`
- One test file: `Rscript -e "devtools::test(filter = 'layout')"` (matches `tests/testthat/test-layout.R`)
- Full check (aim for 0 errors/warnings/notes): `Rscript -e "devtools::check()"`

## Working rules for this project

- Layout bugs are visual. After changing layout or renderer code, render the reference diagram to PNG and look at it (Read the image) before claiming it works; tests on coordinates alone are not enough.
- Test the layout as data (expected coordinates, no overlaps, bracket spans the right leaves) separately from rendering.
- `ROADMAP.md` is the record of phases and progress. Update it at every milestone step: the status table, the milestone's status lines, and a new dated entry at the end of the progress log (what changed, how it was checked, commit, what is open). Never rewrite past log entries.
- Scratch renders go in `dev/` (git-ignored).
- Picture snapshots: `tests/testthat/_snaps/render/*.svg` (vdiffr). After an intended visual change, review and accept with `testthat::snapshot_review()`.
