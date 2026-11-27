# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Status

Package `studydesignflow` (MIT), public at github.com/batestguy/studydesignflow; CI (R CMD check, 5 platforms) runs on every push to `main`. Milestones 0 (scaffold), 1 (lab design), 2 (randomised trial: `flow_assign`, `flow_exclude`), 3 (survey: summary rows, `flow_cluster`, `flow_select`) 4 (several starting streams, `flow_pool`, counts from data, `flow_edit`) and 5 (machine-learning designs including time series: `flow_partition`, outcome events, fold grids with `flow_folds`) are done and approved by the user. The goal (2026-11-24): everyone can describe how their data came to be, in any field. Milestone 6 (side-by-side test, `notes/comparison-2026-11-25.md`) is done; 6b is in progress (117 textbook designs drawn and graded; list in `notes/textbook-designs-list.md`, edit only `notes/textbook-designs.R`); textbook designs come before any paper redrawing. Milestone 7 parts 7-0 (`flow_check()`), 7i (method shown under the row label, on by default) and 7a (copies, `flow_copy()`) are done and approved (2026-11-26), 7j (one measurement box, `flow_outcome(join =)`) 7b (units, `unit =`, `count =`) and 7c (step details, `step_details()`, numbered key) on 2026-11-27; the 71 other textbook designs were drawn, read blind and graded on 2026-11-26 (`notes/textbook-ext-results-2026-11-26.md`); next is 7d physical and data steps. The examples page (milestone 10a) is one long page with contents: about 30 textbook designs from R datasets, then about 20 from papers, written after the features. Milestones 7 to 12 and decisions D1 to D3 (all yes, 2026-11-25) are in `ROADMAP.md`, the evidence in `notes/` (tracked, not in the built package). Principle: studydesignflow documents splits the user made; it never splits data at random or fits models. The plan was last revised on 2026-11-24 (milestones 6 to 13) and on 2026-11-25 (6b). Phases, locked decisions, status, known limitations and the progress log are in `ROADMAP.md`; verbs are prefixed `flow_`. Session handover notes (transient, git-ignored) are in `CLAUDE.local.md`.

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

- `R/flow.R` — S3 class `studydesignflow`: `stages` (id, name, row_label, type, display, over, method), `nodes` (id, stage, label, n as text, note, anchor), `edges` (from, to, kind = arrow/bracket/pool/measure; "measure" = one outcome box joined from several branches by `flow_outcome(join =)`, see `outcome_joins()` in `R/verbs.R`: joined when no count is typed, branches give the same box and are not copies of the same units), `side` (exclusion boxes: id, stage, parent, label, n, note), `reasons` (side, label, n), `leaves`; optional `data` (see `R/data.R`) and `edited` (counts changed by `flow_edit()`), present only when used, so typed flows are unchanged. `anchor` = parent for single-parent boxes, lowest common ancestor of the sources for brackets/pools; NA for top boxes (all on the first stage, several allowed) and for pools joining streams. The layout tree is built from it; `node_upstream()` follows the drawn lines instead (used to refuse a group holding a box and a box below it). `validate_studydesignflow()` checks structure only, never counts.
- `R/verbs.R` — `flow_start`, `flow_split`, `flow_apply` (`over =` draws brackets), `flow_outcome`. Shared engine `attach_per_group()`. Every verb takes `from =` (box ids or leaf labels) and `row_label =` (`""` hides it). `display = "summary"` (one box per parent, label from a `summary` template with `{k}`/`{stage}`; helper `summary_counts()`) works for the split engine `split_boxes()` and is refused for apply/outcome.
- `R/trial.R` — `flow_assign` (a split of type "assign" with `method`, shared engine `split_boxes()`) and `flow_exclude` (side boxes on their own "exclude" stage row, hanging off current end boxes only; never changes `leaves`).
- `R/survey.R` — `flow_cluster` (split of type "cluster", summary by default, `method`) and `flow_select` (one box per current box via `attach_per_group(method =)`; `where =` in data mode).
- `R/pool.R` — `flow_pool` (default / `over =` / `groups =`; `attach_per_group(groups =, join = "pool")`); at least two sources per pool, a box in at most one pool per call.
- `R/data.R` — data mode: `x$data` holds the data frames once (global row numbers via `offsets`), `count` column, and `rows$box` / `rows$left` (rows still in the flow after exclusions) / `rows$side` per box id. `by =` (engine `split_by()`), `where =` and formula `reasons` compute counts; apply/outcome/pool carry rows and compute a missing `n`; boxes made without data have no rows (counts below stay blank). `flow_data()`, `flow_from_data()`, `describe_data()`.
- `R/check.R` — `flow_check()` (read-only report, class `studydesignflow_check`; "problem" = cannot both be right if one unit, "note" = may be right): what is left is a box minus its exclusion boxes above the row; counts and events computed from data and not edited are trusted; row subsets lose the class. Evidence scripts: `dev/m7_mutate.R`, `dev/m7_core.R`.
- `R/edit.R` — `flow_edit()` (labels, counts, events, notes, row labels; never rows or structure; for grids, block labels and counts via `grid_edit()`); `typed_counts()` lists counts and events not computed from data.
- `R/copy.R` — `flow_copy()` (split engine with stage type "copy": every new box receives all units left in its parent; default counts from `left_count_text()`, rows shared in data mode; summary parents refused). The layout marks copied boxes with a first note line (`copy_notes()`, `copy_format`/`copy_none`); `pool_units()` in `R/check.R` bounds the distinct units of a pool of copies, used by `flow_check()` and the description.
- `R/units.R` — units (part 7b): `unit =` on every box-making verb and `flow_edit()`, stored only when given (`x$units`, `x$side_units`, named by box id; `""` = none) and resolved when read by `box_units()` / `side_units()` (own, else the box above; joined boxes when their sources agree). `fill_count()` draws "(50 cultures)" ("n = {n}" becomes "{n} {unit}"; `{unit}` placeholder); `flow_check()` skips comparisons across a change of unit. Data mode: `count =` on the verbs (not `flow_exclude`) stored in `x$data$counts`, resolved by `box_count_cols()`; `count_rows(x, ids, col)`.
- `R/details.R` — step details (part 7c): `step_details()` (who, tool, when, where, how) via `details =` on every box-making verb and `flow_edit()`; `x$details` (only when used) has `steps` (by stage id), `boxes`, `side` (by box id). Layout `show_details = TRUE`: `detail_marks()` numbers them from a first layout pass, markers under the row label or after a box label, `detail_key()` (measured wrapping) gives `lay$key` under the diagram; `describe_details()` adds "Done by ..." sentences.
- `R/ml.R` — `flow_partition` (split of stage type "partition"; data mode: `by =` set column, `group =` checked so no group is in two sets, `time =` checked so sets are in time order, date ranges as notes; typed `group`/`time` described as not checked; recorded in `x$partition`). Outcome events: `x$events` (text per box id, only when used), from `flow_start(events = ~ ...)` in data mode (computed in `set_rows()`) or typed `events =` on start/split/partition/apply/outcome/pool; typed events in data mode listed in `x$events_typed`.
- `R/grid.R` — second diagram type, S3 class `studydesignflow_grid` built by `flow_folds()`: `type` kfold / blocked / expanding / rolling / assigned (the user's own assignments, `R/grid_assign.R`: real group/time checks, `all_times` for unseen gaps, no count where rows are not in the data), `axis` folds, time or rows, `blocks` (id, split, role Training/Validation/Test/Gap/Not used, start, end in fold or period units, n, label), rows behind blocks in `x$data$rows`, `times` (distinct time values in data mode). Windows follow rsample's rolling-origin vocabulary (`initial`, `assess`, `skip`, `gap`). `grid_edit()`, `grid_data()`.
- `R/grid_draw.R` — `grid_layout()` (pure, inches; block text picked from "Test\n(n = 18)", "Test", a short form, none by measured width; counts that do not fit go to the row end; fold grids widen to fit; `widen_blocks()` draws thin blocks wider than scale without covering others; legend lists only short, blank or widened roles; greyscale fills, dashed gaps; date ticks from `pretty()`), `autoplot.studydesignflow_grid()` / `plot()`, `describe_grid()` (called by `flow_describe()`).
- `R/layout.R` — `flow_layout()`: pure function, coordinates in inches. Box text with events via `box_text_events()` (only when events exist); `merge_segments()` draws each stretch of line once. Text measured on an off-screen `pdf(NULL)` device. Contour packing over the anchor tree; bracket/pool boxes centred under their sources. `group_gap` separates groups, `hgap` single boxes. Side boxes sit at parent x + `side_gap` and are reserved in the parent's contour; `crossed_rows()` decides where a line still runs down the parent's centre (past its exclusion boxes) before branching, for elbows, brackets and pools alike. Several top boxes are packed side by side with `group_gap`; `show_method = TRUE` (off by default, default not settled) writes `method` under the row label and lengthens gaps, never box heights; pools that join streams are placed afterwards by `place_joined()` (centred under source centres) and drawn as converging lines.
- `R/render.R` — `autoplot.studydesignflow()` / `plot.studydesignflow()` / `theme_studydesignflow()` / `flow_save()` (natural size from the layout; also takes fold grids). Alt text from `flow_describe()` via `labs(alt =)`.
- `R/describe.R` — `flow_describe()`. Repeated box names are qualified by the parent's full name (`unique_names()`); cluster/select stages read "<stage>, selected by <method>: ... from <box>".
- `R/examples.R` — `flow_example("lab")`, `"trial"` (invented CONSORT 2025-style trial), `"survey"` (invented DHS-style household survey), `"review"` (invented PRISMA 2020-style, two streams), `"factorial"` (invented 2 x 2 with pools), `"trial_data"` (counts from an invented data frame built with a fixed seed), `"ml"` (prediction-model development and external validation, events), `"cv"` and `"forecast"` (fold grids from invented data); invented data are built with `with_example_seed()`, which restores the caller's random state; each example records its source in `attr(, "source")`.

Scope for MVP: vertical top-down flows and the bracket merge. Scope rule: every addition must answer "how did the data come to be?" and be needed by at least two gallery workflows (ROADMAP, milestone 10).

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
- Scratch renders go in `dev/` (git-ignored). Research notes and the scripts behind their numbers go in `notes/` (tracked).
- Other packages are tried in a separate library (`dev/complib/`), never installed into the renv project library.
- Picture snapshots: `tests/testthat/_snaps/render/*.svg` (vdiffr). After an intended visual change, review and accept with `testthat::snapshot_review()`.
