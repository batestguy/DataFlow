# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Status

Package `DataFlow` (MIT), public at github.com/batestguy/DataFlow; CI (R CMD check, 5 platforms) runs on every push to `main`. Milestones 0 (scaffold), 1 (lab design) and 2 (randomised trial: `flow_assign`, `flow_exclude`) are done and approved by the user. Phases, locked decisions, status, known limitations and the progress log are in `ROADMAP.md`; verbs are prefixed `flow_`. Session handover notes (transient, git-ignored) are in `CLAUDE.local.md`.

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

- `R/flow.R` — S3 class `dataflow`: `stages` (id, name, row_label, type, display, over, method), `nodes` (id, stage, label, n as text, note, anchor), `edges` (from, to, kind = arrow/bracket), `side` (exclusion boxes: id, stage, parent, label, n, note), `reasons` (side, label, n), `leaves`. `anchor` = parent for single-parent boxes, lowest common ancestor of the sources for brackets/pools; the layout tree is built from it. `validate_dataflow()` checks structure only, never counts.
- `R/verbs.R` — `flow_start`, `flow_split`, `flow_apply` (`over =` draws brackets), `flow_outcome`. Shared engine `attach_per_group()`. Every verb takes `from =` (box ids or leaf labels) and `row_label =` (`""` hides it). `display = "summary"` errors until milestone 3.
- `R/trial.R` — `flow_assign` (a split of type "assign" with `method`, shared engine `split_boxes()`) and `flow_exclude` (side boxes on their own "exclude" stage row, hanging off current end boxes only; never changes `leaves`).
- `R/layout.R` — `flow_layout()`: pure function, coordinates in inches. Text measured on an off-screen `pdf(NULL)` device. Contour packing over the anchor tree; bracket/pool boxes centred under their sources. `group_gap` separates groups, `hgap` single boxes. Side boxes sit at parent x + `side_gap` and are reserved in the parent's contour; `crossed_rows()` decides where a line still runs down the parent's centre (past its exclusion boxes) before branching, for elbows and brackets alike.
- `R/render.R` — `autoplot.dataflow()` / `plot.dataflow()` / `theme_dataflow()` / `flow_save()` (natural size from the layout). Alt text from `flow_describe()` via `labs(alt =)`.
- `R/describe.R` — `flow_describe()`.
- `R/examples.R` — `flow_example("lab")`, `flow_example("trial")` (invented CONSORT 2025-style trial); each example records its source in `attr(, "source")`.

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
