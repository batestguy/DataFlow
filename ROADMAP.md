# DataFlow roadmap

DataFlow draws publication-quality study-design flow diagrams (sampling,
clustering, selection, assignment, splits, exclusions, merges) from short,
pipe-able R code. This file is the single record of the phases, what each one
delivers, how it is checked, and where it stands. It is updated at every
milestone step; the progress log at the bottom is append-only.

**Status legend:** ✅ done · 🟡 built, awaiting approval · ⏳ in progress ·
⬜ not started · ⏸ needs a decision

## At a glance

| # | Milestone | Status | Approved by user |
|---|---|---|---|
| 0 | Scaffold | ⏳ public repo created and pushed; first CI run pending | 2026-11-22 (go-ahead) |
| 1 | Lab design | ✅ built, reviewed, merged into `main` | 2026-11-22 |
| 2 | Randomised trial with dropouts | ⬜ | — |
| 3 | Multi-stage cluster survey | ⬜ | — |
| 4 | Factorial design | ⬜ | — |
| 5 | Examples from recent papers | ⬜ | — |
| 6 | Documentation and website | ⬜ | — |
| 7 | CRAN submission | ⏸ separate decision, later | — |

## Locked decisions

| Topic | Decision |
|---|---|
| Name / licence | `DataFlow`, MIT |
| Release | Public GitHub repository from day one; CRAN later |
| Input | Short pipe-able R code, one verb per stage, verbs prefixed `flow_` |
| Counts | Typed by hand, drawn exactly as given, never computed or checked (numbers only get thousands separators) |
| Output | A ggplot object (restyleable) plus saved PDF/PNG at the diagram's natural size |
| Stage definition | Each stage has a built-in type and a free-text label and note |
| Step types | split, apply-to-group (bracket), pool/merge, exclusion side box, cluster sampling, selection, assignment |
| Direction | Top to bottom with branches |
| Look | Text in boxes; stage names as row labels down the left margin; right-angled connectors by default |
| Large stages | Per-stage `display = "all"` / `"summary"` (summary = one box, e.g. "9 conditions each") |
| Dependencies | ggplot2 plus base R (grid, grDevices, stats, utils); boxes and brackets are our own code |
| Accessibility | Every diagram has a plain-language description, attached as alt text |
| Working method | Claude builds; the user reviews pictures and approves each milestone |

## Phases

### Milestone 0 — Scaffold 🟡

**Delivers:** package skeleton (DESCRIPTION, MIT licence, roxygen2 docs,
testthat 3rd edition), git repository, pinned environment (renv),
GitHub Actions `R CMD check` workflow, `.gitignore` for the 6 MB reference
photo and scratch renders.

**Done when:** the package installs and checks locally; the public GitHub
repository exists and the CI check is green on it.

**Status:**
- ✅ Local scaffold, git, renv, CI workflow file (2026-11-22).
- ✅ Public GitHub repository `batestguy/DataFlow` created and pushed
  (2026-11-22), author email changed to the user's university address first.
- ⬜ First green CI run on GitHub (not yet observed).

### Milestone 1 — Lab design ✅

**Delivers:** `flow_start`, `flow_split`, `flow_apply` (with `over =` for
brackets), `flow_outcome`; layout engine `flow_layout()`; renderer
(`autoplot()`, `plot()`, `theme_dataflow()`, `flow_save()`);
`flow_describe()`; `flow_example("lab")` reproducing the hand-drawn design.

**Done when:** the rendered lab diagram looks journal-ready, Claude has
viewed it, and the user approves it.

**Checks:**
- Layout as data: no box overlaps, each split parent centred over its
  children, each bracket spans exactly its sources, every arrowhead lands on a
  box, no line or bracket passes through a box, row labels sit left of all
  boxes.
- Irregular designs (partial splits with `from =`, uneven branches, brackets
  over sources on different rows) under the same checks.
- Exact wording of the lab description.
- Picture snapshot of the lab diagram (local only; skipped on CI because
  fonts differ between machines).
- `devtools::check()`: 0 errors, 0 warnings, 0 notes.

**Status:** built, two review rounds passed (see log). Lab diagram renders at
6.75 × 6.26 in. Approved by the user on 2026-11-22 and merged into `main`.

### Milestone 2 — Randomised trial with dropouts ⬜

**Delivers:** `flow_assign` (arms, `method = "random"` etc.),
`flow_exclude` (side box with reasons and counts), side-box layout to the
right of the arrow it hangs off; a CONSORT-style trial example.

**Done when:** a trial diagram with screening exclusions, randomisation into
arms, and losses to follow-up renders cleanly and the user approves it.

**Checks:** side boxes don't cover arrows or other boxes; exclusion reasons
and counts described in the alt text; validator requires every exclusion to
have a parent.

### Milestone 3 — Multi-stage cluster survey ⬜

**Delivers:** `flow_select` (sampling/selection, `method =`),
`flow_cluster` (cluster sampling), and `display = "summary"` for stages too
large to draw box by box.

**Done when:** a multi-stage survey (e.g. regions → districts → households
→ respondents) renders on one page using summary rows, and the user approves.

**Checks:** a summary stage draws one box per parent; the layout warns for
rows over about 20 boxes and now points to `display = "summary"`.

### Milestone 4 — Factorial design ⬜

**Delivers:** crossing splits (every level of one factor under every level
of another) and `flow_pool` (several sources merging into one box with
converging arrows).

**Done when:** a 2 × 2 (or larger) factorial design and a pooled analysis
render cleanly, and the user approves.

**Checks:** validator requires at least two sources per pool; pooled boxes
centred under their sources; no crossings.

### Milestone 5 — Examples from recent papers ⬜

**Delivers:** redrawn design charts from recent open-access papers: an
experiment, a machine-learning dataset (train/validation/test split and
cross-validation folds), and a survey.

**Process:** Claude proposes a shortlist with full citations; only the
examples the user approves are redrawn. Each example records its source
(citation and DOI) and what was simplified.

### Milestone 6 — Documentation and website ⬜

**Delivers:** full README, one vignette per design family, a pkgdown
website, `R CMD check` with 0 errors/warnings/notes, CI green on all
platforms.

### Milestone 7 — CRAN submission ⏸

A separate decision, taken after milestone 6.

## Known limitations (current)

- Counts are drawn as typed; a diagram is only as correct as its numbers.
- Box sizes are measured with the PDF device's standard font metrics. Other
  font families trigger one warning and may not fit their boxes exactly.
  Drawing far below the natural size can make text spill out of boxes.
- Top-to-bottom diagrams only.
- `display = "summary"` is not built yet (errors until milestone 3).

## Open risks

- Wide designs in `"all"` mode may not fit a journal page until summary rows
  exist (milestone 3).
- CI has not run yet; the first push may surface platform differences.
- Reviews so far were by the same model family as the builder; a
  cross-family review is advisable before the first public release.

## Progress log

Newest last. One entry per working step: what changed, how it was checked,
what is still open.

### 2026-11-22 — Milestone 0: scaffold
- Package skeleton written by hand (the usual helper names the package after
  the folder, which is not a valid package name). MIT licence, roxygen2,
  testthat 3e.
- renv project library; test tools (testthat, vdiffr) recorded in the lock
  file; `renv::status()` reports a consistent state.
- GitHub Actions check workflow added, with renv switched off on CI.
- Reference photo and `dev/` scratch folder are git-ignored; confirmed never
  committed.
- Commit `3cea810` on `main`.

### 2026-11-22 — Milestone 1: lab design built
- Verbs, layout engine, renderer, description and the lab example on branch
  `m1-lab-design` (commit `95a689c`).
- Design choices beyond the hand drawing: right-angled connectors (straight
  arrows available), counts as "(n = 50)" on a second line (format
  configurable), light grey fill on outcome boxes, wider gap between groups
  than between single boxes.
- Checked: 97 tests passing, `devtools::check()` 0/0/0, picture viewed.

### 2026-11-22 — Milestone 1: review round 1 and fixes
- Independent review confirmed all claims for the lab design and found
  problems in irregular designs: a long arrow could pass through an unrelated
  box; a bracket source on a higher row was left unconnected; large or
  decimal counts were rounded to 7 digits; descriptions were ambiguous for
  repeated labels and partial splits; an unknown font flooded warnings; the
  picture snapshot would fail on CI.
- Fixed in commit `636e896`: the layout reserves room for long connectors
  and bracket drop lines; counts keep 15 significant digits; descriptions
  name boxes by their parent ("Batch 1 under Type A"); stricter input checks;
  one font warning; snapshot skipped on CI.
- Each fix has a regression test; the new layout tests were confirmed to fail
  against the old code. 132 tests passing; `devtools::check()` 0/0/0.

### 2026-11-22 — Milestone 1: review round 2
- Focused re-review: all nine findings fixed; 400 randomly generated designs
  gave 0 overlaps and 0 lines through boxes; descriptions read correctly.
- Closed the last gap it noted: the crossing test now also covers bracket
  lines (commit `053a91d`).
- Open: user approval of the lab picture; go-ahead for the public GitHub
  repository.

### 2026-11-22 — Milestone 1 approved; public repository
- User approved the lab picture and gave the go-ahead for a public
  repository at `github.com/batestguy/DataFlow`.
- Author email changed from a personal address to `jzbannister@atbu.edu.ng`
  in DESCRIPTION, and in the author/committer fields of all earlier commits
  (rewritten before the first push, so nothing public changed). Commit ids
  in the log entries above refer to the old history; the new ids are:
  `3cea810` → `4f052ee`, `95a689c` → `1ecaf5b`, `636e896` → `868c061`,
  `053a91d` → `339381a`, `8d99630` → `48537fc`, `4d405c9` → `1e4a1b1`.
- `m1-lab-design` merged into `main`; both branches pushed.
- Open: result of the first CI run on GitHub.
