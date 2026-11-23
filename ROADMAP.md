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
| 0 | Scaffold | ✅ public repo, CI green on 5 platforms | 2026-11-22 |
| 1 | Lab design | ✅ built, reviewed, merged into `main` | 2026-11-22 |
| 2 | Randomised trial with dropouts | ✅ built, reviewed, merged into `main` | 2026-11-22 |
| 3 | Multi-stage cluster survey | ✅ built, reviewed, merged into `main` | 2026-11-22 |
| 4 | Factorial design, pooling, several starting streams; counts from data | ✅ built, reviewed, merged into `main` | 2026-11-23 |
| 5 | Machine-learning designs, including time series | 🟡 built on branch `m5`, awaiting review and approval | — |
| 6 | Reporting-guideline templates | ⬜ | — |
| 7 | Examples from recent papers | ⬜ | — |
| 8 | Documentation and website | ⬜ | — |
| 9 | CRAN submission | ⏸ separate decision, later | — |
| T | Time-based designs (later track) | ⏸ after milestone 8 | — |

Plan revised on 2026-11-22 after a survey of reporting guidelines, other R
packages and machine-learning designs (see "Positioning" and the progress
log). Milestones 4 to 9 replace the earlier 4 to 7.

## Locked decisions

| Topic | Decision |
|---|---|
| Name / licence | `DataFlow`, MIT |
| Release | Public GitHub repository from day one; CRAN later |
| Input | Short pipe-able R code, one verb per stage, verbs prefixed `flow_` |
| Counts | Typed by hand, drawn exactly as given, never checked (numbers only get thousands separators); or, optionally, computed from a data frame, each box keeping the rows behind it (decided 2026-11-23) |
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

### Milestone 0 — Scaffold ✅

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
- ✅ First green CI run on GitHub (2026-11-22, run 44993834534).

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

### Milestone 2 — Randomised trial with dropouts ✅

**Delivers:** `flow_assign` (arms, `method = "random"` etc.),
`flow_exclude` (side box with reasons and counts), side-box layout to the
right of the arrow it hangs off; a CONSORT-style trial example.

**Done when:** a trial diagram with screening exclusions, randomisation into
arms, and losses to follow-up renders cleanly and the user approves it.

**Checks:** side boxes don't cover arrows or other boxes; exclusion reasons
and counts described in the alt text; validator requires every exclusion to
have a parent.

**Status:** built and reviewed (2026-11-22). Example follows the CONSORT
2025 flow diagram (Hopewell et al., BMJ 2025;389:e081123,
doi:10.1136/bmj-2024-081123) with invented numbers. Picture approved by the
user on 2026-11-22; merged into `main`.

### Milestone 3 — Multi-stage cluster survey ✅

**Delivers:** `flow_select` (sampling/selection, `method =`),
`flow_cluster` (cluster sampling), and `display = "summary"` for stages too
large to draw box by box.

**Done when:** a multi-stage survey (e.g. regions → districts → households
→ respondents) renders on one page using summary rows, and the user approves.

**Checks:** a summary stage draws one box per parent; the layout warns for
rows over about 20 boxes and now points to `display = "summary"`.

**Status:** built on branch `m3-survey` (2026-11-22). Example
`flow_example("survey")`: an invented two-stage household survey in the
style of a Demographic and Health Survey. Natural size 7.50 x 10.24 in at
9 pt; scaled to a 6.5 in text width the text is about 7.8 pt. Reviewed;
picture approved by the user on 2026-11-22; merged into `main`.

### Milestone 4 — Factorial design, pooling, several starting streams; counts from data ✅

**Delivers:** crossing splits (every level of one factor under every level
of another); `flow_pool` (several sources merging into one box with
converging arrows); several starting boxes in one diagram whose streams run
side by side and later join. The last one unlocks systematic reviews
(PRISMA 2020: databases and other sources in two columns), case-control
studies (cases and controls from different sources), external validation
datasets, and mixed-methods designs.

**Done when:** a 2 x 2 factorial with a pooled analysis and a two-stream
diagram that joins render cleanly, and the user approves.

**Also delivers (added 2026-11-23, user decision):** an optional data mode.
`flow_start(data =, count =)` stores the data once; `by =`, `where =` and
formula `reasons` compute counts; `flow_data()` returns the rows behind any
box; `flow_from_data()` draws a data frame's nested structure; `flow_edit()`
changes labels, counts, notes and row labels afterwards (edited counts are
marked as typed).

**Checks:** validator requires at least two sources per pool; pooled boxes
centred under their sources; parallel streams keep their own rows and
exclusion boxes without overlap; no crossings; random stress test extended.
Data mode: counts equal the rows (or distinct values) of the right subsets;
typed flows unchanged.

**Status:** built on branch `m4` (2026-11-23). Examples
`flow_example("review")` (PRISMA 2020 style), `"factorial"` and
`"trial_data"`. Independent review passed after fixes; pictures approved by
the user on 2026-11-23; merged into `main`.

### Milestone 5 — Machine-learning designs, including time series 🟡

**Delivers:** dataset splits (train / validation / test) including grouped
splits (all records of a patient or site on one side) and time-ordered
splits (older data for training, newer for testing); external validation on
a second dataset; participant flow for prediction-model studies following
TRIPOD+AI item 20a (numbers and outcome events per analysis); a new grid
drawing for cross-validation folds (rows = folds, blocks = train / test),
which is a second diagram type next to the flow diagram. Added by the user
(2026-11-23): time-series designs in the grid, namely expanding-window and
rolling-window evaluation (initial training window, forecast horizon, step
between forecast origins, gap between training and test) and blocked k-fold
for time-ordered data. Principle: DataFlow documents a split the user
already made; it never splits data at random or fits models. Anything drawn
from data stays editable by hand, with hand-typed counts marked as typed.

**Done when:** a model-development flow with a grouped split and external
validation, a 5-fold cross-validation grid and a time-series evaluation grid
render cleanly and the user approves.

**Checks:** the description says which data were used for what and that test
data were kept apart; fold grids are tested as data like the flow layout
(blocks add up to the data, test windows never overlap training, gaps sit
between them, time order kept).

**Status:** built on branch `m5` (2026-11-23): `flow_partition()`, outcome
events, `flow_folds()` fold grids (k-fold, blocked, expanding and rolling
windows) with `grid_layout()`, examples `"ml"`, `"cv"` and `"forecast"`.
Awaiting independent review and the user's approval of the pictures.

### Milestone 6 — Reporting-guideline templates ⬜

**Delivers:** one-call starting points that follow the official layouts:
CONSORT 2025 (trial), PRISMA 2020 (systematic review), STARD 2015
(diagnostic accuracy: index test, reference standard, true/false
positives/negatives), STROBE-style cohort attrition, TRIPOD+AI. Each cites
its guideline and is checked against the official template before it is
built (not yet seen for STARD and TRIPOD+AI; STROBE flow-diagram wording
not yet verified).

**Done when:** each template renders, matches its guideline's boxes, and the
user approves.

### Milestone 7 — Examples from recent papers ⬜

**Delivers:** redrawn design charts from recent open-access papers, at least
one per design family (experiment, trial, survey, machine learning,
systematic review).

**Process:** Claude proposes a shortlist with full citations; only the
examples the user approves are redrawn. Each example records its source
(citation and DOI) and what was simplified.

### Milestone 8 — Documentation and website ⬜

**Delivers:** full README, one vignette per design family, a pkgdown
website, `R CMD check` with 0 errors/warnings/notes, CI green on all
platforms, and a short honest comparison with other R packages.

### Milestone 9 — CRAN submission ⏸

A separate decision, taken after milestone 8.

### Later track T — Time-based designs ⏸

Designs whose shape is a timeline rather than a flow: crossover trials
(periods and washout), stepped-wedge cluster trials (staggered switch-over;
CONSORT extension template), panel surveys with waves and attrition,
platform trials (arms added and dropped). Needs a left-to-right time axis.
Also: "cloning" in target trial emulation (the same people copied into every
strategy), which needs its own wording.

**Out of scope:** causal diagrams (DAGs), already covered by the ggdag
package.

## Positioning (2026-11-22, from documentation only; not tested side by side)

Other R packages that draw study-flow charts: `consort`, `flowchart` (on
CRAN since February 2024, pipe verbs `fc_filter`, `fc_split`, `fc_merge`,
`fc_stack`), `visR` (attrition charts), `GroupCONSORT` (per-site counts) and
`PRISMA2020` (systematic reviews, built on DiagrammeR). Most build CONSORT
or attrition charts and compute counts from a data frame.

Where DataFlow differs: design-shaped diagrams beyond attrition (nested
splits, brackets that rejoin groups, summary rows, cluster sampling); output
is a restyleable ggplot; every diagram carries a plain-language description
as alt text; layout tested as data (no overlaps, no lines through boxes,
random stress tests). Where others are ahead: counts computed from data, a
CRAN release, more users and a longer track record.

**Decision (user, 2026-11-23):** yes to an optional data-driven mode that
computes counts from a data frame, built in milestone 4; typed counts stay as
they are. A diagram drawn from data stays fully editable (`flow_edit()`),
since the data may not reflect the whole design.

## Known limitations (current)

- Typed counts are drawn as typed; a typed diagram is only as correct as
  its numbers. In data mode counts are computed, but only for steps given
  `by =`, `where =`, formula `reasons`, or (apply, outcome, pool) no `n`;
  boxes made without data have no rows and counts below them stay blank.
  A typed exclusion in data mode also blanks the counts below it.
- In data mode, formula reasons count a row under the first reason it
  matches; with `count =` a column, reason counts need not add up to the
  box count (a person can have rows under two reasons), and boxes made from
  one box can share a person, so their counts add up to more than the box
  above (a warning says so).
- In data mode, a box with no rows (a planned group) gets no boxes below it
  unless `labels` are given for the next level. Values of a `by` column
  that print alike (0.3 and 0.1 + 0.2) are one group.
- `flow_edit()` changes text and counts only; boxes and branches cannot be
  removed or moved after they are built (use `from =` and the verbs).
- Box sizes are measured with the PDF device's standard font metrics. Other
  font families trigger one warning and may not fit their boxes exactly.
  Drawing far below the natural size can make text spill out of boxes.
- Top-to-bottom diagrams only.
- Several starting boxes and pools exist (milestone 4). A box can feed only
  one pool per call, so overlapping pools (for example both margins of a
  factorial design) are not supported.
- A pool whose sources sit either side of another line that continues
  downward (another stream, or interleaved `groups =`) crosses that line;
  the random test found such crossings in 22 of 800 layouts. Lines never
  pass through boxes.
- Outcome events are shown in flow boxes only, not in exclusion boxes.
- `flow_partition()` checks groups and time order only in data mode with
  `by =`; typed `group` and `time` are described as not checked. With
  `time`, the sets must be given in time order (`labels`).
- Fold grids draw time on a linear scale of periods (one period per
  distinct time value in data mode, however far apart the values are).
  Very narrow blocks (for example a one-month gap in eight years) show as a
  thin sliver with a dashed edge; their role is then given only by the
  legend and the description.
- `flow_edit()` on a fold grid changes block text and counts only; the
  windows themselves are changed by calling `flow_folds()` again.
- Descriptions of repeated box names in parallel streams can be long
  ("Reports assessed for eligibility under Reports sought for retrieval
  under Records screened").
- `display = "summary"` works for steps that split boxes (`flow_split`,
  `flow_assign`, `flow_cluster`); the summary box shows a count, not the
  names of the boxes it stands for.
- Large designs can still exceed a page at 9 pt (the survey example is
  7.5 x 10.2 in); journals scale figures, or use a smaller `base_size`.
- Exclusion boxes always sit to the right of the line they hang off. Long
  reasons are not wrapped, so many wide exclusion boxes make wide diagrams.
- Reasons are listed with an ASCII "- " and a fixed "(n = ...)"; they do not
  follow `count_format` or `side_format`.
- Where a bracket joins boxes that have exclusion boxes below them, the join
  is drawn as a plain horizontal line under the exclusion boxes (no end
  ticks).

## Open risks

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

### 2026-11-22 — Author name
- Author in DESCRIPTION set to Jerry Bannister (aka batestguy), at the
  user's request. Future commits in this repo use the same name.

### 2026-11-22 — First CI run green
- Pushes did not start the check workflow (0 runs after three pushes,
  although Actions was enabled and the workflow active). Added
  `workflow_dispatch` and started it by hand: run 44993834534 passed on all
  five platforms (macOS, Windows, Ubuntu release, devel, oldrel-1).
- Open: whether pushes now trigger runs on their own.

### 2026-11-22 — Push-triggered CI fixed
- Symptom: GitHub received every push (repository activity log) and other
  apps got check suites, but Actions created none for 7 pushes to `main`.
  Not explained by GitHub status (all operational), branch or path filters,
  token type, or turning the workflow off and on.
- Adding a second, minimal `on: push` workflow made both it and
  `R-CMD-check` start on that push; after removing it, an ordinary push
  started `R-CMD-check` by itself (run 44996079146, green on all 5
  platforms). Root cause on GitHub's
  side not established: most likely the push trigger was not registered for
  the new repository until a workflow file was added.
- The empty test commits (`3661372`, `88a62a9`) and the temporary workflow
  stay in history.

### 2026-11-22 — Milestone 2: trial design built
- New verbs `flow_assign()` (a split that records how boxes were assigned,
  e.g. randomly; shown in the description only) and `flow_exclude()`
  (exclusion box to the right of the line leaving each end box, with
  optional reasons and counts; the flow continues below the same boxes).
- New `row_label =` argument on `flow_split`, `flow_apply`, `flow_outcome`,
  `flow_assign` (`""` hides a row label). `flow_example("trial")`: an
  invented two-arm trial in the CONSORT 2025 layout.
- Layout: each exclusion has its own row; the exclusion box is reserved in
  its parent's outline so neighbouring branches make room for it; lines
  that branch out below a parent with exclusion boxes, and brackets below
  them, start under the exclusion boxes.
- Description: "Excluded (n = 112) after ...: reason (n = 68), ...";
  "randomly assigned to"; note lines joined with "; ".
- Checked: 244 tests pass (112 new); `devtools::check()` 0/0/0; lab
  picture snapshot unchanged; 9 awkward trial designs and 400 random designs
  mixing all steps (795 exclusion boxes) gave 0 overlaps and 0 lines
  through boxes in both connector styles. Random testing found two bugs
  (a line to a lower exclusion box crossing a neighbour's exclusion box;
  child lines skipping a row crossing their parent's own exclusion box),
  both fixed; their regression tests were confirmed to fail without the fix.
  Pictures viewed: the trial and all awkward designs.
- Open: independent review; user approval of the trial picture.

### 2026-11-22 — Milestone 2: review and approval
- Independent review (same model family) re-ran 244 tests and the check
  (0/0/0), reproduced the 400-design stress result, ran its own wider random
  designs (five seeds, varied font sizes and gaps: 0 problems) and hand-built
  awkward cases, and confirmed milestone 1 layouts are identical to `main`
  on 200 random designs. It could not check the claim that the regression
  tests fail without the fixes (single commit); that was checked in-session
  by swapping the old layout code back in (1 crossing each).
- Fixed from the review: CONSORT citation volume 388 -> 389 (verified
  against the BMJ record) with DOI; named reasons with a missing count are
  refused (the description read as if one count covered two reasons);
  `flow_exclude(stage = "")` refused; note lines joined with ", " in
  descriptions (was "; ", the same as the count separator); describe copes
  with objects that have no reasons table; CLAUDE.md and the known
  limitations brought up to date.
- Not changed (recorded as limitations): reasons ignore `count_format`; no
  wrapping of long reasons; the wide-row warning ignores exclusion boxes.
- User approved the trial picture. 247 tests pass; check 0/0/0. Merged
  into `main` (`4f8cd31`); CI run 44999943531 green on all 5 platforms.
- A cross-family review is still advisable before the first release.

### 2026-11-22 — Milestone 3: survey design built
- `display = "summary"` for `flow_split` and `flow_assign`, and the default
  for the new `flow_cluster()` (cluster sampling): one box per current box,
  labelled by a `summary` template with `{k}` (number of boxes it stands
  for; `labels` may be that number) and `{stage}`.
- New `flow_select()` (units selected within each box, e.g. households per
  EA) with `method =`. Methods of `flow_cluster`/`flow_select` are described
  as "<stage>, selected by <method>: ... from <box>".
- Descriptions of summary stages say each box stands for several boxes not
  drawn. The wide-row warning now points to `display = "summary"` and counts
  exclusion boxes. `display = "summary"` on `flow_apply`/`flow_outcome`
  is refused with an explanation.
- `flow_example("survey")`: invented household survey (frame of 9,200 EAs,
  3 regions x urban/rural, 270 EAs by PPS shown as 6 summary boxes, 25
  households per EA, non-response boxes for households and women). Region
  names are invented on purpose so the numbers cannot be mistaken for a real
  survey. Arithmetic: 3,100+2,800+3,300 = 9,200; strata sum to their
  regions; 270 EAs x 25 = 6,750 households; 6,750-412 = 6,338;
  8,211-307 = 7,904; reasons sum to their totals.
- Checked: 293 tests pass (46 new); `devtools::check()` 0/0/0; lab and
  trial picture snapshots unchanged; 400 random designs mixing every step
  including cluster, select and summary splits (seed 20261123,
  `dev/stress3.R`): 0 overlaps, 0 lines through boxes. Picture viewed.
- Open: independent review; user approval of the survey picture.

### 2026-11-22 — Milestone 3: review, approval and plan revision
- Independent review (same model family): 293 tests and the check re-run
  (0/0/0); stress script instrumented and confirmed it really produced
  cluster (181), select (188) and summary (344) stages; milestone 1 and 2
  layouts and descriptions identical to `main` on 300 random designs.
- Fixed from the review: repeated box names in descriptions are qualified by
  the parent's full name ("40 EAs selected under Urban under North", not
  the ambiguous "under Urban"); summary note reworded so it is also true for
  one box and avoids double brackets; "there is 1 box" grammar; docs say
  summary rows show a count, not labels; CLAUDE.md and README brought up to
  date; tests added for summary with `from =`, summary with exclusions and a
  bracket, the wide-row warning counting exclusion boxes. 307 tests pass.
- Not changed (nits): default summary templates do not handle singulars
  ("1 clusters selected"); "assigned by <method> to" reads oddly with a bare
  method word.
- User approved the survey picture.
- Plan revised after research: new milestones for several starting streams
  (in 4), machine-learning designs (5) and reporting-guideline templates
  (6); examples, documentation and CRAN move to 7 to 9; time-based designs
  become a later track. Positioning section added. Open decision: optional
  counts computed from data. Merged into `main` (`dca78c6`); CI run
  45002933135 green on all 5 platforms.

### 2026-11-23 — Milestone 4: streams, pools, counts from data built
- User decision recorded: optional counts computed from data, built in this
  milestone; later in the day the user added `flow_from_data()` and
  `flow_edit()` (a diagram drawn from data must stay editable).
- Part A (commit `02c5972`): `flow_start()` takes several labels (streams
  side by side, `group_gap` apart); validator relaxed to "at least one top
  box, all on the first stage"; `flow_pool()` (default, `over =`,
  `groups =`) drawn as converging lines into one arrow, pools across
  streams placed after the streams (no object change); descriptions "2
  boxes side by side" and "X and Y are combined into Z"; examples
  `"review"` (PRISMA 2020 layout, Page et al., BMJ 2021;372:n71, citation
  from memory, not re-checked online) and `"factorial"`. Random testing
  found one bug: a pool could join a box with a box below it when an
  earlier pool had joined streams; the check now follows the drawn lines.
  Its regression test was confirmed to fail with the old check, which
  accepted the bad pool.
- Part B (second commit on `m4`): data mode (`data =`, `count =`, `by =`,
  `where =`, formula `reasons`, automatic counts for apply/outcome/pool),
  `flow_data()`, `flow_from_data()` (rebuilds the lab design's first five
  rows of boxes and counts from a 100-row data frame), `flow_edit()` with
  `n_edited` in `flow_nodes()` and typed counts listed in the description,
  `labels` mapping readable names and planned groups, example
  `"trial_data"` (412 invented participants, fixed seed, caller's random
  state restored).
- Checked: 627 tests pass; `devtools::check()` 0/0/0; 315 typed designs
  (lab, trial and survey examples, irregular and trial designs, 300 random
  designs)
  give identical objects, descriptions and layouts to `main`; random stress
  test `dev/stress4.R` (seed 20261123): 400 designs x 2 connectors, 0
  overlaps and 0 lines through boxes; it produced 234 multi-start designs,
  259 one-stream steps, 119 pool boxes (81 across streams); line-on-line
  crossings in 22 of 800 layouts (also 4 of 630 on `main`). Pictures
  viewed: review, factorial, trial_data, lab, trial, survey, and six
  awkward stream designs.
- Open: independent review; user approval of the review, factorial and
  trial_data pictures; whether pools should reorder streams to avoid line
  crossings.

### 2026-11-23 — Milestone 4: review of the data mode, fixes
- Independent review (same model family) found three ways to get wrong
  numbers without an error, all fixed: `flow_edit()` applied values in the
  order of the boxes in the diagram, not the order given (swapped labels
  and counts); a factor in one data frame and text in another were combined
  as the factor's integer codes (bogus labels "1", "2"); values of a `by`
  column that print alike (0.3 and 0.1 + 0.2) made two boxes with the same
  label, counting rows twice. `by` now also stops with an internal error if
  its boxes ever hold a different number of rows than the box above.
- Also from the review: a warning when `count =` values sit in two boxes
  made from one box; `flow_edit(n = NA)` clears a count; typed reason counts
  under a computed exclusion box are listed as typed in the description;
  `flow_data()` returns a list whenever there are several data frames;
  clearer errors for a condition written without `~`, for reasons mixing
  formulas and numbers, and for empty text values in a `by` column; a box
  with no rows stays an end box instead of stopping `by` (so
  `flow_from_data()` copes with planned groups); summary `{k}` counts
  planned labels; the raw newline in the data sentence's pattern replaced
  by `
`.
- New `tests/testthat/test-data-random.R`: 60 random data flows (one or two
  data frames, factor and text) checking that boxes made by `by` hold
  exactly the rows of the box above, that box rows = continuing rows +
  excluded rows, and that every drawn count equals its rows. The review's
  `dev/rv4_rand.R` (300 flows): 893 steps, 0 problems.
- Checked: the new tests fail 25 times on the reviewed commit (at least
  once for every finding; the random test did not fail there) and pass
  now: 3423 expectations pass, 0 fail. 315 typed designs still identical
  to `main` (one false alarm on the way: evaluating `reasons` earlier
  changed the order in which the random test script drew its random
  numbers, so it built different designs; the original order is kept).

### 2026-11-23 — Milestone 4: each line drawn once
- Where connectors ran along the same line (an arrow and the stub down to
  an exclusion box beside it, a pool line), they were drawn on top of each
  other, which looked thicker in some PNGs (seen in the review and
  trial_data pictures). The layout now merges them: segments with an
  arrowhead are kept whole, other horizontal and vertical segments keep only
  the parts not already drawn (`merge_segments()` in `R/layout.R`).
- Checked: on 315 typed designs x 2 connector styles, boxes, exclusion
  boxes, brackets, row labels, sizes, objects and descriptions are
  identical to `main`; segments equal `main`'s after the merge (214 of 630
  layouts changed, 7,488 segments down to 7,157), and the stretch of line
  covered on every line, every arrowhead and every slanted line is
  unchanged (`dev/m4_cover.R`: 0 differences). New tests: a unit test of
  the merge and a check that no stretch of line is drawn twice in the
  examples and awkward designs.
- Picture snapshots of the trial, survey, review, factorial and trial_data
  diagrams changed: each new SVG only drops duplicate line elements
  (2 to 6 per picture, nothing added or moved); reviewed and accepted. The
  lab snapshot is unchanged (it had no doubled lines). PNGs of all six
  examples re-rendered and viewed: the thicker line under "Exercise
  programme" in trial_data is gone.
- Also re-run after both review commits: `dev/stress4.R` 400 designs x 2
  connector styles, 0 overlaps and 0 lines through boxes; line-on-line
  crossings 34 in the same 22 layouts (36 before: two crossings were
  counted twice through duplicate segments); `dev/rv4_rand.R` 893 steps, 0
  problems; 3519 test expectations pass.

### 2026-11-23 — Milestone 4 approved and merged
- User approved the review, factorial and trial_data pictures. Re-checked
  before merging: all tests pass on `m4` (9c5730a); review and trial_data
  PNGs viewed after the line fix.
- Merged into `main` (`3636687`); CI run 45102882569 green on all 5
  platforms. Open: a
  cross-family review before the first release; PRISMA 2020 citation not
  yet checked online.

### 2026-11-23 — Milestone 5: machine-learning designs built
- User approved starting milestone 5 and added time-series models to it;
  scope recorded in the milestone text (commit `ca90e08`). Principle kept:
  DataFlow documents a split the user made; it never splits data at random
  or fits models, and everything drawn from data stays editable by hand,
  with typed counts marked as typed.
- `flow_partition()`: a split of its own stage type. With data, `by =` the
  set column (readable names, planned sets, order as in `by` splits),
  `group =` checked (error naming which sets share how many groups),
  `time =` checked (each set ends strictly before the next starts) with
  each set's date range as its note. Typed `group`/`time` are described as
  "(not checked against data)".
- Outcome events (TRIPOD+AI item 20a): `flow_start(events = ~ died == 1)`
  computes events in every box with rows, drawn as "(n = 412; events =
  37)"; typed `events =` on start, split, partition, apply, outcome and
  pool; `flow_edit(events =)`; typed events listed as typed in the
  description; `{events}` usable in `count_format`; `flow_nodes()` has an
  `events` column.
- Fold grids, a second diagram type: `flow_folds()` for k-fold (typed, or
  from a fold column with a grouped check), blocked k-fold for
  time-ordered data, and expanding or rolling windows with rsample's
  rolling-origin vocabulary (`initial`, `assess`, `skip`, `gap`), typed or
  from a date column. `grid_layout()` (tested as data), `autoplot()`,
  `plot()`, `flow_save()`, `flow_describe()`, `flow_edit()` and
  `flow_data()` work on grids. Every block carries its role as text or an
  abbreviation with a legend; fills are greys, gaps dashed.
- Examples `"ml"` (two development hospitals pooled, exclusions with
  reasons, grouped training/test split, a third hospital as external
  validation stream; all counts and events from invented data), `"cv"`
  (grouped 5-fold from invented data) and `"forecast"` (expanding window
  with a one-month gap on invented monthly sales of three stores).
- Checked: 10,350 test expectations pass (new: test-ml.R, test-grid.R
  with 60 random grids); `devtools::check()` 0/0/0; 323 designs from
  `main` (typed and data examples, irregular, trial and stream designs,
  300 random designs) give identical objects, descriptions and layouts;
  `dev/stress4.R` 400 designs x 2 connector styles, 0 overlaps and 0 lines
  through boxes; `dev/rv4_rand.R` 893 steps, 0 problems;
  `dev/m5_grid_rand.R` 494 random grids built (153 expanding, 116 rolling,
  111 blocked, 114 k-fold; 252 from data; 6 impossible settings refused),
  0 problems (rows cover the data once, test after training plus gap,
  counts add up, every label fits its block). Pictures viewed:
  `dev/ml.png` (first version was 9.6 in wide; exclusions moved after the
  pooled development box, now 7.0 in), `dev/cv.png`, `dev/forecast.png`
  (first version had odd date ticks and hid the test counts; now year
  ticks and "Test / 18"), and four typed grids (k-fold, expanding,
  rolling, blocked).
- Open: independent review; user approval of the ml, cv and forecast
  pictures.

