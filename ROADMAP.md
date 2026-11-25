# studydesignflow roadmap

**Goal (user, 2026-11-24):** everyone can describe their data collection
process with figures, from lab experiments to surveys, scraping, machine
learning and testing. In one sentence: studydesignflow shows how any dataset came to
be. It draws publication-quality diagrams from short, pipe-able R code. This
file is the single record of the phases, what each one
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
| 5 | Machine-learning designs, including time series | ✅ built, reviewed, merged into `main` | 2026-11-24 |
| 6 | Side-by-side test with other packages | 🟡 done, user to read the comparison | — |
| 6b | Textbook designs: 117 listed, then drawn and graded | ⏳ list approved 2026-11-25, nothing drawn | — |
| 7 | Describe any collection step | ⬜ | — |
| 8 | Check and explain | ⬜ | — |
| 9 | Time | ⬜ | — |
| 10 | Textbook vignettes, then gallery of 25 workflows | ⬜ | — |
| 11 | No-code: spreadsheet, then web form | ⬜ | — |
| 12 | Documentation, outside review, first release | ⬜ | — |
| 13 | CRAN submission | ⏸ separate decision, later | — |

Plan revised on 2026-11-24 to follow the goal above; the evidence is in
`notes/scope-research-2026-11-24.md`. Milestones 6 to 13 replace the earlier
6 to 9 and track T (reporting-guideline templates dropped, paper examples
became part of the gallery, time-based designs became milestone 9).

**Three decisions, confirmed by the user on 2026-11-25** (yes to all three):

| # | Question | Decision |
|---|---|---|
| D1 | May the package check whether typed numbers add up? | Yes, as a separate `flow_check()` the user runs; it reports and never changes the picture |
| D2 | Is the no-code route part of the project? | Yes; the user finds five testers who do not code |
| D3 | Start with the side-by-side test? | Yes; small, and may save building what exists |

**Order set by the user (2026-11-25):** textbook designs are tested before any
design is redrawn from a published paper.

## Locked decisions

| Topic | Decision |
|---|---|
| Name / licence | `studydesignflow` (named `DataFlow` until 2026-11-25), MIT |
| Release | Public GitHub repository from day one; CRAN later |
| Input | Short pipe-able R code, one verb per stage, verbs prefixed `flow_` |
| Counts | Typed by hand, drawn exactly as given, never checked while drawing (numbers only get thousands separators; `flow_check()`, milestone 8, will report when asked); or, optionally, computed from a data frame, each box keeping the rows behind it (decided 2026-11-23) |
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
- ✅ Public GitHub repository `batestguy/studydesignflow` created and pushed
  (2026-11-22), author email changed to the user's university address first.
- ✅ First green CI run on GitHub (2026-11-22, run 44993834534).

### Milestone 1 — Lab design ✅

**Delivers:** `flow_start`, `flow_split`, `flow_apply` (with `over =` for
brackets), `flow_outcome`; layout engine `flow_layout()`; renderer
(`autoplot()`, `plot()`, `theme_studydesignflow()`, `flow_save()`);
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

### Milestone 5 — Machine-learning designs, including time series ✅

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
for time-ordered data. Principle: studydesignflow documents a split the user
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
Reviewed independently twice; findings fixed 2026-11-24, including grids of
the user's own split assignments. Approved by the user 2026-11-24.

### Milestone 6 — Side-by-side test 🟡

**Delivers:** an honest written comparison, and a plan adjusted to it.

**Steps:**
1. Confirm D1 to D3 with the user (done 2026-11-25: yes to all).
2. Install `selecta`, `flowchart`, `dtrackr`, `consort` and `TrialFlowR` into
   a separate library in `dev/complib/` (git-ignored). Never into the renv
   project library. Record the versions.
3. Draw the same eight designs in each tool that claims to support them:
   parallel trial, PRISMA review with two streams, factorial with
   recombination, cluster household survey, nested lab design, grouped ML
   split with external validation, scraping to a labelled set, two-annotator
   labelling. Keep every script and picture.
4. Score each on: can it say it truthfully; lines of code; counts from data;
   catches a planted number that does not add up; editable after drawing;
   restyleable; description for screen readers; output formats; readable at
   page width.
5. Settle the open questions in section 6 of the research note that a test can
   settle (`triageR`, fold-grid packages).
6. Write `notes/comparison-<date>.md`, rewrite "Positioning" below, show the
   user.

**Done when:** the user has read the comparison and decided to keep, cut or
change milestones 7 to 12. If another package already does most of milestone
7, say so plainly and propose joining or building on it.

### Milestone 6b — Textbook designs ⏳

**Delivers:** the designs taught in applied classical-statistics textbooks,
from one-sample and paired designs through every kind of ANOVA layout to
multilevel, longitudinal, case-control and survey designs, each drawn with the
package as it is and graded. No package code changes.

**Files:** `notes/textbook-designs.md` (method, grading, vignette rules),
`notes/textbook-designs-list.md` (the list), `.csv`, `.R` (the source of the
list) and `-output.txt`.

**Steps:**
1. ✅ List: 117 designs (46 core, 53 extended, 18 edge), each with a data set,
   a source where the help page names one, counts computed from the data, what
   the picture must say, and a prediction written before testing.
2. ✅ List approved by the user (2026-11-25), nothing added or dropped.
3. ⬜ Check each core design against its book or paper; find sources for H08
   and H14; look for the designs listed without data.
4. ⬜ Answer keys for the 46 core designs, written before drawing.
5. ⬜ Draw, blind-read, grade: core first, then extended, then edge.
6. ⬜ `notes/textbook-results-<date>.md`: grades beside predictions, and what
   they change in milestones 7 and 9 (copies first; rows by columns has no
   planned answer).

**Done when:** all 117 have a grade, or "source not read", and the user has
seen the core pictures.

### Milestone 7 — Describe any collection step ⬜

**Delivers:** every step can say what went in and in what unit, what was done,
who or what did it, when, and what came out or was lost. Built in this order,
each with tests, a picture the user approves, and a description that reads
correctly. Names below are proposals, settled in the design of each part.

| Part | What | Proposed form | Watch for |
|---|---|---|---|
| 7a Copies | Every branch gets all the items (annotators, models, replicated tests, cloning in target-trial emulation) | `flow_copy()`, marked in the picture by more than colour, described as "each receives all 2,000 records" | Data mode: rows shared, pools of copies count distinct items; no warning about shared ids here |
| 7b Units | What each number counts | `unit =` on every verb; "(40 sites)" in place of "(n = 40)" | Default output must not change; `count =` in data mode |
| 7c Step details | Tool or instrument, who, dates, method | `details =` with fixed fields after Datasheets and Croissant; drawn as a note or a numbered key; always in the description | Long text; layout height |
| 7d Physical and data steps | A step is done to things or to data | `kind =`; solid and dashed as in the NC3Rs tool, with a legend | Never colour alone; dashed already means gap in grids |
| 7e Linking | Two sources joined record by record | `flow_link()`: linked box plus unmatched from each side; from data by key | Read GUILD first; one-to-many links; matching errors are typed |
| 7f Shared groups | One group in two analyses | lift "one pool per box per call" | The layout is a tree today: highest risk, do it on its own branch with the stress test extended first |
| 7g Loops | Repeat until (label, check, improve; retries) | a back arrow in the side margin with its rule and number of rounds | Counts are never recomputed round a loop; description must not loop |
| 7h Planned and actual | One diagram for protocol and report | `planned =`; "(planned 100; n = 92)" | Data mode fills the actual number |

**Checks:** existing diagrams identical (`dev/m5_reg.R` and `dev/m5_cmp.R`, 323
designs); stress test extended with copies, links, shared groups and loops
(0 overlaps, 0 lines through boxes); data-mode numbers checked against base R;
independent review.

**Done when:** all ten workflows in `notes/workflow-test.R` draw truthfully
(time axes excepted, milestone 9), the script is rewritten to use the new
steps, the core textbook designs that 7a to 7h were meant to fix are graded
again (milestone 6b), and the user approves the pictures.

### Milestone 8 — Check and explain ⬜

**Delivers:**
1. `flow_check()` (if D1 is yes): numbers that do not add up, reasons that do
   not sum, splits larger than their parent (copies and changes of unit
   excepted), missing units, dates or tools. It reports; it never edits or
   blocks.
2. A long description as a nested list (text, Markdown, HTML), beside the
   short alt text, following the W3C complex-images tutorial; title and
   description inside saved SVG files.
3. Exports from the same object: a step table (CSV), methods text to edit, and
   Croissant RAI fields (names verified first). Exports mark typed, computed
   and missing values.

**Done when:** every mistake in a planted list is caught and none is invented
on the correct examples; a person using a screen reader can say what happened
in three diagrams (automatic checks alone do not count).

### Milestone 9 — Time ⬜

**Delivers:** a third diagram type on a real time axis (calendar time, or time
since entry): panel waves with refreshment and attrition, crossover periods
and washout, stepped-wedge switch-over, platform trials, repeated scraping,
sensors with outages, study windows around an index date (Schneeweiss
framework). Axis to scale, breaks marked, every bar named by text.

**Before building:** read Schneeweiss et al. 2019, the CONSORT stepped-wedge
extension and the SPIRIT schedule figure; look at `swcrt` and `steppedwedge`.

**Done when:** each design above draws on a correct axis checked as data, and
the user approves.

### Milestone 10 — Textbook vignettes, then gallery of 25 workflows ⬜

**10a. Textbook vignettes first.** Regrade all 117 textbook designs with the
finished package; write ten vignettes, one per family A to J, from the core
designs graded true; publish the rest as unsupported with the reason.

**Gate:** 10b and every redrawing from a published paper wait until 10a has
the user's approval.

**10b. Gallery.**

The acceptance test for the goal. Each has code, picture, description, source
(invented, or citation and DOI), and a result: passes, or listed as
unsupported with the reason. At least eight are redrawn from open-access
papers the user approves from a shortlist.

| Family | Workflows |
|---|---|
| Lab | reference culture design; samples to assay with replicates; omics batches |
| Animal and field | animal study with cages; nested ecological sampling; split-plot field trial |
| Trials | parallel; crossover; stepped-wedge |
| Surveys | cluster household; panel with refreshment; mixed web and phone with dispositions |
| Interviews and reviews | interviews and focus groups; sequential mixed methods; systematic review |
| Web | scrape to labelled set; repeated scraping; API collection with limits |
| Sensors and records | sensors with outages; two linked registers; cohort from health records |
| Labelling, ML, testing | double labelling with adjudication loop; prediction model with external validation (checked against TRIPOD+AI); language-model evaluation (prompts, models, raters); software or A/B test |

**Done when:** all 25 have a result and the user has seen each picture.

### Milestone 11 — No-code ⬜ (needs D2)

**Delivers:** first a spreadsheet template (one row per step) and a reader
that turns it into a diagram, with error messages that name the row; then a
web form that runs in the visitor's browser (Shinylive on GitHub Pages),
showing the R code it used. Data stay on the visitor's computer. Keyboard and
screen-reader use, zero axe violations on every view (WCAG 2.2 AA).

**Done when:** five people who do not code each draw their own study unaided;
what stopped them is written down and fixed or listed.

### Milestone 12 — Documentation, outside review, first release ⬜

**Delivers:** README, the textbook vignettes of 10a, a pkgdown website with
the gallery, the comparison with other packages (pointing people to `selecta`,
`consort`, `PRISMA2020` where they fit better), a published list of what is
not supported, a cross-family review (OpenCode `ship-reviewer`), a tagged
release.

### Milestone 13 — CRAN submission ⏸

A separate decision, taken after milestone 12.

### Scope rules

- Every addition must answer "how did the data come to be?" and be needed by
  at least two gallery workflows.
- Left out on purpose: ready-made guideline templates (they exist elsewhere),
  causal diagrams (`ggdag`), free drawing, choosing a design or sample size,
  making splits or fitting models.

## Positioning (tested side by side 2026-11-25)

Evidence: `notes/comparison-2026-11-25.md` (with the limits of the test: the
grader built this package and knows it best). In short:

- **Ours after the test:** designs with many levels that rejoin, several kinds
  of source kept apart, outcome events, sampling stages, one picture type
  across fields, a restyleable ggplot, a written description.
- **No longer ours alone:** pooling a factorial one factor at a time
  (`selecta`), copies and joins (`dtrackr`, for data in R), pictures of folds
  (`timetk`, `spatialsample`, `blockCV`, `mlr3spatiotempcv`).
- **Others are better at:** warning about wrong typed numbers (`selecta`),
  PRISMA 2020 (`PRISMA2020`), following an R data pipeline (`dtrackr`).

### Earlier positioning notes (2026-11-22, corrected 2026-11-24)

**Correction (2026-11-24).** The 2026-11-22 note missed `selecta` (CRAN,
0.6.1): piped code, typed or computed counts, several converging sources,
split and recombine, factorial designs, arithmetic checking, and CONSORT,
STROBE, STARD, PRISMA and MOOSE layouts. Several things listed below as
studydesignflow's own are therefore shared. A search of its source found no
training/test splits, outcome events, fold grids or alternative text (see
`notes/scope-research-2026-11-24.md`); milestone 6 tests this properly. The
2026-11-22 note, kept for the record:

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
  distinct time value in data mode, however far apart the values are; the
  description says when the spacing is not regular). Very narrow blocks are
  drawn wider than scale, which the legend and the description say.
- Expanding, rolling and blocked grids draw the design their settings
  describe; they do not check the user's resampling code. Grids of the
  user's own assignments (`split =`, `role =`) do check it, but time values
  in no split are drawn only when `all_times` is given, and the warning
  that some may be missing relies on a step longer than usual.
- With `time`, own assignments must have training before validation before
  test in every split, so blocked cross-validation assignments can only be
  drawn without `time`.
- The group check of `flow_partition()` covers the boxes of one call, not
  sets made by separate calls on separate streams.
- `flow_edit()` on a fold grid changes block text and counts only (a fold's
  count changes in every split); the
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

### 2026-11-24 — Milestone 5: review findings fixed
- Independent review of `abd6248` confirmed the builder's numbers and found
  two blocking faults and seven others; all fixed. The session that began
  the fixes stopped part-way; its code was saved as `3725d08` (6 test
  expectations failing there) and finished in this entry's commit.
- Blocking: the grouped-split check now runs across all boxes of the stage
  (a patient in training under one arm and test under another is an error);
  time columns must be dates, date-times or numbers (text sorted "10"
  before "9", so a wrong order could pass).
- Missing group values warn and are described; missing outcomes are neither
  events nor non-events, are described, and drawing warns when some are
  left in the end boxes. `{non_events}` in `count_format` (TRIPOD+AI item
  20a asks for both). Typed events outside 0 to n warn. `flow_folds(k =)`
  must agree with the data; blocked grids without training data are
  refused; wording on rsample vocabulary, on irregular times and on what is
  checked corrected.
- Own assignments: `flow_folds(split =, role =, roles =, all_times =)` draws
  the split actually made; group and time checks are real checks of it.
- Found while finishing, and fixed: a gap of time values that no split uses
  vanished from own-assignment grids (now `all_times`, else a warning and a
  sentence in the description); blocks with no rows in the data showed
  "(n = 0)" (now no count); a widened gap could hide a thin test block
  (`widen_blocks()` takes the room from wide neighbours, nothing is
  covered); grids with 10 folds wrote all counts at the row end (grids are
  now made wide enough, and a too narrow `width` warns); the ml example
  warned about missing outcomes that it excludes further down.
- Second independent review of the fixes: nothing blocking. Fixed from it:
  rows without the event were still drawn after a hand-typed `n` (now
  blank); "Checked" sentences when no split has two roles; equal fold sizes
  described once; both ends of a time range written alike; split names that
  are numbers stored as text keep number order. Left as they are: the
  all-times warning also appears for weekday-only data over a weekend; one
  very long hand-typed block label widens a whole fold grid.
- Checked: 12,630 test expectations pass; `devtools::check()` 0/0/0;
  test-grid.R and test-ml.R run against the old code (`abd6248`) fail as
  they should; `dev/m5_reg.R` + `dev/m5_cmp.R` 323 designs, 0 differing
  from `main`; `dev/m5_grid_rand.R` 494 grids, 0 problems;
  `dev/rv4_rand.R` 893 steps, 0 problems; `dev/stress4.R` 400 designs x 2
  connector styles, 0 bad layouts (the last two run before the second
  review's small fixes, which do not touch the flow layout). Reviewer:
  3000 random rows of thin blocks tile without overlap. Pictures viewed:
  ml, cv, forecast, own assignments with and without time, 10 folds from
  data, one-day test and gap over a year. The forecast snapshot was
  replaced (intended change).
- Open: user approval of the ml, cv and forecast pictures.

### 2026-11-24 — Milestone 5 approved
- User approved the ml, cv, forecast, own-assignment and 10-fold pictures;
  `m5` merged into `main`. Open: CI result (recorded in the next entry); a
  cross-family review before the first release.

### 2026-11-24 — CI green for milestone 5
- Merge `0dc147a` pushed to `main`; CI run 45323962438 green on all 5
  platforms. Open: a cross-family review before the first release. Next:
  milestone 6.

### 2026-11-24 — Plan rewritten around the goal
- User asked whether milestone 6 (guideline templates) was needed, stated the
  goal (see the top of this file), asked for proper research, and asked for
  the resulting plan to be documented for the next sessions.
- Evidence: CRAN scan (25,848 packages, 22 hits), text search of the `selecta`
  source, ten new workflows drawn with the current package (nine draw, one
  refused, one of the nine misleading), and reading of reporting guidance
  across fields. Scripts, outputs and sources, with what was read and what was
  only seen as a search summary: `notes/`.
- Milestones 6 to 13 replace the earlier 6 to 9 and track T. Decisions D1 to
  D3 recorded as open, with recommendations.
- No package code changed. Next: confirm D1 to D3, then milestone 6.

### 2026-11-25 — Renamed from DataFlow to studydesignflow
- User's choice, after comparing it with `studydesigngraphs`. Reasons:
  "dataflow" is a computing term and a Google product name; "design graph"
  already means something else in experimental statistics; the verbs already
  start with `flow_`. Name availability checked the same day: not on CRAN
  (current 25,850 or archived 28,609), not in Bioconductor software (2,385),
  no GitHub repository of that name. Trademarks not checked.
- Changed: package, S3 classes (`studydesignflow`, `studydesignflow_grid`),
  `theme_dataflow()` to `theme_studydesignflow()`, `validate_dataflow()` to
  `validate_studydesignflow()`, the `_size` attribute, help pages, documents
  and the GitHub repository. Verbs and arguments are unchanged. README gained
  one sentence stating the aim. Earlier entries of this log, the 2026-11-22
  positioning note and `notes/scope-research-2026-11-24.md` keep the old name.
- Objects saved to disk under the old name must be rebuilt; nothing had been
  released, so no compatibility layer was added.
- Checked: 12,630 test expectations pass, picture snapshots unchanged;
  `devtools::check()` 0/0/0; `notes/workflow-test.R` gives the recorded
  output; a search for the old name finds only the places listed above.
- Open: CI result under the new name (next entry); decisions D1 to D3; a
  cross-family review before the first release.

### 2026-11-25 — CI green under the new name
- Commit `96d7f49` pushed to `main`; CI run 45526765601 green on all 5
  platforms. Open: decisions D1 to D3; a cross-family review before the first
  release. Next: milestone 6.

### 2026-11-25 — D1 to D3 confirmed; textbook design list written
- User: yes to the recommendations (D1 to D3). Asked for a rigorous list of
  designs from applied textbooks for the vignettes, documented, to be done
  before testing on papers. Added as milestone 6b, after the side-by-side test
  (D3) and before milestone 7, with a gate in milestone 10.
- List: 117 designs in 11 families, from 23 R packages installed in
  `dev/complib/` (not the renv library). `notes/textbook-designs.R` computes
  every count from the data; 0 of 117 failed. The first run caught four
  mistakes in the list (rooms, states, a column name, a severity that was
  classified afterwards), now corrected.
- Not done: nothing drawn; the books were not read; predictions are not
  results. H08 and H14 have no source on their help page.
- No package code changed. README status line corrected (it said milestone 5
  of 9 in progress).
- Open: user reads the list; then milestone 6 and 6b. A cross-family review
  before the first release.

### 2026-11-25 — Textbook list approved; milestone 6 started
- User approved the textbook design list as it stands (nothing added or
  dropped) and asked to proceed. Milestone 6 started on branch `m6`.

### 2026-11-25 — Milestone 6: side-by-side test run
- Eight designs in `selecta`, `flowchart`, `dtrackr`, `consort` (and
  `PRISMA2020` for the review; `TrialFlowR` has no drawing function), same
  numbers, up to three tries each: `notes/comparison.R`, output and write-up
  in `notes/`. Pictures looked at one by one.
- Found against us: wrong typed numbers are drawn without a word; the sampling
  or allocation method is never in the picture; the description says "split"
  when each branch gets all records.
- Corrected claims: see Positioning. `triageR` does not draw a TRIPOD flow;
  other packages do draw fold pictures.
- Proposed, for the user to decide: move `flow_check()` to the front of
  milestone 7; a small new part 7i (show the method in the picture); 7a copies
  first; point readers to other packages in the README now.
- Open: user's decision on the four proposals and on milestones 7 to 12. The
  grading was done by one reader who built the package.
