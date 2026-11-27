# studydesignflow

<!-- badges: start -->
<!-- badges: end -->

studydesignflow draws publication-quality study-design flow diagrams (splits,
brackets that bring groups back together, outcomes) from a few lines of
pipe-friendly R code. The result is a ggplot object you can restyle, save as
PDF or PNG, and describe in plain sentences for alternative text. The
aim: draw how your data came to be, in any field.

**Status:** early development (milestones 0 to 6 and parts of 7 done).
Splits, bracket merges, outcomes, random assignment, exclusion boxes with
reasons, cluster sampling, selection, summary rows, several starting
streams, pooling, copies (every branch gets all the units), the method of
assignment or sampling shown in the picture, a number check, counts
computed from a data frame, machine-learning data splits with outcome
events, and fold grids for cross-validation and time-series evaluation
work. What comes next is in
`ROADMAP.md`. Built-in examples: `flow_example("lab")`, `"trial"`,
`"survey"`, `"review"`, `"factorial"`, `"trial_data"`, `"ml"`, `"cv"` and
`"forecast"`.

## When another package fits better

- PRISMA 2020 review diagrams: [PRISMA2020](https://cran.r-project.org/package=PRISMA2020).
- Standard clinical flows with automatic warnings when typed numbers do not
  add up (studydesignflow reports them only when you run `flow_check()`):
  [selecta](https://cran.r-project.org/package=selecta).
- A flow chart made automatically from a dplyr pipeline, with joins:
  [dtrackr](https://cran.r-project.org/package=dtrackr).
- Pictures of existing resampling objects:
  [timetk](https://cran.r-project.org/package=timetk) and
  [spatialsample](https://cran.r-project.org/package=spatialsample)
  (seen in their help pages, not tried).

The test behind this list, and its limits: `notes/comparison-2026-11-25.md`.

## Install

```r
# install.packages("remotes")
remotes::install_github("batestguy/studydesignflow")
```

## Example

```r
library(studydesignflow)

design <- flow_start("Cultures", n = 100) |>
  flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = c(50, 50)) |>
  flow_split("Culture type", list("Type A", "Type B"), n = 50) |>
  flow_split("Batch", c("Batch 1", "Batch 2"), n = 25) |>
  flow_split("Cell culture composition", c("A", "B", "C")) |>
  flow_apply("Condition", "9 conditions each", over = "Experiment ID") |>
  flow_apply("Task", "2 tasks each\n(per condition)") |>
  flow_outcome("Number of trials", "600 each", join = FALSE)

plot(design)                       # draw it
flow_save(design, "design.pdf")    # save at its natural size
flow_describe(design)              # plain-language description
```

## Several starting streams and pools

Give `flow_start()` several labels to start several columns side by side,
for example the two columns of a PRISMA 2020 flow diagram. Continue one
stream with `from =`; the other waits and continues on a later row.
`flow_pool()` merges boxes into one, drawn with converging lines:

```r
flow_start(c("Records from databases", "Records from other sources"),
           n = c(1954, 77), stage = "Identification") |>
  flow_exclude("Duplicates removed", n = 452,
               from = "Records from databases") |>
  flow_apply("Screening", "Records screened", n = 1502,
             from = "Records from databases") |>
  flow_pool("Included", "Studies included in review", n = 43)
```

`flow_pool(over = "Drug")` makes one pool per box of an earlier stage (for
example the cells of a factorial design pooled per drug group), and
`groups = list(...)` lists the boxes of each pool. See
`flow_example("review")` and `flow_example("factorial")`.

## Copies: every branch gets all of them

`flow_split()` divides units between boxes. When every box gets all of
them (two people label the same records, every person tries every
treatment), use `flow_copy()`. Each copied box then says so in the picture
("the same 1,960 as above"), in the description ("each of Annotator 1 and
Annotator 2 receives all the units left in Records"), and to the number
check:

```r
flow_start("Records", n = 2000) |>
  flow_exclude("Removed", n = 40) |>
  flow_copy("Labelling", c("Annotator 1", "Annotator 2")) |>
  flow_pool("Adjudication", "Disagreements resolved", n = 1960)
```

With data in one row per unit and treatment, `flow_copy(by = "treatment")`
gives each copy its own rows, and `count =` in `flow_start()` counts the
units (see Counts computed from data).

## How units were assigned or sampled

The method given to `flow_assign()`, `flow_cluster()` or `flow_select()`
(for example `"random"` or `"probability proportional to size"`) is written
in italics under the row label, and always in the description. Turn it off
with `plot(design, show_method = FALSE)`.

## Checking the numbers

`flow_check()` reports counts that cannot all be right (boxes of a split
that do not add up to the box above, reasons that do not add up to their
exclusion box, more excluded than there were) and, as notes, counts that
may be right but are worth a look. It never changes the diagram:

```r
flow_start("Assessed", n = 500) |>
  flow_exclude("Excluded", n = 100,
               reasons = c("Not eligible" = 60, "Declined" = 30)) |>
  flow_split("Randomised", "Randomised", n = 400) |>
  flow_assign("Allocation", c("Drug", "Placebo"), n = c(200, 190)) |>
  flow_check()
```

## Counts computed from data

Counts are typed by hand unless you start the flow from a data frame. Then
each box remembers which rows are behind it, the counts are computed, and
`flow_data()` returns the rows behind any box:

```r
d <- flow_start("Assessed for eligibility", data = trial_data) |>
  flow_exclude("Excluded", reasons = list("Under 18" = ~ age < 18,
                                          "Declined" = ~ consent == "no")) |>
  flow_apply("Randomisation", "Randomised") |>
  flow_assign("Allocation", by = "arm") |>
  flow_exclude("No outcome recorded", where = ~ is.na(outcome_score)) |>
  flow_outcome("Analysis", "Analysed")

flow_data(d, "Randomised")   # the rows behind that box
flow_describe(d)             # ends "Counts were computed from the data
                             # frame `trial_data` (412 rows)."
```

- `by = "column"` (in `flow_split()`, `flow_assign()`, `flow_cluster()`)
  makes one box per value present. `labels` can fix the order, give
  readable names (`c("Ex. 1" = "exp1")`) and add planned groups with no
  rows yet (count 0); a value in the data that `labels` leaves out is an
  error, so nothing is dropped silently. A group with no rows gets no boxes
  below it unless you give `labels` for the next level too.
- `where = ~ condition` selects rows (`flow_select()`) or excludes them
  (`flow_exclude()`). Formula `reasons` in `flow_exclude()` count each row
  under the first reason it matches.
- `count = "patient_id"` in `flow_start()` counts distinct patients rather
  than rows. If one patient has rows in two boxes made from the same box,
  their counts add up to more than the box above, and studydesignflow warns.
- `flow_data()` returns a data frame when the flow started from one data
  frame, and always a named list (one data frame per source) when it
  started from several.
- `flow_from_data(data, c("site", "ward"))` draws the whole nested
  structure of a data frame, one row of boxes per column.
- `flow_edit()` changes any box's text, count or note, or a row label,
  without rebuilding the flow. Counts changed by hand are marked in
  `flow_nodes()` and listed in the description as typed, not computed.
  It does not remove boxes; change the structure with the verbs and
  `from =`.

A typed `n` always wins, and boxes made without data (typed labels) have no
rows, so counts below them stay blank rather than guessed. See
`flow_example("trial_data")`.

## Machine-learning designs

studydesignflow documents the split you made; it never splits data at random or
fits a model. `flow_partition()` draws the split into training, validation
and test sets. From data, `by =` names the column holding each row's set;
`group = "patient_id"` checks that no patient is in two sets, and
`time = "date"` (dates or numbers, not text) checks that the sets follow
each other in time and adds each set's date range. Outcome events, asked for by TRIPOD+AI (item 20a),
are shown with every count:

```r
flow_start(c("Development data", "External validation data"),
           data = list(development, validation),
           events = ~ died_30d == 1) |>
  flow_exclude("Excluded", reasons = list("Under 18" = ~ age < 18)) |>
  flow_partition(labels = c("Training set" = "train", "Test set" = "test"),
                 by = "set", group = "patient_id", from = 1)
# boxes read e.g. "Training set (n = 763; events = 38)"
```

Without data, type the numbers: `flow_partition(labels = c("Training",
"Test"), n = c(800, 200), events = c(64, 16), group = "patient")`; the
description then says the grouping was not checked against data. External
validation is a second starting stream. See `flow_example("ml")`.

## Cross-validation and time-series grids

`flow_folds()` draws a second kind of diagram: one row per split, showing
which part of the data is used for training, validation and testing.

```r
flow_folds(k = 5, n = 82)                                  # 5-fold CV
flow_folds(cv_data, fold = "fold", group = "patient_id")   # from data
flow_folds(type = "expanding", n = 48, initial = 24, assess = 3,
           skip = 2, gap = 1, time_unit = "months")
flow_folds(sales, type = "rolling", time = "month", initial = 36,
           assess = 6, skip = 5, time_unit = "months")
flow_folds(type = "blocked", n = 60, k = 5, gap = 2)
```

Time-series designs have `initial` training periods, `assess` periods in
each test window, origins `skip + 1` periods apart, and a `gap` left out
between training and test. `initial`, `assess` and `skip` mean what they
mean in rsample's `rolling_origin()`; `gap` is an embargo, like
`assess_start = gap + 1` in its `sliding_*()` functions, not
`rolling_origin(lag =)`. Expanding windows grow, rolling windows move;
blocked cross-validation cuts time into contiguous blocks and also trains on
later data, so it does not evaluate forecasting. From data, each distinct
time value is one period and counts are computed. These grids draw the
design your settings describe, so check them against your resampling code.

Or draw the split you actually made, from a data frame with one row per
record per split; then the group and time checks are real checks of it:

```r
flow_folds(assignments, split = "split", role = "role",
           roles = c(Training = "analysis", Test = "assessment"),
           time = "week", all_times = unique(sales$week))
```

Every block carries its role as text, or is explained in a legend, so the
grid does not rely on colour. Blocks too thin to see are drawn wider than
scale, and the legend says so.
Plot, save and describe grids like flow diagrams; `flow_edit()` changes a
block's text or count (marked as typed). See `flow_example("cv")` and
`flow_example("forecast")`.

## What it does not do

- Typed counts are drawn exactly as you type them. `flow_check()` reports
  counts that do not add up, but numbers that fit each other can still be
  wrong, and a change of unit or a group used twice can look like an error.
  With data, counts are computed, but the data must hold what the diagram
  claims.
- Box sizes come from text measured with the PDF device's standard fonts. If
  you draw with a very different font, or much smaller than the natural size,
  text can spill out of boxes.
- Only top-to-bottom flow diagrams, plus fold grids, are supported.
- It does not split data or fit models: it draws and checks the split you
  made.

## Licence

MIT.
