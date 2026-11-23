# DataFlow

<!-- badges: start -->
<!-- badges: end -->

DataFlow draws publication-quality study-design flow diagrams (splits,
brackets that bring groups back together, outcomes) from a few lines of
pipe-friendly R code. The result is a ggplot object you can restyle, save as
PDF or PNG, and describe in plain sentences for alternative text.

**Status:** early development (milestone 4 of 9). Splits, bracket merges,
outcomes, random assignment, exclusion boxes with reasons, cluster sampling,
selection, summary rows, several starting streams, pooling, and counts
computed from a data frame work. Machine-learning designs and
reporting-guideline templates are planned; see `ROADMAP.md`. Built-in
examples: `flow_example("lab")`, `"trial"`, `"survey"`, `"review"`,
`"factorial"` and `"trial_data"`.

## Install

```r
# install.packages("remotes")
remotes::install_github("batestguy/DataFlow")
```

## Example

```r
library(DataFlow)

design <- flow_start("Cultures", n = 100) |>
  flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = c(50, 50)) |>
  flow_split("Culture type", list("Type A", "Type B"), n = 50) |>
  flow_split("Batch", c("Batch 1", "Batch 2"), n = 25) |>
  flow_split("Cell culture composition", c("A", "B", "C")) |>
  flow_apply("Condition", "9 conditions each", over = "Experiment ID") |>
  flow_apply("Task", "2 tasks each\n(per condition)") |>
  flow_outcome("Number of trials", "600 each")

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
  their counts add up to more than the box above, and DataFlow warns.
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

## What it does not do

- Typed counts are drawn exactly as you type them and never checked, so a
  typed diagram is only as correct as the numbers given to it. With data,
  counts are computed, but the data must hold what the diagram claims.
- Box sizes come from text measured with the PDF device's standard fonts. If
  you draw with a very different font, or much smaller than the natural size,
  text can spill out of boxes.
- Only top-to-bottom diagrams are supported.

## Licence

MIT.
