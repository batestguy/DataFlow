# DataFlow

<!-- badges: start -->
<!-- badges: end -->

DataFlow draws publication-quality study-design flow diagrams (splits,
brackets that bring groups back together, outcomes) from a few lines of
pipe-friendly R code. The result is a ggplot object you can restyle, save as
PDF or PNG, and describe in plain sentences for alternative text.

**Status:** early development (milestone 1 of 7). Splits, bracket merges and
outcomes work; exclusions, random assignment, cluster sampling, pooling and
summary rows are planned.

## Install

The GitHub repository is not public yet. Once it is:

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

## What it does not do

- It does not compute or check counts. Every number is drawn exactly as you
  type it, so a diagram is only as correct as the numbers given to it.
- Box sizes come from text measured with the PDF device's standard fonts. If
  you draw with a very different font, or much smaller than the natural size,
  text can spill out of boxes.
- Only top-to-bottom diagrams are supported.

## Licence

MIT.
