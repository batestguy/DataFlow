test_that("the lab design is described in plain sentences", {
  expect_equal(
    unclass(flow_describe(flow_example("lab"))),
    paste(
      "Flow diagram with 8 stages, read from top to bottom.",
      "It starts with Cultures (n = 100).",
      "Experiment ID: Cultures is split into Ex. 1 (n = 50) and Ex. 2 (n = 50).",
      "Culture type: Ex. 1 leads to Type A (n = 50); Ex. 2 leads to Type B",
      "(n = 50).",
      "Batch: each of the 2 boxes above (Type A and Type B) is split into",
      "Batch 1 (n = 25) and Batch 2 (n = 25).",
      "Cell culture composition: each of the 4 boxes above is split into",
      "A, B and C.",
      "Condition: 9 conditions each, applied across all boxes within each",
      "Experiment ID (Ex. 1 and Ex. 2).",
      "Task: 2 tasks each (per condition), for each of the 2 boxes above.",
      "Number of trials: the outcome is 600 each, for each of the 2 boxes",
      "above."
    )
  )
})

test_that("notes and differing labels are described", {
  d <- flow_start("People", n = 10, note = "adults only") |>
    flow_split("Site", c("North", "South")) |>
    flow_outcome("Result", c("score", "time"))
  txt <- unclass(flow_describe(d))
  expect_match(txt, "People (n = 10; adults only)", fixed = TRUE)
  expect_match(txt, "score after North; time after South", fixed = TRUE)
})

test_that("print wraps the description", {
  expect_output(print(flow_describe(flow_example("lab"))), "Flow diagram")
})
