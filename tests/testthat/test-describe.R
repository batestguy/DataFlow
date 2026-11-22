test_that("the lab design is described in plain sentences", {
  expect_equal(
    unclass(flow_describe(flow_example("lab"))),
    paste(
      "Flow diagram with 8 stages, read from top to bottom.",
      "It starts with Cultures (n = 100).",
      "Experiment ID: Cultures is split into Ex. 1 (n = 50) and Ex. 2 (n = 50).",
      "Culture type: Ex. 1 leads to Type A (n = 50); Ex. 2 leads to Type B",
      "(n = 50).",
      "Batch: each of the 2 boxes in the Culture type row is split into",
      "Batch 1 (n = 25) and Batch 2 (n = 25).",
      "Cell culture composition: each of the 4 boxes in the Batch row is",
      "split into A, B and C.",
      "Condition: 9 conditions each, applied across all boxes under each",
      "Experiment ID box (Ex. 1 and Ex. 2).",
      "Task: 2 tasks each (per condition), for each of the 2 boxes in the",
      "Condition row.",
      "Number of trials: the outcome is 600 each, for each of the 2 boxes in",
      "the Task row."
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

test_that("partial splits and repeated labels are described unambiguously", {
  d <- irregular_designs()$uneven_bracket
  txt <- unclass(flow_describe(d))
  expect_match(txt, "Pt: s1 under A1 and s1 under A2 are each split into",
               fixed = TRUE)
  expect_match(txt, "merged label, applied across all boxes under each Arm box (A1 and A2)",
               fixed = TRUE)
})

test_that("mixed brackets and single boxes are described per box", {
  d <- flow_start("All") |>
    flow_split("Arm", c("Ctl", "Trt")) |>
    flow_split("Sub", c("a", "b"), from = "Trt") |>
    flow_apply("Cond", "m", over = "Arm")
  txt <- unclass(flow_describe(d))
  expect_match(txt, "Cond: m after Ctl; m across all 2 boxes under Trt.",
               fixed = TRUE)
})

test_that("grouping over the top stage is described plainly", {
  d <- flow_start("All") |>
    flow_split("Arm", c("A", "B")) |>
    flow_apply("T", "m", over = "All")
  expect_match(unclass(flow_describe(d)),
               "T: m, applied across all 2 boxes under All.", fixed = TRUE)
})
