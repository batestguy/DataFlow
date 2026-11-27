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
  # joined, the same last row is described as measured in every branch
  expect_match(unclass(flow_describe(lab_joined())),
               paste("Number of trials: 600 each is measured in every branch,",
                     "drawn as one box joined from the 2 boxes in the Task row."),
               fixed = TRUE)
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

test_that("units are assigned, not the box they come from", {
  d <- flow_start("People", n = 30) |>
    flow_apply("Before", "Depression score", n = 30) |>
    flow_assign("Condition", c("SSRI", "Placebo"), n = 15)
  expect_match(flow_describe(d),
               "those in Depression score are randomly assigned to SSRI",
               fixed = TRUE)
  two <- flow_start("Field") |> flow_split("Block", c("B1", "B2")) |>
    flow_assign("Rate", c("low", "high"), n = 3)
  expect_match(flow_describe(two), paste(
    "those in each of the 2 boxes in the Block row are randomly assigned to",
    "low (n = 3) and high (n = 3)"), fixed = TRUE)
  some <- flow_start("Field") |> flow_split("Block", c("B1", "B2", "B3")) |>
    flow_assign("Rate", c("low", "high"), from = c("B1", "B2"))
  expect_match(flow_describe(some),
               "those in each of B1 and B2 are randomly assigned to",
               fixed = TRUE)
})

test_that("one text for several boxes says each", {
  d <- flow_start("Field") |> flow_split("Block", c("B1", "B2")) |>
    flow_outcome("Measured", "Hills", n = 36, join = FALSE)
  expect_match(flow_describe(d),
               "Hills (n = 36 each), for each of the 2 boxes in the Block row",
               fixed = TRUE)
  one <- flow_start("Field") |> flow_outcome("Measured", "Hills", n = 36)
  expect_no_match(flow_describe(one), "each")
})

test_that("wrapped notes get no stray comma, listed notes keep theirs", {
  say <- function(note) unclass(flow_describe(flow_start("A", n = 1, note = note)))
  expect_match(say("by 6 of the\n12 panellists"), "by 6 of the 12 panellists")
  expect_match(say("once in every row,\nonce in every column"),
               "every row, once in every column")
  expect_match(say("Received it (n = 146)\nDid not (n = 4)"),
               "Received it (n = 146), Did not (n = 4)", fixed = TRUE)
  expect_match(say("Adults\nSpeakers"), "Adults, Speakers")
})

test_that("the first row is called by its name, or top", {
  a <- flow_start(c("Cases", "Controls"), n = c(2, 3), stage = "Sampled apart") |>
    flow_outcome("Recorded", "Exposure")
  expect_match(flow_describe(a), "side by side in the Sampled apart row:")
  expect_match(flow_describe(a), "2 boxes in the Sampled apart row")
  b <- flow_start(c("Cases", "Controls"), n = c(2, 3)) |>
    flow_outcome("Recorded", "Exposure", join = FALSE)
  expect_match(flow_describe(b), "for each of the 2 boxes in the top row")
  b2 <- flow_start(c("Cases", "Controls"), n = c(2, 3)) |>
    flow_outcome("Recorded", "Exposure")
  expect_match(flow_describe(b2), "joined from the 2 boxes in the top row")
  expect_no_match(flow_describe(b), "Start row")
})
