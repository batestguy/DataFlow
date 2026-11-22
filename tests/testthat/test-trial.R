trial_layout <- function(...) flow_layout(flow_example("trial"), ...)

test_that("flow_assign is a split that records its method", {
  d <- flow_start("Randomised", n = 300) |>
    flow_assign("Allocation", c("A", "B"), n = 150)
  expect_equal(d$stages$type[2], "assign")
  expect_equal(d$stages$method[2], "random")
  expect_equal(d$nodes$n, c("300", "150", "150"))
  same <- flow_start("Randomised", n = 300) |>
    flow_split("Allocation", c("A", "B"), n = 150)
  expect_equal(flow_layout(d)$boxes[, c("x", "y", "w", "h")],
               flow_layout(same)$boxes[, c("x", "y", "w", "h")])
  expect_true(is.na(flow_assign(flow_start("R"), "Arm", c("A", "B"),
                                method = NULL)$stages$method[2]))
  expect_error(flow_assign(flow_start("R"), "Arm", c("A", "B"), method = ""),
               "must not be empty")
})

test_that("flow_exclude adds side boxes and leaves the flow where it was", {
  d <- flow_start("Assessed", n = 412)
  e <- flow_exclude(d, "Excluded", n = 112,
                    reasons = c("Not eligible" = 68, "Declined" = 44))
  expect_equal(e$leaves, d$leaves)
  expect_equal(nrow(e$nodes), 1)
  expect_equal(e$side$parent, 1L)
  expect_equal(e$side$n, "112")
  expect_equal(e$reasons$label, c("Not eligible", "Declined"))
  expect_equal(e$reasons$n, c("68", "44"))
  expect_equal(e$stages$type[2], "exclude")
  expect_equal(e$stages$row_label[2], "")
  # the next step attaches below the parent, not to the side box
  nxt <- flow_split(e, "Randomisation", "Randomised", n = 300)
  expect_equal(nxt$edges$from, 1L)
})

test_that("exclusion reasons: unnamed, per box, and bad input", {
  d <- flow_start("R") |> flow_assign("Arm", c("A", "B"))
  e <- flow_exclude(d, "Lost", reasons = c("Moved", "Died"))
  expect_equal(nrow(e$reasons), 4)
  expect_true(all(is.na(e$reasons$n)))
  e2 <- flow_exclude(d, "Lost", n = c(3, 1),
                     reasons = list(c(Moved = 3), c(Died = 1)))
  expect_equal(e2$reasons$label, c("Moved", "Died"))
  expect_equal(e2$reasons$side, e2$side$id)
  expect_error(flow_exclude(d, "Lost", reasons = list(c(Moved = 3))),
               "one element per box")
  expect_error(flow_exclude(d, "Lost", reasons = c(1, 2)), "must be text")
  expect_error(flow_exclude(d, "Lost", reasons = c(a = 1, 2)),
               "needs a name")
  expect_error(flow_exclude(d, ""), "not empty")
})

test_that("exclusions hang off end boxes only and get unique stage names", {
  d <- flow_start("R") |> flow_assign("Arm", c("A", "B"))
  expect_error(flow_exclude(d, "Lost", from = 1), "current end boxes")
  e <- d |> flow_exclude("Lost", from = "A") |> flow_exclude("Lost")
  expect_equal(e$stages$name[3:4], c("Lost", "Lost 2"))
  expect_equal(e$side$parent, c(2L, 2L, 3L))
  f <- flow_exclude(d, "Lost", stage = "Follow-up")
  expect_equal(f$stages$row_label[3], "Follow-up")
  expect_error(flow_exclude(f, "Lost", stage = "Follow-up"), "already")
})

test_that("validator checks exclusion boxes", {
  e <- flow_start("A") |> flow_exclude("X")
  bad <- e
  bad$side$parent <- 99L
  expect_error(validate_dataflow(bad), "hang off an existing box")
  bad <- e
  bad$side$stage <- 1L
  expect_error(validate_dataflow(bad), "exclusion stage")
  bad <- flow_exclude(e, "Y", reasons = "why")
  bad$reasons$side <- 99L
  expect_error(validate_dataflow(bad), "reason must belong")
})

test_that("row_label = \"\" hides a row label", {
  d <- flow_start("Assessed", stage = "Enrolment") |>
    flow_split("Randomisation", "Randomised", row_label = "")
  lay <- flow_layout(d)
  expect_equal(lay$row_labels$label, "Enrolment")
  expect_equal(d$stages$name[2], "Randomisation")
})

test_that("an exclusion box sits right of its parent's line, between rows", {
  lay <- trial_layout()
  b <- lay$boxes
  s <- lay$side
  expect_equal(nrow(s), 3)
  for (i in seq_len(nrow(s))) {
    p <- b[b$id == s$parent[i], ]
    expect_equal(s$xmin[i], p$x + 0.3, tolerance = 1e-9)
    expect_true(s$ymax[i] < p$ymin)
    below <- b[b$row == p$row + 2 & abs(b$x - p$x) < 1e-9, ]
    expect_equal(nrow(below), 1)
    expect_true(s$ymin[i] > below$ymax)
    # its arrow runs level from the parent's line into the box's left edge
    arrow <- lay$segments[lay$segments$kind == "side" & lay$segments$head &
                            abs(lay$segments$xend - s$xmin[i]) < 1e-9, ]
    expect_equal(nrow(arrow), 1)
    expect_equal(arrow$x, p$x)
    expect_equal(arrow$y, s$y[i])
    expect_equal(arrow$yend, s$y[i])
  }
})

test_that("trial layout: arrowheads land on boxes; rows ordered; text fits", {
  lay <- trial_layout()
  b <- lay$boxes
  s <- lay$side
  heads <- lay$segments[lay$segments$head, ]
  on_top <- vapply(seq_len(nrow(heads)), function(i) {
    any(abs(b$ymax - heads$yend[i]) < 1e-9 &
          heads$xend[i] >= b$xmin & heads$xend[i] <= b$xmax)
  }, logical(1))
  on_side <- vapply(seq_len(nrow(heads)), function(i) {
    any(abs(s$xmin - heads$xend[i]) < 1e-9 &
          heads$yend[i] > s$ymin & heads$yend[i] < s$ymax)
  }, logical(1))
  expect_true(all(on_top | on_side))
  expect_equal(nrow(heads), nrow(b) - 1 + nrow(s))
  expect_true(all(diff(tapply(c(b$y, s$y), c(b$row, s$row), mean)) < 0))
  w <- with_measure_device(text_width(s$text, 9, "", "plain"))
  expect_true(all(s$w >= w))
  expect_true(all(lay$row_labels$x + lay$row_labels$w < min(b$xmin)))
  expect_equal(lay$row_labels$label,
               c("Enrolment", "Allocation", "Follow-up", "Analysis"))
})

test_that("trial designs: no overlaps and no line through any box", {
  for (nm in names(trial_designs())) {
    for (con in c("elbow", "straight")) {
      lay <- flow_layout(trial_designs()[[nm]], connector = con)
      expect_equal(boxes_overlap(all_boxes(lay)), 0,
                   label = paste(nm, con, "overlaps"))
      expect_equal(crossings(lay), 0, label = paste(nm, con, "crossings"))
    }
  }
})

test_that("the line under a parent with an exclusion box branches below it", {
  lay <- flow_layout(trial_designs()$split_below_exclusion)
  s <- lay$side
  bar <- lay$segments[lay$segments$kind == "elbow" &
                        abs(lay$segments$y - lay$segments$yend) < 1e-9, ]
  expect_equal(nrow(bar), 1)
  expect_true(bar$y < s$ymin)
})

test_that("a bracket below exclusion boxes sits under them", {
  lay <- flow_layout(trial_designs()$exclusion_then_bracket)
  s <- lay$side
  for (g in split(lay$brackets, lay$brackets$group)) {
    expect_true(max(g$y) < min(s$ymin))
  }
})

test_that("the trial is described in plain sentences", {
  expect_equal(
    unclass(flow_describe(flow_example("trial"))),
    paste(
      "Flow diagram with 6 stages, read from top to bottom.",
      "It starts with Assessed for eligibility (n = 412).",
      "Excluded (n = 112) after Assessed for eligibility: Did not meet",
      "inclusion criteria (n = 68), Declined to participate (n = 31) and",
      "Other reasons (n = 13).",
      "Randomisation: Assessed for eligibility leads to Randomised",
      "(n = 300).",
      "Allocation: Randomised is randomly assigned to Exercise programme",
      "(n = 150; Received allocated intervention (n = 146); Did not receive",
      "it (n = 4)) and Usual care (n = 150; Received usual care (n = 150)).",
      "Follow-up: Lost to follow-up (n = 9) after Exercise programme: Moved",
      "away (n = 5) and Withdrew consent (n = 4); Lost to follow-up (n = 6)",
      "after Usual care: Moved away (n = 2), Withdrew consent (n = 3) and",
      "Died (n = 1).",
      "Analysis: the outcome is Analysed (n = 141) after Exercise programme;",
      "Analysed (n = 144) after Usual care."
    )
  )
})

test_that("assignment methods are described", {
  d <- flow_start("Eligible")
  expect_match(flow_describe(flow_assign(d, "Arm", c("A", "B"),
                                         method = "minimisation")),
               "Eligible is assigned by minimisation to A and B")
  expect_match(flow_describe(flow_assign(d, "Arm", c("A", "B"),
                                         method = NULL)),
               "Eligible is assigned to A and B")
})

test_that("print shows exclusion boxes", {
  expect_output(print(flow_example("trial")),
                "6 stages, 6 boxes, 3 exclusion boxes")
  expect_output(print(flow_example("trial")), "\\[exclude\\] Follow-up")
})

test_that("the trial diagram draws exclusion boxes and looks right", {
  p <- autoplot(flow_example("trial"))
  expect_true(any(vapply(p$layers, function(l) {
    "text_x" %in% names(l$data)
  }, logical(1))))
  expect_match(ggplot2::get_alt_text(p), "randomly assigned")
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("trial design", p)
})
