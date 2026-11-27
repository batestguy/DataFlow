# Part 7j: one outcome box measured in every branch (flow_outcome()).

latin <- function(...) {
  flow_start("Cells", n = 64) |>
    flow_split("Row", paste("Row", 1:8), n = 8) |>
    flow_apply("Solution", "8 solutions,\none cell each") |>
    flow_outcome("Measured", "Decrease\nin volume", ...)
}

test_that("the same outcome under every branch is drawn once", {
  d <- latin()
  m <- d$nodes$id[d$nodes$stage == max(d$stages$id)]
  expect_length(m, 1)
  src <- d$nodes$id[d$nodes$stage == d$stages$id[d$stages$name == "Solution"]]
  expect_setequal(d$edges$from[d$edges$to == m], src)
  expect_true(all(d$edges$kind[d$edges$to == m] == "measure"))
  expect_identical(d$leaves, m)
  # anchored to the lowest common ancestor, the top box
  expect_identical(d$nodes$anchor[d$nodes$id == m], 1L)
  expect_identical(d$stages$type[nrow(d$stages)], "outcome")
})

test_that("the joined box is centred under its sources, nothing overlaps", {
  for (con in c("elbow", "straight")) {
    lay <- flow_layout(latin(), connector = con)
    bx <- lay$boxes
    m <- bx[bx$stage == "Measured", ]
    s <- bx[bx$stage == "Solution", ]
    expect_equal(m$x, mean(range(s$x)))
    expect_gt(min(s$ymin), m$ymax)
    expect_identical(boxes_overlap(all_boxes(lay)), 0)
    expect_identical(crossings(lay), 0)
    sg <- lay$segments[lay$segments$kind == "measure", ]
    # one line down from every source, and one arrow into the joined box
    for (i in seq_len(nrow(s))) {
      expect_true(any(abs(sg$x - s$x[i]) < 1e-9 &
                        abs(sg$y - s$ymin[i]) < 1e-9))
    }
    head <- sg[sg$head, ]
    expect_identical(nrow(head), 1L)
    expect_equal(head$xend, m$x)
    expect_equal(head$yend, m$ymax)
  }
})

test_that("join = FALSE keeps one box per branch, as before", {
  d <- latin(join = FALSE)
  expect_length(d$leaves, 8)
  expect_false(any(d$edges$kind == "measure"))
  expect_error(latin(join = NA), "NULL, TRUE or FALSE")
  expect_error(latin(join = "yes"), "NULL, TRUE or FALSE")
})

test_that("one box to measure gets an arrow", {
  d <- flow_start("A", n = 5) |> flow_outcome("End", "x")
  expect_identical(d$edges$kind, "arrow")
  d <- flow_start("A", n = 5) |> flow_outcome("End", "x", join = TRUE)
  expect_identical(d$edges$kind, "arrow")
})

test_that("different labels, counts, events or notes keep branches apart", {
  base <- flow_start("A", n = 10) |>
    flow_split("Arm", c("X", "Y"), n = 5)
  apart <- function(d) expect_length(d$leaves, 2)
  together <- function(d) expect_length(d$leaves, 1)
  apart(flow_outcome(base, "End", c("p", "q")))
  apart(flow_outcome(base, "End", "p", n = c(4, 5)))
  apart(flow_outcome(base, "End", "p", events = c(1, 2)))
  apart(flow_outcome(base, "End", "p", note = c("a", "b")))
  together(flow_outcome(base, "End", c("p", "p")))
  together(flow_outcome(base, "End", "p", events = 3, note = "a"))
  # a typed count means "each": kept apart unless join = TRUE
  apart(flow_outcome(base, "End", "p", n = 5))
  apart(flow_outcome(base, "End", "p", n = c(5, 5)))
  apart(flow_outcome(base, "End", "p", n = NA_character_))
  together(flow_outcome(base, "End", "p", n = c(5, 5), join = TRUE))
  j <- flow_outcome(base, "End", "p", n = 10, events = 3, note = "a",
                    join = TRUE)
  m <- j$leaves
  expect_identical(j$nodes$n[j$nodes$id == m], "10")
  expect_identical(j$nodes$note[j$nodes$id == m], "a")
  expect_identical(unname(j$events[as.character(m)]), "3")
  # join = TRUE cannot put two counts in one box
  expect_error(flow_outcome(base, "End", "p", n = c(4, 5), join = TRUE),
               "different count")
  expect_error(flow_outcome(base, "End", c("p", "q"), note = c("a", "b"),
                            join = TRUE), "different label and note")
  # values that fit no number of boxes give the usual error
  expect_error(flow_outcome(base, "End", "p", n = c(1, 2, 3)), "3 values")
})

test_that("copies of the same units are kept separate (repeated measures)", {
  # copies by condition, then the outcome (A07/D02-style)
  rep1 <- flow_start("Boys", n = 10) |>
    flow_copy("Shoe material", c("Material A", "Material B")) |>
    flow_outcome("Measured", "Wear")
  expect_length(rep1$leaves, 2)
  expect_false(any(rep1$edges$kind == "measure"))
  # an apply below the copies: still the same units
  rep2 <- flow_start("People", n = 9) |>
    flow_copy("Stool", c("T1", "T2", "T3", "T4")) |>
    flow_apply("Effort", "Rated") |>
    flow_outcome("Measured", "Borg scale")
  expect_length(rep2$leaves, 4)
  # split, then copies: within each sex the same people
  sc <- flow_start("People", n = 20) |>
    flow_split("Sex", c("F", "M"), n = 10) |>
    flow_copy("Condition", c("C1", "C2"))
  expect_length(flow_outcome(sc, "End", "Score")$leaves, 4)
  # but C1 of women and C1 of men are different people
  c1 <- sc$nodes$id[sc$nodes$label == "C1"]
  j <- flow_outcome(sc, "End", "Score", from = c1)
  expect_true(all(j$edges$kind[j$edges$to == max(j$nodes$id)] == "measure"))
  # a split of a pool of copies shares out units: joined
  pc <- flow_start("Records", n = 100) |>
    flow_copy("Labelling", c("Annotator 1", "Annotator 2")) |>
    flow_pool("Adjudication", "Resolved", n = 100) |>
    flow_split("Set", c("Train", "Test"), n = c(80, 20)) |>
    flow_outcome("End", "Scored")
  expect_length(pc$leaves, 1)
  # a pool of two copies and the third copy: the same units again
  pc3 <- flow_start("Records", n = 100) |>
    flow_copy("Labelling", c("A1", "A2", "A3")) |>
    flow_pool("Pair", "A1 and A2 agreed", from = c("A1", "A2"), n = 100)
  expect_true(measured_copies(pc3, pc3$leaves))
  expect_length(flow_outcome(pc3, "End", "Scored")$leaves, 2)
})

test_that("join = TRUE joins copies too and the description says so", {
  d <- flow_start("Boys", n = 10) |>
    flow_copy("Shoe material", c("Material A", "Material B")) |>
    flow_outcome("Measured", "Wear", join = TRUE)
  expect_length(d$leaves, 1)
  expect_match(flow_describe(d),
               "copies of the same units, so each unit is measured in more",
               fixed = TRUE)
})

test_that("the description says the outcome is measured in every branch", {
  txt <- flow_describe(latin())
  expect_match(txt, paste0("Measured: Decrease in volume is measured in every ",
                           "branch, drawn as one box joined from the 8 boxes ",
                           "in the Solution row."), fixed = TRUE)
  # some branches only
  d <- flow_start("A", n = 30) |>
    flow_split("Arm", c("X", "Y", "Z"), n = 10) |>
    flow_outcome("End", "Score", from = c("X", "Y"))
  expect_match(flow_describe(d),
               "End: Score is measured after each of X and Y, drawn as one box joined from them.",
               fixed = TRUE)
  # repeated box names are told apart
  d <- flow_start("A", n = 40) |>
    flow_split("Site", c("S1", "S2"), n = 20) |>
    flow_split("Arm", c("X", "Y"), n = 10) |>
    flow_outcome("End", "Score", from = c(4, 6))
  expect_match(flow_describe(d), "after each of X under S1 and X under S2",
               fixed = TRUE)
})

test_that("flow_check treats a joined outcome as a measurement, not a pool", {
  base <- flow_start("A", n = 20) |>
    flow_split("Arm", c("X", "Y"), n = 10)
  # count of each branch, or all branches together: no finding
  for (n in c(10, 20)) {
    k <- flow_check(flow_outcome(base, "End", "Score", n = n, join = TRUE))
    expect_identical(nrow(k), 0L)
    expect_identical(attr(k, "compared"), 2L)
  }
  # other counts: a note, never a problem
  k <- flow_check(flow_outcome(base, "End", "Score", n = 15, join = TRUE))
  expect_identical(k$level, "note")
  expect_match(k$message, "neither the 20")
  k <- flow_check(flow_outcome(base, "End", "Score", n = 25, join = TRUE))
  expect_identical(k$level, "note")
  expect_match(k$message, "more than the 20")
  # copies joined on purpose: the count of each copy is fine
  cp <- flow_start("Boys", n = 10) |>
    flow_copy("Shoe", c("A", "B")) |>
    flow_outcome("Measured", "Wear", n = 10, join = TRUE)
  expect_identical(nrow(flow_check(cp)), 0L)
})

test_that("data mode: rows are the union of the branches, editable", {
  cells <- data.frame(plot = 1:36, block = rep(paste("B", 1:4), each = 9),
                      dead = rep(c(TRUE, FALSE, FALSE), 12))
  d <- flow_start("Plots", data = cells) |>
    flow_split("Block", by = "block") |>
    flow_outcome("Measured", "Stem volume")
  m <- d$leaves
  expect_length(m, 1)
  expect_identical(d$nodes$n[d$nodes$id == m], "36")
  expect_identical(nrow(flow_data(d, m)), 36L)
  expect_match(flow_describe(d), "its count is of all these branches together",
               fixed = TRUE)
  expect_identical(nrow(flow_check(d)), 0L)
  e <- flow_edit(d, box = m, n = 30, label = "Stem volume (living)")
  expect_identical(e$nodes$n[e$nodes$id == m], "30")
  expect_no_match(flow_describe(e), "all these branches together")
  expect_identical(flow_check(e)$level, "note")
  expect_no_error(flow_layout(e))
  # computed counts that differ keep the branches apart
  uneven <- data.frame(plot = 1:10, block = rep(c("a", "b"), c(4, 6)))
  u <- flow_start("Plots", data = uneven) |>
    flow_split("Block", by = "block") |>
    flow_outcome("Measured", "Yield")
  expect_length(u$leaves, 2)
  # computed events that differ keep them apart too
  ev <- data.frame(id = 1:8, g = rep(c("a", "b"), each = 4),
                   y = c(1, 1, 0, 0, 1, 0, 0, 0))
  v <- flow_start("All", data = ev, events = ~ y == 1) |>
    flow_split("G", by = "g") |>
    flow_outcome("End", "Y")
  expect_length(v$leaves, 2)
  ev$y <- c(1, 0, 0, 0, 1, 0, 0, 0)
  v <- flow_start("All", data = ev, events = ~ y == 1) |>
    flow_split("G", by = "g") |>
    flow_outcome("End", "Y")
  expect_identical(unname(v$events[as.character(v$leaves)]), "2")
})

test_that("joining several starting streams lays out cleanly", {
  d <- flow_start(c("Site North", "Site South"), n = c(30, 30)) |>
    flow_split("Arm", c("Drug", "Placebo"), n = 15) |>
    flow_outcome("Measured", "Blood pressure")
  m <- d$leaves
  expect_length(m, 1)
  expect_true(is.na(d$nodes$anchor[d$nodes$id == m]))
  lay <- flow_layout(d)
  bx <- lay$boxes
  s <- bx[bx$stage == "Arm", ]
  expect_equal(bx$x[bx$id == m], mean(range(s$x)))
  expect_identical(boxes_overlap(all_boxes(lay)), 0)
  expect_identical(crossings(lay), 0)
})

test_that("validation knows the measurement line", {
  d <- latin()
  bad <- d
  bad$edges$kind[1] <- "wiggle"
  expect_error(validate_studydesignflow(bad), "Unknown kind of line: wiggle")
  bad <- d
  m <- d$leaves
  keep <- which(d$edges$to == m)[1]
  bad$edges <- bad$edges[d$edges$to != m | seq_len(nrow(d$edges)) == keep, ]
  expect_error(validate_studydesignflow(bad), "at least two boxes")
})
