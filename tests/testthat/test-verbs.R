test_that("flow_start makes a one-box flow", {
  d <- flow_start("Cultures", n = 100)
  expect_s3_class(d, "dataflow")
  expect_equal(nrow(d$nodes), 1)
  expect_equal(d$nodes$n, "100")
  expect_equal(d$stages$row_label, "")
  expect_equal(flow_start("X", stage = "Root")$stages$row_label, "Root")
})

test_that("counts are drawn as given, never computed", {
  d <- flow_start("All", n = 1234567) |>
    flow_split("Arm", c("A", "B"), n = c("25-30", "999"))
  expect_equal(d$nodes$n, c("1,234,567", "25-30", "999"))
  # children's counts need not add up to the parent's
  d2 <- flow_start("All", n = 10) |> flow_split("S", c("a", "b"), n = 50)
  expect_equal(d2$nodes$n, c("10", "50", "50"))
})

test_that("flow_split applies to every current box and recycles n", {
  d <- flow_start("C", n = 100) |>
    flow_split("Exp", c("E1", "E2"), n = 50) |>
    flow_split("Batch", c("B1", "B2"), n = 25)
  expect_equal(length(d$leaves), 4)
  expect_equal(d$nodes$label[match(d$leaves, d$nodes$id)],
               c("B1", "B2", "B1", "B2"))
  expect_equal(d$nodes$n[d$nodes$stage == 3], rep("25", 4))
  expect_true(all(d$edges$kind == "arrow"))
})

test_that("flow_split with a list gives different children per box", {
  d <- flow_start("C") |>
    flow_split("Exp", c("E1", "E2")) |>
    flow_split("Type", list("Type A", c("Type B", "Type C")), n = 5)
  kids <- d$nodes[d$nodes$stage == 3, ]
  expect_equal(kids$label, c("Type A", "Type B", "Type C"))
  expect_equal(d$nodes$label[match(kids$anchor, d$nodes$id)],
               c("E1", "E2", "E2"))
  expect_error(
    flow_start("C") |> flow_split("Exp", c("E1", "E2")) |>
      flow_split("Type", list("only one")),
    "one element per box"
  )
})

test_that("bad n lengths are refused with a clear message", {
  expect_error(
    flow_start("C") |> flow_split("S", c("a", "b", "c"), n = c(1, 2)),
    "boxes to fill"
  )
})

test_that("flow_apply with `over` brackets each group into one box", {
  d <- flow_example("lab")
  cond_stage <- d$stages$id[d$stages$name == "Condition"]
  cond <- d$nodes[d$nodes$stage == cond_stage, ]
  expect_equal(nrow(cond), 2)
  for (id in cond$id) {
    e <- d$edges[d$edges$to == id, ]
    expect_equal(nrow(e), 6)
    expect_true(all(e$kind == "bracket"))
  }
  # the six sources of the first bracket all sit under Ex. 1
  src <- d$edges$from[d$edges$to == cond$id[1]]
  ex1 <- d$nodes$id[d$nodes$label == "Ex. 1"]
  expect_true(all(vapply(src, function(s) ex1 %in% node_ancestors(d, s), NA)))
})

test_that("flow_apply without `over` adds one box per current box", {
  d <- flow_example("lab")
  task <- d$nodes$id[d$nodes$label == "2 tasks each\n(per condition)"]
  expect_length(task, 2)
  expect_true(all(d$edges$kind[d$edges$to %in% task] == "arrow"))
})

test_that("`from` targets specific boxes", {
  d <- flow_start("C") |>
    flow_split("Exp", c("E1", "E2")) |>
    flow_split("Sub", c("x", "y"), from = "E2")
  expect_equal(d$nodes$label[match(d$leaves, d$nodes$id)], c("E1", "x", "y"))
  expect_error(flow_start("C") |> flow_split("S", "a", from = "nope"),
               "matched none")
  expect_error(flow_start("C") |> flow_split("S", "a", from = 99),
               "No box with id")
})

test_that("stage names must be unique and `over` must exist", {
  d <- flow_start("C") |> flow_split("S", c("a", "b"))
  expect_error(flow_split(d, "S", "z"), "already a stage")
  expect_error(flow_apply(d, "T", "t", over = "Nope"), "no stage")
})

test_that("display = 'summary' is refused until it is built", {
  expect_error(flow_start("C") |> flow_split("S", "a", display = "summary"),
               "not available yet")
})

test_that("validator catches broken objects", {
  d <- flow_example("lab")
  bad <- d
  bad$stages$type[2] <- "teleport"
  expect_error(validate_dataflow(bad), "Unknown stage type")
  bad <- d
  bad$edges <- bad$edges[bad$edges$to != 2, ]
  expect_error(validate_dataflow(bad), "no incoming arrow")
  bad <- d
  bad$edges$to[1] <- 999L
  expect_error(validate_dataflow(bad), "does not exist")
  expect_error(validate_dataflow(list()), "dataflow object")
})

test_that("flow_nodes lists boxes and leaves", {
  n <- flow_nodes(flow_example("lab"))
  expect_equal(nrow(n), 27)
  expect_equal(sum(n$leaf), 2)
  expect_equal(n$label[n$leaf], c("600 each", "600 each"))
})
