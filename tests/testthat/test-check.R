problems <- function(x) {
  k <- flow_check(x)
  k$message[k$level == "problem"]
}
notes <- function(x) {
  k <- flow_check(x)
  k$message[k$level == "note"]
}

test_that("the built-in flow examples have no problems", {
  for (e in c("lab", "trial", "survey", "review", "factorial", "trial_data",
              "ml")) {
    expect_length(problems(flow_example(e)), 0)
  }
  # households to women: a change of unit, noted and not a problem
  expect_match(notes(flow_example("survey")), "unit changes")
  expect_length(notes(flow_example("trial")), 0)
})

test_that("a clean flow reports what was compared", {
  k <- flow_check(flow_example("trial"))
  expect_s3_class(k, "studydesignflow_check")
  expect_equal(nrow(k), 0)
  expect_gt(attr(k, "compared"), 5)
  expect_output(print(k), "0 problems, 0 notes")
  expect_output(print(k), "does not show they are right")
})

test_that("split, assignment and reason errors are found", {
  d <- flow_start("Assessed", n = 500) |>
    flow_exclude("Excluded", n = 100,
                 reasons = c("Not eligible" = 60, "Declined" = 30)) |>
    flow_split("Randomised", "Randomised", n = 400) |>
    flow_assign("Allocation", c("Drug", "Placebo"), n = c(200, 190))
  p <- problems(d)
  expect_length(p, 2)
  expect_match(p[1], "add up to 90, not 100")
  expect_match(p[2], "add up to 390, not 400")
  expect_output(print(flow_check(d)), "Problems")
})

test_that("what is left after exclusions is the number compared", {
  base <- flow_start("A", n = 1500) |> flow_exclude("Out", n = 300)
  expect_length(problems(base |> flow_apply("Next", "B", n = 1200)), 0)
  expect_match(problems(base |> flow_apply("Next", "B", n = 1300)),
               "1,200 (1,500 minus 300 excluded)", fixed = TRUE)
  expect_match(problems(base |> flow_apply("Next", "B", n = 1100)), "has 1,100")
  # two exclusion steps on one box
  two <- base |> flow_exclude("Out 2", n = 200) |> flow_outcome("End", "E", n = 1000)
  expect_length(problems(two), 0)
  # more than the whole box: the unit may have changed
  expect_length(problems(base |> flow_apply("Next", "Visits", n = 4000)), 0)
  expect_match(notes(base |> flow_apply("Next", "Visits", n = 4000)), "4,000")
})

test_that("more excluded than there were is a problem", {
  d <- flow_start("A", n = 50) |> flow_exclude("Out", n = 60)
  expect_match(problems(d), "More are excluded than there are")
})

test_that("missing and worded counts are left out, and counted", {
  d <- flow_start("A", n = "about 50") |>
    flow_split("B", c("x", "y"), n = c(20, 30)) |>
    flow_apply("C", "z")
  k <- flow_check(d)
  expect_equal(nrow(k), 0)
  expect_gt(attr(k, "not_compared"), 0)
  expect_output(print(k), "not possible")
  # some counts missing, but those given are already too many
  e <- flow_start("A", n = 50) |> flow_split("B", c("x", "y"), n = c(60, NA))
  expect_match(problems(e), "already add up to 60")
})

test_that("branches that each have everything are a note, not a problem", {
  d <- flow_start("Records", n = 2000) |>
    flow_split("Annotator", c("One", "Two"), n = 2000) |>
    flow_pool("Compared", "Both", n = 2000)
  expect_length(problems(d), 0)
  expect_length(notes(d), 2)
  expect_match(notes(d)[1], "every branch gets all")
})

test_that("pools: too few is a problem, too many a note", {
  base <- flow_start(c("A", "B"), n = c(40, 60))
  expect_length(problems(base |> flow_pool("P", "All", n = 100)), 0)
  expect_match(problems(base |> flow_pool("P", "All", n = 90)), "add up to 100")
  expect_length(problems(base |> flow_pool("P", "Extracts", n = 640)), 0)
  expect_match(notes(base |> flow_pool("P", "Extracts", n = 640)), "more than")
})

test_that("fewer in the next box without an exclusion is a note", {
  d <- flow_start("A", n = 55) |> flow_outcome("End", "Improved", n = 38)
  expect_length(problems(d), 0)
  expect_match(notes(d), "no exclusion is drawn")
})

test_that("sampling steps are not compared, larger selections are noted", {
  d <- flow_start("Areas", n = 9200) |>
    flow_cluster("Clusters", 20, summary = "{k} areas") |>
    flow_select("Households", "25 per area", n = 500)
  expect_length(problems(d), 0)
  expect_length(problems(flow_start("F", n = 100) |>
                           flow_select("S", "Sampled", n = 10)), 0)
  expect_match(notes(flow_start("F", n = 100) |>
                       flow_select("S", "Sampled", n = 110)), "more than")
})

test_that("events are compared", {
  d <- flow_start("Dev", n = 900, events = 135) |>
    flow_partition(labels = c("Train", "Test"), n = c(700, 200),
                   events = c(100, 30))
  expect_match(problems(d), "events under \"Dev\" add up to 130, not 135")
  expect_warning(e <- flow_start("Dev", n = 9, events = 12), "between 0 and n")
  expect_match(problems(e), "12 events but only 9 units")
})

test_that("summary boxes and labels that repeat do not confuse the check", {
  d <- flow_start("A", n = 100) |>
    flow_split("Arm", c("x", "y"), n = 50) |>
    flow_split("Half", c("h1", "h2"), n = c(25, 25, 25, 20))
  expect_match(problems(d), "under \"y\" add up to 45, not 50")
  s <- flow_start("A", n = 100) |>
    flow_split("Batch", 4, n = 25, display = "summary")
  expect_length(problems(s), 0)
})

test_that("counts from data are right until they are edited", {
  set.seed(1)
  people <- data.frame(id = 1:60, age = rep(10:69, 1),
                       arm = rep(c("a", "b"), 30))
  d <- flow_start("All", data = people) |>
    flow_exclude("Under 18", reasons = list("Under 18" = ~ age < 18)) |>
    flow_assign("Arm", by = "arm")
  expect_equal(nrow(flow_check(d)), 0)
  nd <- flow_nodes(d)
  e <- flow_edit(d, nd$id[nd$label == "b"], n = 30)
  expect_match(problems(e), "not 52 (60 minus 8 excluded)", fixed = TRUE)
})

test_that("people counted in two branches are not reported in data mode", {
  visits <- data.frame(id = c(1, 1, 2, 3), arm = c("a", "b", "a", "b"))
  d <- suppressWarnings(flow_start("All", data = visits, count = "id") |>
                          flow_split("Arm", by = "arm"))
  expect_equal(nrow(flow_check(d)), 0)
})

test_that("the check changes nothing and refuses what it cannot check", {
  d <- flow_example("trial")
  before <- d
  flow_check(d)
  expect_identical(d, before)
  expect_error(flow_check(1), "must be a studydesignflow")
  expect_error(flow_check(flow_folds(n = 50, k = 5)), "fold grids")
})

test_that("subsets of the result still print", {
  k <- flow_check(flow_start("A", n = 55) |> flow_outcome("E", "x", n = 38))
  expect_output(print(k[, c("level", "box")]), "note")
})

test_that("notes of one sort in a row are given once, problems every time", {
  d <- flow_start("Batches", n = 9) |>
    flow_split("Recipe", c("A", "B", "C"), n = 3) |>
    flow_apply("Portions", "6 per batch", n = 18)
  expect_length(notes(d), 1)
  expect_match(notes(d), "The same in 2 more places in this row")
  e <- flow_start("A", n = 100) |> flow_split("S", c("x", "y"), n = 50) |>
    flow_split("T", c("a", "b"), n = 20)
  expect_length(problems(e), 2)
})

test_that("one box that counts another unit is a note; a split is a problem", {
  d <- flow_start("Trainees", n = 3) |>
    flow_assign("Clients", "4 clients per trainee", n = 12)
  expect_length(problems(d), 0)
  expect_match(notes(d), "unit changes")
  e <- flow_start("Panellists", n = 12) |>
    flow_split("Recipe", c("A", "B", "C", "D"), n = 6)
  expect_match(problems(e), "in more than one box")
})

test_that("typed and edited events are compared in data mode", {
  v <- data.frame(id = 1:20, arm = rep(c("a", "b"), 10),
                  ev = rep(c(1, 0, 0, 1), 5))
  base <- flow_start("All", data = v, events = ~ ev == 1)
  clean <- flow_split(base, "Arm", by = "arm")
  expect_equal(nrow(flow_check(clean)), 0)
  e <- suppressWarnings(flow_edit(clean, 1, events = 99))
  expect_match(problems(e), "99 events but only 20 units", all = FALSE)
  expect_match(problems(e), "add up to 10, not 99", all = FALSE)
  typed <- flow_split(base, "Arm", by = "arm", events = c(3, 4))
  expect_match(problems(typed), "events under \"All\" add up to 7, not 10")
})

test_that("part of the findings is never called a clean result", {
  k <- flow_check(flow_start("A", n = 100) |>
                    flow_exclude("Out", n = 10, reasons = c(a = 8, b = 6)))
  for (part in list(k[k$level == "note", ], head(k, 0), k[0, ], subset(k, FALSE))) {
    expect_false(inherits(part, "studydesignflow_check"))
    expect_false(any(grepl("fit each other", capture.output(print(part)))))
  }
  # a tool that keeps the class and the attributes
  part <- structure(k[0, ], class = class(k), compared = 2L, not_compared = 0L,
                    found = 1L)
  expect_false(any(grepl("fit each other", capture.output(print(part)))))
  expect_match(problems(flow_start("A", n = 100) |>
                          flow_exclude("Out", n = 10,
                                       reasons = c(a = 8, b = 6))),
               "more than one reason")
})

test_that("pools, selections and brackets are held to what is left", {
  two <- flow_start(c("Db", "Reg"), n = c(1000, 500)) |>
    flow_exclude("Dup", n = c(100, 50))
  expect_match(problems(flow_pool(two, "P", "All", n = 1500)),
               "more than the 1,350 (1,500 minus 150 excluded) joined in it",
               fixed = TRUE)
  expect_length(problems(flow_pool(two, "P", "All", n = 1350)), 0)
  expect_match(problems(flow_pool(two, "P", "All", n = 1300)),
               "1,350 (1,500 minus", fixed = TRUE)
  # more than everything above: the unit may have changed
  expect_length(problems(flow_pool(two, "P", "Extracts", n = 4000)), 0)
  one <- flow_start("A", n = 1000) |> flow_exclude("Out", n = 100)
  expect_match(problems(flow_select(one, "S", "x", n = 1000)),
               "more than the 900 (1,000 minus 100", fixed = TRUE)
  expect_equal(nrow(flow_check(flow_select(one, "S", "x", n = 900))), 0)
  br <- flow_start("C", n = 100) |> flow_split("Exp", c("E1", "E2"), n = 50) |>
    flow_split("Comp", c("A", "B"), n = 25) |>
    flow_exclude("Out", n = c(5, 0, 0, 5)) |>
    flow_apply("Cond", "9 each", over = "Exp", n = c(40, 40))
  expect_length(notes(br), 0)
  expect_length(problems(br), 2)
  expect_match(problems(br), "has 40, not 45 (50 minus 5 excluded)", fixed = TRUE)
})

test_that("one box with fewer and no exclusion is a note in every verb", {
  base <- flow_start("A", n = 500)
  expect_length(problems(flow_split(base, "R", "Randomised", n = 400)), 0)
  expect_match(notes(flow_split(base, "R", "Randomised", n = 400)),
               "no exclusion is drawn")
  expect_match(problems(flow_exclude(base, "Out", n = 50) |>
                          flow_split("R", "Randomised", n = 400)), "not 450")
})
