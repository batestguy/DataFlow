# Copies: every new box gets all the units of the box above

labelled <- function() {
  flow_start("Records", n = 2000) |>
    flow_exclude("Removed", n = 40, reasons = c("Duplicate" = 25, "Empty" = 15)) |>
    flow_copy("Labelling", c("Annotator 1", "Annotator 2"))
}

test_that("copies take what is left in the box above", {
  d <- labelled()
  nd <- flow_nodes(d)
  expect_equal(nd$n[nd$stage == "Labelling"], c("1,960", "1,960"))
  expect_equal(d$stages$type[d$stages$name == "Labelling"], "copy")
  expect_equal(d$leaves, nd$id[nd$stage == "Labelling"])
  # without exclusions the count is kept as typed, even if not a number
  x <- flow_start("Boys", n = "about 10") |> flow_copy("Shoe", c("A", "B"))
  expect_equal(x$nodes$n[2:3], c("about 10", "about 10"))
  # with exclusions and a count that is not a number, there is no count
  x <- flow_start("Boys", n = "about 10") |> flow_exclude("Out", n = 2) |>
    flow_copy("Shoe", c("A", "B"))
  expect_true(all(is.na(x$nodes$n[2:3])))
  # no count above: no count
  x <- flow_start("Boys") |> flow_copy("Shoe", c("A", "B"))
  expect_true(all(is.na(x$nodes$n[2:3])))
})

test_that("typed counts, notes, events, lists of labels and from work", {
  x <- flow_start("Records", n = 100) |>
    flow_copy("Labelling", c("A", "B"), n = c(100, 97), note = "blind",
              events = c(10, 9))
  expect_equal(x$nodes$n[2:3], c("100", "97"))
  expect_equal(x$nodes$note[2:3], c("blind", "blind"))
  expect_equal(unname(x$events[c("2", "3")]), c("10", "9"))
  y <- flow_start("P", n = 20) |>
    flow_split("Site", c("North", "South"), n = c(12, 8)) |>
    flow_copy("Raters", list(c("R1", "R2"), "R3"))
  expect_equal(y$nodes$n[4:6], c("12", "12", "8"))
  z <- flow_start("P", n = 20) |>
    flow_split("Site", c("North", "South"), n = c(12, 8)) |>
    flow_copy("Raters", c("R1", "R2"), from = "South")
  expect_equal(z$nodes$n[4:5], c("8", "8"))
  expect_true(2 %in% z$leaves)
})

test_that("in a flow with data, copies carry all rows left", {
  d <- data.frame(id = 1:50, ok = rep(c(TRUE, FALSE), c(45, 5)),
                  y = rep(0:1, 25))
  x <- flow_start("Records", data = d, events = ~ y == 1) |>
    flow_exclude("Removed", where = ~ !ok) |>
    flow_copy("Labelling", c("A", "B")) |>
    flow_pool("Adjudication", "Resolved")
  nd <- flow_nodes(x)
  expect_equal(nd$n, c("50", "45", "45", "45"))
  expect_equal(flow_data(x, 2), d[1:45, ], ignore_attr = TRUE)
  expect_equal(flow_data(x, 3), flow_data(x, 2))
  expect_equal(unname(x$events[c("2", "3", "4")]),
               rep(format_n(sum(d$y[1:45] == 1)), 3))
  expect_error(flow_start("R", data = d) |> flow_copy("L", c("A", "B"), n = 5),
               "only for boxes made without data")
  # typed events win over computed ones
  t <- flow_start("Records", data = d, events = ~ y == 1) |>
    flow_copy("Labelling", c("A", "B"), events = 3)
  expect_equal(unname(t$events[c("2", "3")]), c("3", "3"))
  expect_true(all(2:3 %in% t$events_typed))
  # counting distinct ids
  dc <- data.frame(pid = rep(1:10, each = 2))
  c2 <- flow_start("Visits", data = dc, count = "pid") |>
    flow_copy("Readers", c("A", "B"))
  expect_equal(c2$nodes$n, c("10", "10", "10"))
})

test_that("copied boxes say so in the picture, above any note", {
  lay <- flow_layout(labelled() |> flow_copy("Again", "C", note = "checked"))
  b <- lay$boxes
  expect_equal(b$note[b$label %in% c("Annotator 1", "Annotator 2")],
               rep("the same 1,960 as above", 2))
  expect_equal(b$note[b$label == "C"], rep("the same 1,960 as above\nchecked", 2))
  expect_true(is.na(b$note[b$label == "Records"]))
  none <- flow_start("B") |> flow_copy("S", c("A", "B"))
  expect_equal(flow_layout(none)$boxes$note[2], "copied from the box above")
  # a count that is not what is left above does not claim to be the same
  odd <- flow_start("Records", n = 100) |>
    flow_copy("L", c("A", "B"), n = c(100, 97))
  expect_equal(flow_layout(odd)$boxes$note[2:3],
               c("the same 100 as above", "copied from the box above"))
  expect_equal(flow_layout(flow_edit(odd, box = 2, n = 90))$boxes$note[2],
               "copied from the box above")
  off <- flow_layout(labelled(), copy_format = NA)
  expect_true(all(is.na(off$boxes$note)))
  expect_error(flow_layout(labelled(), copy_format = 1), "one piece of text")
  # the line is inside its box
  on <- flow_layout(labelled())$boxes
  expect_true(all(on$note_y[2:3] - on$y[2:3] > -on$h[2:3] / 2))
  # flows without copies are unchanged
  for (e in c("trial", "survey", "lab", "review", "factorial", "ml")) {
    expect_equal(flow_layout(flow_example(e))$boxes,
                 flow_layout(flow_example(e), copy_format = NA)$boxes)
  }
})

test_that("the description says each copy receives all the units", {
  expect_match(flow_describe(labelled()),
               "each of Annotator 1 \\(n = 1,960\\) and Annotator 2 \\(n = 1,960\\) receives all the units left in Records")
  shoes <- flow_start("Boys", n = 10) |> flow_copy("Shoe", c("A", "B"))
  expect_match(flow_describe(shoes), "receives all the units in Boys\\.")
  one <- flow_start("Boys", n = 10) |> flow_copy("Again", "A")
  expect_match(flow_describe(one), "A \\(n = 10\\) receives all the units in Boys")
  crs <- flow_start("People", n = 20) |>
    flow_assign("Sequence", c("AB", "BA"), n = 10) |>
    flow_copy("Period", c("Period 1", "Period 2"))
  expect_match(flow_describe(crs),
               "under each of the 2 boxes in the Sequence row, each of Period 1 \\(n = 10 each\\)|under each of the 2 boxes in the Sequence row, each of Period 1 \\(n = 10\\)")
  pooled <- labelled() |> flow_pool("Adjudication", "Resolved", n = 1960)
  expect_match(flow_describe(pooled), "each unit counted once")
  plain <- flow_start("P", n = 10) |> flow_split("S", c("A", "B"), n = 5) |>
    flow_pool("J", "Joined", n = 10)
  expect_no_match(flow_describe(plain), "counted once")
})

test_that("the number check holds copies to the box they copy", {
  ok <- labelled() |> flow_pool("Adjudication", "Resolved", n = 1960)
  expect_equal(nrow(flow_check(ok)), 0)
  bad <- flow_start("Records", n = 2000) |>
    flow_copy("Labelling", c("A", "B"), n = c(2000, 2100)) |>
    flow_pool("Adjudication", "Resolved", n = 4000)
  ch <- flow_check(bad)
  expect_equal(ch$level, c("problem", "note"))
  expect_match(ch$message[1], "more than the 2,000 it is a copy of")
  expect_match(ch$message[2], "more than the 2,000 different units")
  expect_match(ch$message[2], "Fine if the unit changes")
  expect_no_match(flow_describe(bad), "counted once")
  few <- flow_start("Records", n = 2000) |>
    flow_copy("Labelling", c("A", "B"), n = c(2000, 1990)) |>
    flow_pool("Adjudication", "Resolved", n = 1980)
  ch <- flow_check(few)
  expect_equal(ch$level, c("problem", "note"))
  expect_match(ch$message[1], "hold at least 2,000 different units")
  expect_match(ch$message[2], "Fine if not every unit reached it")
  # the same counts made with a split are still a note that points to copies
  sp <- flow_start("Records", n = 2000) |> flow_split("L", c("A", "B"), n = 2000)
  expect_match(flow_check(sp)$message, "flow_copy\\(\\) draws that")
  # copies after an exclusion: what is left
  ex <- flow_start("Records", n = 2000) |> flow_exclude("Out", n = 40) |>
    flow_copy("L", c("A", "B"), n = 2000)
  expect_match(flow_check(ex)$message[1], "1,960 \\(2,000 minus 40 excluded\\)")
  # copies computed from data are trusted
  d <- data.frame(id = 1:20)
  dd <- flow_start("R", data = d) |> flow_copy("L", c("A", "B")) |>
    flow_pool("J", "Joined")
  expect_equal(nrow(flow_check(dd)), 0)
  # a copy edited by hand is compared again
  ed <- flow_edit(dd, box = 2, n = 25)
  expect_match(flow_check(ed)$message[1], "more than the 20 it is a copy of")
})

test_that("pools of copies further down are recognised", {
  x <- flow_start("Records", n = 100) |>
    flow_copy("Labelling", c("A", "B")) |>
    flow_apply("Cleaned", "Cleaned labels", n = 100) |>
    flow_pool("Adjudication", "Resolved", n = 100)
  expect_equal(nrow(flow_check(x)), 0)
  expect_match(flow_describe(x), "each unit counted once")
  # a pool of copies of different boxes is an ordinary pool
  y <- flow_start("P", n = 20) |>
    flow_split("Site", c("North", "South"), n = 10) |>
    flow_copy("Raters", "R1") |>
    flow_pool("All", "All ratings", n = 20)
  expect_equal(nrow(flow_check(y)), 0)
  expect_no_match(flow_describe(y), "counted once")
})

test_that("copies draw without overlaps and render", {
  x <- flow_start("People", n = 20) |>
    flow_assign("Sequence", c("AB", "BA"), n = 10) |>
    flow_copy("Period", c("Period 1", "Period 2")) |>
    flow_exclude("Missed", n = 1)
  b <- flow_layout(x)$boxes
  for (i in seq_len(nrow(b))) for (j in seq_len(nrow(b))) if (i < j) {
    apart <- abs(b$x[i] - b$x[j]) >= (b$w[i] + b$w[j]) / 2 - 1e-9 ||
      abs(b$y[i] - b$y[j]) >= (b$h[i] + b$h[j]) / 2 - 1e-9
    expect_true(apart)
  }
  expect_true(ggplot2::is_ggplot(ggplot2::autoplot(x)))
})

test_that("the copies diagram looks right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  x <- labelled() |> flow_pool("Adjudication", "Disagreements resolved")
  vdiffr::expect_doppelganger("copies", autoplot(x))
})

test_that("pools mixing copies with other boxes are bounded correctly", {
  # copies of one site plus the other site: 50 + 50
  a <- flow_start("P", n = 100) |> flow_split("S", c("N", "S"), n = 50) |>
    flow_copy("R", c("R1", "R2"), from = "N") |>
    flow_pool("J", "Joined", n = 100, groups = list(c("R1", "R2", "S")))
  expect_equal(nrow(flow_check(a)), 0)
  # two raters at each of two sites, pooled across sites: 10 + 10
  b <- flow_start("P", n = 20) |>
    flow_split("Site", c("North", "South"), n = 10) |>
    flow_copy("Raters", c("R1", "R2")) |> flow_pool("All", "All", n = 20)
  expect_equal(nrow(flow_check(b)), 0)
  expect_match(flow_describe(b), "each unit counted once")
  b40 <- flow_edit(b, box = 8, n = 40)
  expect_equal(flow_check(b40)$level, "note")
  b5 <- flow_edit(b, box = 8, n = 5)
  expect_match(flow_check(b5)$message, "at least 20 different units")
  # stacking periods into person-periods is a note, not a problem
  pp <- flow_start("People", n = 20) |> flow_copy("Period", c("P1", "P2")) |>
    flow_pool("All", "Person-periods", n = 40)
  expect_equal(flow_check(pp)$level, "note")
  expect_no_match(flow_describe(pp), "counted once")
  # copies of copies are still copies of the same units
  nest <- flow_start("P", n = 100) |> flow_copy("R", c("A", "B")) |>
    flow_copy("R2", c("X", "Y"))
  j <- flow_pool(nest, "J", "Joined", n = 400, groups = list(c(4, 6)))
  expect_match(flow_check(j)$message, "more than the 100 different units")
  j2 <- flow_pool(nest, "J", "Joined", n = 100, groups = list(c(4, 5)))
  expect_equal(nrow(flow_check(j2)), 0)
  # a plain split pooled back is unchanged
  sp <- flow_start("P", n = 20) |> flow_split("S", c("A", "B"), n = 10) |>
    flow_pool("J", "J", n = 30)
  expect_match(flow_check(sp)$message, "more than the 20 joined")
})

test_that("summary boxes cannot be copied", {
  expect_error(flow_start("P", n = 100) |>
                 flow_split("C", c("a", "b", "c"), n = 30, display = "summary") |>
                 flow_copy("R", c("R1", "R2")),
               "summary box stands for several boxes")
})

test_that("by = gives each copy its own rows of the same units", {
  stools <- data.frame(person = rep(1:9, each = 4),
                       stool = rep(c("T1", "T2", "T3", "T4"), 9),
                       effort = 1:36)
  expect_no_warning(
    x <- flow_start("People", data = stools, count = "person") |>
      flow_copy("Stool type", by = "stool")
  )
  expect_equal(x$nodes$label[-1], c("T1", "T2", "T3", "T4"))
  expect_equal(x$nodes$n, rep("9", 5))
  expect_equal(flow_data(x, 2)$effort, stools$effort[stools$stool == "T1"],
               ignore_attr = TRUE)
  expect_equal(flow_layout(x)$boxes$note[2], "the same 9 as above")
  expect_equal(nrow(flow_check(x)), 0)
  # named labels fix order and text
  y <- flow_start("People", data = stools, count = "person") |>
    flow_copy("Stool", c("Fourth" = "T4", "First" = "T1", "T2", "T3"),
              by = "stool")
  expect_equal(y$nodes$label[-1], c("Fourth", "First", "T2", "T3"))
  expect_error(flow_start("P", data = stools) |>
                 flow_copy("S", by = "stool", n = 9), "not both")
  expect_error(flow_start("P", n = 3) |> flow_copy("S"), "Give the `labels`")
  # without count = the copies count rows, and the line says so honestly
  z <- flow_start("Rows", data = stools) |> flow_copy("Stool", by = "stool")
  expect_equal(flow_layout(z)$boxes$note[2], "copied from the box above")
})
