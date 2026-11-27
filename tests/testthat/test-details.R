# Step details: who, with what, when, where and how (part 7c)

all5 <- function() {
  step_details(who = "trained enumerators",
               tool = "tablet questionnaire (CSPro)",
               when = "March to May 2025", where = "Kaduna State",
               how = "two visits per household")
}

village <- function(details = TRUE) {
  d <- function(v) if (details) v
  flow_start("Villages sampled", n = 40, unit = "villages") |>
    flow_split("Village", c("Village A", "Village B"), n = 20) |>
    flow_select("Households", "20 per village", n = 400, unit = "households",
                details = d(all5())) |>
    flow_exclude("Refused", n = 12,
                 reasons = c("Not at home" = 7, "Declined" = 5),
                 details = d(step_details(who = "field supervisors"))) |>
    flow_select("People", "All members listed", n = 1600, unit = "people",
                details = d(list(NULL, step_details(tool = "paper forms"))))
}

test_that("step_details() keeps the fields given, in a fixed order", {
  d <- step_details(how = "two visits", who = "enumerators")
  expect_s3_class(d, "studydesignflow_details")
  expect_identical(names(d), c("who", "how"))
  expect_identical(d$who, "enumerators")
  expect_output(print(d), "Who:\\s+enumerators")
  expect_output(print(d), "How:\\s+two visits")
  expect_identical(names(all5()), c("who", "tool", "when", "where", "how"))
})

test_that("step_details() refuses empty or malformed fields", {
  expect_error(step_details(), "at least one")
  expect_error(step_details(who = ""), "`who` must be one piece of text")
  expect_error(step_details(who = "  "), "`who`")
  expect_error(step_details(tool = c("a", "b")), "`tool`")
  expect_error(step_details(when = NA_character_), "`when`")
  expect_error(step_details(where = 3), "`where`")
  expect_error(step_details(how = list("x")), "`how`")
  expect_error(step_details(whom = "x"), "unused argument")
})

test_that("every verb that makes boxes takes details", {
  sd <- step_details(who = "someone")
  base <- flow_start("All", n = 100)
  made <- list(
    start = flow_start("All", n = 100, details = sd),
    split = flow_split(base, "S", c("a", "b"), details = sd),
    assign = flow_assign(base, "S", c("a", "b"), details = sd),
    cluster = flow_cluster(base, "S", 10, details = sd),
    select = flow_select(base, "S", "some", details = sd),
    apply = flow_apply(base, "S", "done", details = sd),
    outcome = flow_outcome(base, "S", "measured", details = sd),
    pool = flow_pool(flow_start(c("A", "B")), "S", "together", details = sd),
    copy = flow_copy(base, "S", c("a", "b"), details = sd),
    partition = flow_partition(base, "S", c("Training", "Test"),
                               details = sd),
    exclude = flow_exclude(base, "Out", n = 3, details = sd)
  )
  for (nm in names(made)) {
    x <- made[[nm]]
    sid <- x$stages$id[nrow(x$stages)]
    expect_identical(names(x$details), "steps", info = nm)
    expect_identical(names(x$details$steps), as.character(sid), info = nm)
    expect_identical(x$details$steps[[1]], sd, info = nm)
  }
})

test_that("a flow without details has no details field", {
  d <- village(details = FALSE)
  expect_null(d$details)
  expect_false("details" %in% names(unclass(d)))
  expect_null(flow_layout(d)$key)
  expect_false("detail" %in% names(flow_layout(d)$boxes))
})

test_that("details are stored per step, per box and per exclusion box", {
  sd <- step_details(who = "a")
  bd <- step_details(tool = "b")
  x <- flow_start("All", n = 10) |>
    flow_split("Site", c("North", "South", "North"),
               details = list(step = sd, North = bd))
  sid <- as.character(x$stages$id[x$stages$name == "Site"])
  expect_identical(x$details$steps[[sid]], sd)
  north <- x$nodes$id[x$nodes$label == "North"]
  expect_setequal(names(x$details$boxes), as.character(north))
  # one per new box, NULL for none
  y <- flow_start("All", n = 10) |>
    flow_split("Site", c("North", "South"), details = list(NULL, bd))
  expect_null(y$details$steps)
  expect_identical(names(y$details$boxes),
                   as.character(y$nodes$id[y$nodes$label == "South"]))
  # exclusion boxes
  z <- flow_start("All", n = 10) |>
    flow_split("Site", c("North", "South")) |>
    flow_exclude("Out", n = 1, details = list(NULL, bd))
  expect_identical(names(z$details), "side")
  expect_identical(names(z$details$side), as.character(z$side$id[2]))
  # labels given with `by` in data mode
  df <- data.frame(arm = c("a", "a", "b"))
  w <- flow_start("All", data = df) |>
    flow_split("Arm", by = "arm", details = list(b = bd))
  expect_identical(names(w$details$boxes),
                   as.character(w$nodes$id[w$nodes$label == "b"]))
  expect_identical(flow_data(w, w$nodes$id[w$nodes$label == "b"]),
                   df[3, , drop = FALSE])
})

test_that("wrong shapes of `details` give clear errors", {
  base <- flow_start("All", n = 10)
  expect_error(flow_split(base, "S", c("a", "b"), details = "who"),
               "must be step_details")
  expect_error(flow_split(base, "S", c("a", "b"), details = list()),
               "must be step_details")
  expect_error(flow_split(base, "S", c("a", "b"),
                          details = list(step_details(who = "x"))),
               "list of 1 but this step makes 2 boxes")
  expect_error(flow_split(base, "S", c("a", "b"),
                          details = list(step_details(who = "x"), "y")),
               "Entry 2 of the `details` list")
  expect_error(flow_split(base, "S", c("a", "b"),
                          details = list(c = step_details(who = "x"))),
               "no box with that label")
  expect_error(flow_split(base, "S", c("a", "b"),
                          details = list(a = step_details(who = "x"),
                                         step_details(who = "y"))),
               "Name every entry")
  expect_error(flow_split(base, "S", c("a", "b"),
                          details = list(a = step_details(who = "x"),
                                         a = step_details(who = "y"))),
               "twice")
  expect_error(flow_start("All", details = list(1)), "Entry 1")
})

test_that("validation checks the stored details", {
  x <- village()
  y <- x
  y$details$steps[["99"]] <- step_details(who = "x")
  expect_error(validate_studydesignflow(y), "named by the ids of existing")
  y <- x
  y$details$boxes[[1]] <- list(who = "x")
  expect_error(validate_studydesignflow(y), "must come from")
  y <- x
  y$details$other <- list()
  expect_error(validate_studydesignflow(y), "parts")
  y <- x
  y$details$side[["1"]] <- structure(list(who = ""),
                                     class = "studydesignflow_details")
  expect_error(validate_studydesignflow(y), "must come from")
})

test_that("flow_edit() sets and removes details of steps and boxes", {
  x <- village(details = FALSE)
  sd <- step_details(when = "2025")
  y <- flow_edit(x, stage = "People", details = sd)
  sid <- as.character(x$stages$id[x$stages$name == "People"])
  expect_identical(y$details$steps[[sid]], sd)
  # with a row label in the same call
  y2 <- flow_edit(x, stage = "People", row_label = "Members", details = sd)
  expect_identical(y2$stages$row_label[y2$stages$name == "People"], "Members")
  expect_identical(y2$details, y$details)
  # removing the only details leaves the flow as it was
  expect_identical(flow_edit(y, stage = "People", details = NA), x)
  # boxes: one for all, or one per box; exclusion boxes
  z <- flow_edit(x, box = "All members listed", details = sd)
  expect_length(z$details$boxes, 2)
  ids <- x$nodes$id[x$nodes$label == "Village A" | x$nodes$label == "Village B"]
  z <- flow_edit(x, box = ids, details = list(sd, NA))
  expect_identical(names(z$details$boxes), as.character(ids[1]))
  z <- flow_edit(x, box = 1, excluded = TRUE, details = sd)
  expect_identical(names(z$details$side), "1")
  expect_identical(flow_edit(z, box = 1, excluded = TRUE, details = NA), x)
  # errors
  expect_error(flow_edit(x, stage = "People", n = 3, details = sd),
               "`n` need `box`")
  expect_error(flow_edit(x, stage = "People"), "give `row_label`, `details`")
  expect_error(flow_edit(x, stage = "Nope", details = sd), "no stage")
  expect_error(flow_edit(x, box = 1, details = "x"), "must be step_details")
  expect_error(flow_edit(x, box = ids, details = list(sd, sd, sd)),
               "list of 3")
  expect_error(flow_edit(x, box = "All members listed",
                         details = list(sd, sd)), "ambiguous")
  expect_error(flow_edit(x, details = sd), "Give `box`")
  expect_error(flow_edit(flow_example("cv"), details = sd), "Fold grids")
})

test_that("the description gives the details after their step", {
  desc <- unclass(flow_describe(village()))
  expect_match(desc, paste0(
    "counted in households. Done by trained enumerators, with tablet ",
    "questionnaire \\(CSPro\\), March to May 2025, in Kaduna State; how: two ",
    "visits per household. Refused"))
  expect_match(desc, "Declined \\(5 households\\). Done by field supervisors.")
  expect_match(desc, "For All members listed under 20 per village under ",
               fixed = TRUE)
  expect_match(desc, "Village B: done with paper forms.", fixed = TRUE)
  # any subset of fields reads as a sentence
  one <- function(d) {
    unclass(flow_describe(flow_start("All", n = 1, details = d)))
  }
  expect_match(one(step_details(tool = "a scale")), "Done with a scale\\.$")
  expect_match(one(step_details(how = "by hand.")), "How: by hand\\.$")
  expect_match(one(step_details(when = "2024", where = "Kano")),
               "Done 2024, in Kano\\.$")
  # the alt text carries them
  p <- ggplot2::autoplot(village())
  expect_match(ggplot2::get_alt_text(p), "Done by trained enumerators",
               fixed = TRUE)
  # exclusion box of its own
  x <- flow_start("All", n = 10) |>
    flow_exclude("Out", n = 1, details = list(step_details(who = "a nurse")))
  expect_match(unclass(flow_describe(x)),
               "For Out after All: done by a nurse.", fixed = TRUE)
})

test_that("the key is numbered top to bottom, left to right", {
  lay <- flow_layout(village())
  heads <- lay$key$text[lay$key$face == "bold"]
  expect_identical(heads, c(
    "[1] Households", "[2] Refused",
    "[3] All members listed under 20 per village under Village B"))
  expect_identical(lay$key$text[lay$key$entry == 1][-1], c(
    "Who: trained enumerators", "Tool: tablet questionnaire (CSPro)",
    "When: March to May 2025", "Where: Kaduna State",
    "How: two visits per household"))
  expect_identical(lay$row_labels$detail[!is.na(lay$row_labels$detail)],
                   c("[1]", "[2]"))
  expect_identical(lay$boxes$detail[!is.na(lay$boxes$detail)], 3L)
  expect_match(lay$boxes$text[lay$boxes$detail %in% 3],
               "^All members listed \\[3\\]\n")
  expect_identical(lay$boxes$label, flow_layout(village(FALSE))$boxes$label)
  # boxes in a row from left to right, after the step
  x <- flow_start("All", n = 10) |>
    flow_split("Site", c("North", "South", "East"),
               details = list(step = step_details(who = "a"),
                              East = step_details(who = "b"),
                              North = step_details(who = "c")))
  lay <- flow_layout(x)
  expect_identical(lay$key$text[lay$key$face == "bold"],
                   c("[1] Site", "[2] North", "[3] East"))
  b <- lay$boxes[!is.na(lay$boxes$detail), ]
  expect_true(all(diff(b$x[order(b$detail)]) > 0))
})

test_that("the key and markers overlap nothing and the size includes them", {
  for (show_method in c(TRUE, FALSE)) {
    lay <- flow_layout(village(), show_method = show_method)
    every <- rbind(all_boxes(lay), label_rects(lay))
    expect_identical(boxes_overlap(every), 0)
    expect_identical(crossings(lay), 0)
    # the key is under everything else and left-aligned with the row labels
    expect_true(max(lay$key$ymax) < min(c(lay$boxes$ymin, lay$side$ymin)))
    expect_equal(min(lay$key$xmin), lay$row_labels$x[1])
    expect_true(lay$extent[["ymin"]] < min(lay$key$ymin))
    expect_true(lay$extent[["xmax"]] > max(lay$key$xmax))
    # boxes keep their size
    plain <- flow_layout(village(FALSE), show_method = show_method)
    expect_equal(lay$boxes$h, plain$boxes$h)
    expect_true(lay$size[["height"]] > plain$size[["height"]])
  }
  # the natural size used by flow_save() includes the key
  p <- ggplot2::autoplot(village())
  expect_equal(attr(p, "studydesignflow_size"), flow_layout(village())$size)
})

test_that("long values wrap to the width of the diagram", {
  long <- paste("a long protocol with many words that cannot fit on one",
                "line of a narrow diagram, so it has to wrap onto several",
                "lines of the key under it")
  x <- flow_start("All", n = 10) |>
    flow_split("S", c("a", "b"), details = step_details(how = long))
  lay <- flow_layout(x)
  key <- lay$key
  width <- max(3, max(lay$boxes$xmax) - lay$row_labels$x[1])
  expect_gt(sum(key$entry == 1), 3)
  expect_true(all(key$xmax - key$xmin[1] <= width + 1e-9))
  # continuation lines hang under the value, not under "How:"
  expect_true(all(key$x[-(1:2)] > key$x[2]))
  words <- strsplit(long, " ")[[1]]
  got <- strsplit(sub("^How: ", "", paste(key$text[-1], collapse = " ")),
                  " ")[[1]]
  expect_identical(got, words)
  expect_identical(boxes_overlap(all_boxes(lay)), 0)
})

test_that("a row without a label still shows its marker", {
  x <- flow_start("All", n = 10, details = step_details(who = "a")) |>
    flow_split("S", c("a", "b"), row_label = "",
               details = step_details(who = "b"))
  lay <- flow_layout(x)
  rl <- lay$row_labels
  expect_identical(rl$label, c("", ""))
  expect_identical(rl$detail, c("[1]", "[2]"))
  expect_equal(rl$detail_y, unique(lay$boxes$y))
  expect_identical(boxes_overlap(rbind(all_boxes(lay), label_rects(lay))), 0)
})

test_that("show_details = FALSE draws a flow as if it had no details", {
  x <- village()
  y <- village(details = FALSE)
  expect_identical(flow_layout(x, show_details = FALSE), flow_layout(y))
  expect_identical(
    flow_layout(x, show_details = FALSE, connector = "straight",
                show_method = FALSE),
    flow_layout(y, connector = "straight", show_method = FALSE))
  expect_error(flow_layout(x, show_details = NA), "show_details")
  p <- ggplot2::autoplot(x, show_details = FALSE)
  expect_equal(attr(p, "studydesignflow_size"), flow_layout(y)$size)
})

test_that("details do not change counts or rows in data mode", {
  df <- data.frame(arm = rep(c("a", "b"), c(3, 4)), age = 20:26)
  build <- function(d) {
    flow_start("All", data = df, details = d) |>
      flow_split("Arm", by = "arm", details = d) |>
      flow_select("Older", "Aged 22+", where = ~ age >= 22, details = d)
  }
  x <- build(step_details(who = "a"))
  y <- build(NULL)
  expect_identical(flow_nodes(x), flow_nodes(y))
  expect_identical(x$data, y$data)
  x$details <- NULL
  expect_identical(x, y)
})

test_that("printing mentions the details", {
  expect_output(print(village()), "step details: 2 steps and 1 box")
  expect_failure(expect_output(print(village(FALSE)), "step details"))
})


test_that("key headings use the row label and qualify exclusion boxes", {
  w <- step_details(who = "a nurse")
  d <- flow_start("All", n = 10) |>
    flow_split("Arm", c("a", "b"), n = 5, row_label = "Allocation",
               details = w) |>
    flow_exclude("Out", n = 1, details = list(w, w))
  heads <- flow_layout(d)$key$text[flow_layout(d)$key$face == "bold"]
  expect_identical(heads, c("[1] Allocation", "[2] Out after a",
                            "[3] Out after b"))
  # an edited row label is followed; a hidden one falls back to the name
  e <- flow_edit(d, stage = "Arm", row_label = "Treatment")
  expect_identical(flow_layout(e)$key$text[1], "[1] Treatment")
  h <- flow_edit(d, stage = "Arm", row_label = "")
  expect_identical(flow_layout(h)$key$text[1], "[1] Arm")
})

test_that("flow_edit() refuses named or listed details where unclear", {
  w <- step_details(who = "a nurse")
  y <- flow_start("All", n = 10) |> flow_split("S", c("a", "b"), n = 5)
  expect_error(flow_edit(y, stage = "S", details = list(step = w)),
               "not a named list")
  expect_error(flow_edit(y, box = "a", details = list(step = w)),
               "not a named list")
  expect_error(flow_edit(y, stage = "S", details = list(w, w)),
               "not a list")
  s <- flow_edit(y, stage = "S", details = w)
  expect_identical(s$details$steps[[as.character(s$stages$id[2])]], w)
  expect_identical(flow_edit(s, stage = "S", details = NA)$details, NULL)
})
