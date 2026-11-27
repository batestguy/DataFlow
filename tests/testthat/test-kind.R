# Kinds of step: done to physical things or to data (part 7d)

lab_kind <- function(kinds = TRUE) {
  k <- function(v) if (kinds) v
  flow_start("Samples collected", n = 120, unit = "samples",
             kind = k("physical")) |>
    flow_split("Site", c("Site A", "Site B"), n = 60) |>
    flow_apply("Assay", "ELISA run", kind = k("physical")) |>
    flow_apply("Records", "Records cleaned",
               kind = k(list(step = "data", "Records cleaned" = "data"))) |>
    flow_exclude("Duplicates removed", n = 3, kind = k("data")) |>
    flow_split("Analysis", "Analysed", kind = k(c(NA, "physical")))
}

rect_layers <- function(p) {
  Filter(function(l) inherits(l$geom, "GeomRect"), p$layers)
}

test_that("every verb that makes boxes takes kind, stored for the step", {
  base <- flow_start("All", n = 100)
  made <- list(
    start = flow_start("All", n = 100, kind = "data"),
    split = flow_split(base, "S", c("a", "b"), kind = "data"),
    assign = flow_assign(base, "S", c("a", "b"), kind = "data"),
    cluster = flow_cluster(base, "S", 10, kind = "data"),
    select = flow_select(base, "S", "some", kind = "data"),
    apply = flow_apply(base, "S", "done", kind = "data"),
    outcome = flow_outcome(base, "S", "measured", kind = "data"),
    pool = flow_pool(flow_start(c("A", "B")), "S", "together", kind = "data"),
    copy = flow_copy(base, "S", c("a", "b"), kind = "data"),
    partition = flow_partition(base, "S", c("Training", "Test"),
                               kind = "data"),
    exclude = flow_exclude(base, "Out", n = 3, kind = "data")
  )
  for (nm in names(made)) {
    x <- made[[nm]]
    sid <- x$stages$id[nrow(x$stages)]
    expect_identical(names(x$kind), "steps", info = nm)
    expect_identical(x$kind$steps, stats::setNames("data", sid), info = nm)
  }
})

test_that("a flow without kinds has no kind field, column or key", {
  d <- lab_kind(kinds = FALSE)
  expect_false("kind" %in% names(unclass(d)))
  lay <- flow_layout(d)
  expect_false("kind" %in% names(lay$boxes))
  expect_false("kind" %in% names(lay$side))
  expect_false("kind_key" %in% names(lay))
  expect_length(rect_layers(ggplot2::autoplot(d))[[1]]$aes_params$linetype, 0)
  expect_no_match(flow_describe(d), "done to")
  expect_no_match(capture.output(print(d)), "kinds of step")
})

test_that("kinds resolve: own value, else the step's, never the box above", {
  d <- lab_kind()
  bk <- box_kinds(d)
  lab <- function(l) d$nodes$id[d$nodes$label == l]
  expect_true(all(bk[as.character(lab("Samples collected"))] == "physical"))
  # the split under a physical step has no kind: not taken from above
  expect_true(all(is.na(bk[as.character(lab("Site A"))])))
  expect_true(all(bk[as.character(lab("ELISA run"))] == "physical"))
  expect_true(all(bk[as.character(lab("Records cleaned"))] == "data"))
  an <- d$nodes$id[d$nodes$label == "Analysed"]
  expect_identical(unname(bk[as.character(an)]), c(NA, "physical"))
  sk <- side_kinds(d)
  expect_true(all(sk == "data"))
  # a box's own kind wins over its step's
  e <- flow_edit(d, lab("ELISA run")[2], kind = "data")
  expect_identical(unname(box_kinds(e)[as.character(lab("ELISA run"))]),
                   c("physical", "data"))
  expect_identical(unname(e$kind$boxes[as.character(lab("ELISA run")[2])]),
                   "data")
})

test_that("kind lists: one per box, named by label, step, NA or NULL", {
  base <- flow_start("All", n = 100)
  x <- flow_split(base, "S", c("a", "b", "c"),
                  kind = list("data", NULL, "physical"))
  expect_null(x$kind$steps)
  expect_identical(x$kind$boxes, c("2" = "data", "4" = "physical"))
  x <- flow_split(base, "S", c("a", "b", "c"), kind = c(NA, "data", NA))
  expect_identical(x$kind$boxes, c("3" = "data"))
  x <- flow_split(base, "S", c("a", "b"),
                  kind = c(step = "data", b = "physical"))
  expect_identical(x$kind$steps, c("2" = "data"))
  expect_identical(x$kind$boxes, c("3" = "physical"))
  x <- flow_split(base, "S", c("a", "b"), kind = list(b = "phys"))
  expect_identical(x$kind$boxes, c("3" = "physical"))
  # exclusion boxes, per box
  y <- flow_split(base, "S", c("a", "b")) |>
    flow_exclude("Out", n = 2, kind = list(NULL, "data"))
  expect_identical(y$kind$side, c("2" = "data"))
  expect_identical(unname(side_kinds(y)), c(NA, "data"))
  # all NA stores nothing
  expect_false("kind" %in% names(unclass(
    flow_split(base, "S", c("a", "b"), kind = c(NA, NA)))))
})

test_that("bad kind arguments are refused with clear messages", {
  base <- flow_start("All", n = 100)
  expect_error(flow_start("All", kind = "digital"),
               "must be \"physical\" or \"data\", not \"digital\"")
  expect_error(flow_split(base, "S", c("a", "b"), kind = ""),
               "\"physical\" or \"data\"")
  expect_error(flow_split(base, "S", c("a", "b"), kind = NA_character_),
               "\"physical\" or \"data\"")
  expect_error(flow_split(base, "S", c("a", "b"), kind = 1), "`kind` must")
  expect_error(flow_split(base, "S", c("a", "b"), kind = list()),
               "`kind` must")
  expect_error(flow_split(base, "S", c("a", "b"),
                          kind = list("data", "wrong")),
               "Entry 2 of `kind`")
  expect_error(flow_split(base, "S", c("a", "b"),
                          kind = list("data", "data", "data")),
               "3 entries but this step makes 2 boxes")
  expect_error(flow_split(base, "S", c("a", "b"),
                          kind = list(step = "data", "data")),
               "Name every entry")
  expect_error(flow_split(base, "S", c("a", "b"),
                          kind = c(a = "data", a = "physical")),
               "names \"a\" twice")
  expect_error(flow_split(base, "S", c("a", "b"), kind = c(z = "data")),
               "no box with that label")
  expect_error(flow_split(base, "S", c("a", "b"),
                          kind = step_details(who = "x")), "`kind` must")
  # nothing is built when the argument is wrong
  expect_error(flow_exclude(base, "Out", n = 3, kind = "x"), "not \"x\"")
})

test_that("flow_edit() sets and removes kinds of steps and boxes", {
  d <- lab_kind(kinds = FALSE)
  e <- flow_edit(d, stage = "Records", kind = "data")
  sid <- d$stages$id[d$stages$name == "Records"]
  expect_identical(e$kind$steps, stats::setNames("data", sid))
  e <- flow_edit(e, stage = "Records", kind = NA)
  expect_identical(e, d)
  ids <- d$nodes$id[d$nodes$label == "ELISA run"]
  e <- flow_edit(d, ids, kind = c("data", "physical"))
  expect_identical(unname(e$kind$boxes), c("data", "physical"))
  e <- flow_edit(e, ids, kind = NA)
  expect_identical(e, d)
  e <- flow_edit(d, "Duplicates removed", excluded = TRUE, kind = "data")
  expect_true(all(side_kinds(e) == "data"))
  # with a row label at the same time; labels and counts unchanged
  e <- flow_edit(d, stage = "Site", row_label = "Where", kind = "physical")
  expect_identical(e$nodes, d$nodes)
  expect_identical(e$stages$row_label[2], "Where")
  # errors
  expect_error(flow_edit(d, stage = "Site", kind = c("data", "data")),
               "A step takes one `kind`")
  expect_error(flow_edit(d, ids, kind = c(a = "data")), "not named")
  expect_error(flow_edit(d, ids, kind = c("data", "data", "data")),
               "3 values but there are 2 boxes")
  expect_error(flow_edit(d, ids, kind = "paper"), "not \"paper\"")
  expect_error(flow_edit(d, "ELISA run", kind = c("data", "physical")),
               "ambiguous")
  expect_error(flow_edit(flow_example("cv"), kind = "data"),
               "Fold grids do not take a kind")
})

test_that("stored kinds are validated", {
  d <- lab_kind()
  bad <- d
  bad$kind$steps[1] <- "paper"
  expect_error(validate_studydesignflow(bad), "must be \"physical\" or")
  bad <- d
  bad$kind$boxes <- c("999" = "data")
  expect_error(validate_studydesignflow(bad), "ids of existing boxes")
  bad <- d
  bad$kind$other <- c("1" = "data")
  expect_error(validate_studydesignflow(bad), "parts `steps`")
  # an old object without the field is fine
  old <- d
  old$kind <- NULL
  expect_silent(validate_studydesignflow(old))
})

test_that("the layout has a kind column only when kinds are used", {
  d <- lab_kind()
  lay <- flow_layout(d)
  expect_identical(lay$boxes$kind, unname(box_kinds(d)))
  expect_identical(lay$side$kind, unname(side_kinds(d)))
  # boxes keep their place and size: only the key is added
  plain <- flow_layout(lab_kind(kinds = FALSE))
  cols <- setdiff(names(plain$boxes), "kind")
  expect_identical(lay$boxes[, cols], plain$boxes[, cols])
  expect_identical(lay$segments, plain$segments)
  off <- flow_layout(d, show_kind_key = FALSE)
  expect_null(off$kind_key)
  expect_identical(off$size, plain$size)
  expect_identical(off$extent, plain$extent)
  expect_error(flow_layout(d, show_kind_key = NA), "`show_kind_key`")
})

test_that("the key: one dashed line for data, no line for physical", {
  base <- flow_start("All", n = 100)
  only_data <- flow_split(base, "S", c("a", "b"), kind = "data")
  k <- flow_layout(only_data)$kind_key
  expect_identical(k$kind, "data")
  expect_identical(k$linetype, "dashed")
  expect_match(k$text, "Dashed edge")
  both <- flow_split(base, "S", c("a", "b"), kind = c("data", "physical"))
  k <- flow_layout(both)$kind_key
  expect_identical(k$kind, "data")
  expect_identical(k$linetype, "dashed")
  only_phys <- flow_split(base, "S", c("a", "b"), kind = "physical")
  expect_null(flow_layout(only_phys)$kind_key)
  plain_s <- flow_split(base, "S", c("a", "b"))
  expect_identical(flow_layout(only_phys)$size, flow_layout(plain_s)$size)
  # the key is under the diagram, inside the picture, and grows its height
  lay <- flow_layout(both)
  plain <- flow_layout(flow_split(base, "S", c("a", "b")))
  expect_lt(max(lay$kind_key$ymax), min(lay$boxes$ymin))
  expect_gt(lay$size[["height"]], plain$size[["height"]])
  expect_lt(lay$extent[["ymin"]], min(lay$kind_key$ymin))
  expect_gt(lay$extent[["xmax"]], max(lay$kind_key$xmax))
  expect_true(all(lay$kind_key$sxmax < lay$kind_key$x))
  expect_equal(boxes_overlap(all_boxes(lay)), 0)
})

test_that("the key of kinds and the key of details do not overlap", {
  d <- flow_example("survey") |>
    flow_edit(stage = "Households",
              details = step_details(who = "trained enumerators",
                                     tool = "tablet questionnaire")) |>
    flow_edit(stage = "Interviews", kind = "physical") |>
    flow_edit(stage = "Analysis", kind = "data")
  lay <- flow_layout(d)
  expect_false(is.null(lay$key))
  expect_identical(lay$kind_key$kind, "data")
  every <- rbind(all_boxes(lay), label_rects(lay))
  expect_equal(boxes_overlap(every), 0)
  expect_equal(crossings(lay), 0)
  # kinds first, then details, all below the diagram and inside the picture
  expect_lt(max(lay$key$ymax), min(lay$kind_key$ymin))
  expect_lt(max(lay$kind_key$ymax),
            min(c(lay$boxes$ymin, lay$side$ymin)))
  expect_lt(lay$extent[["ymin"]], min(lay$key$ymin))
  no_kind <- flow_layout(flow_edit(flow_edit(d, stage = "Interviews",
                                             kind = NA),
                                   stage = "Analysis", kind = NA))
  expect_gt(lay$size[["height"]], no_kind$size[["height"]])
})

test_that("the renderer draws data boxes with a dashed edge", {
  d <- lab_kind()
  p <- ggplot2::autoplot(d)
  rl <- rect_layers(p)
  lt <- rl[[1]]$aes_params$linetype
  expect_identical(lt, ifelse(!is.na(box_kinds(d)) & box_kinds(d) == "data",
                              "dashed", "solid"), ignore_attr = TRUE)
  expect_true(all(rl[[2]]$aes_params$linetype == "dashed"))  # exclusions
  # the key's sample boxes
  expect_identical(rl[[3]]$aes_params$linetype, "dashed")
  f <- tempfile(fileext = ".png")
  expect_no_error(flow_save(d, f, dpi = 50))
  expect_true(file.exists(f))
})

test_that("the description names the steps and differing boxes", {
  d <- lab_kind()
  txt <- flow_describe(d)
  expect_match(txt, paste0("Steps done to data \\(drawn with a dashed ",
                           "edge\\): Records; Duplicates removed\\."))
  expect_match(txt, "Steps done to physical things: Samples collected; Assay\\.")
  # "Records cleaned" boxes have their own kind, but it is the step's
  expect_no_match(txt, "Boxes? done to data")
  expect_match(txt, "Box done to physical things: Analysed under")
  e <- flow_edit(d, d$side$id[1], excluded = TRUE, kind = "physical")
  expect_match(flow_describe(e),
               "Boxes done to physical things: Duplicates removed after")
  # alt text of the plot is the description
  expect_identical(ggplot2::autoplot(d)$labels$alt, unclass(txt))
  expect_output(print(d), "kinds of step: 4 boxes done to data")
})

test_that("examples are unchanged without kinds, and after kinds are removed", {
  for (nm in c("lab", "trial", "survey", "review", "factorial",
               "trial_data", "ml")) {
    x <- flow_example(nm)
    expect_false("kind" %in% names(unclass(x)), info = nm)
    lay <- suppressWarnings(flow_layout(x))
    expect_null(lay$kind_key, info = nm)
    expect_false("kind" %in% names(lay$boxes), info = nm)
    st <- x$stages$name[nrow(x$stages)]
    y <- flow_edit(x, stage = st, kind = "data")
    expect_false(identical(y, x), info = nm)
    y <- flow_edit(y, stage = st, kind = NA)
    expect_identical(unclass(y), unclass(x), info = nm)
    expect_identical(suppressWarnings(flow_layout(y)), lay, info = nm)
    expect_identical(flow_describe(y), flow_describe(x), info = nm)
  }
})

test_that("flow_check() is not changed by kinds", {
  x <- flow_example("trial")
  y <- flow_edit(x, stage = "Analysis", kind = "data")
  expect_identical(flow_check(y), flow_check(x))
})
