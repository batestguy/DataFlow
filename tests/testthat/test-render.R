test_that("autoplot returns a ggplot with alt text and natural size", {
  p <- autoplot(flow_example("lab"))
  expect_true(ggplot2::is_ggplot(p))
  expect_match(ggplot2::get_alt_text(p), "^Flow diagram with 8 stages")
  expect_named(attr(p, "dataflow_size"), c("width", "height"))
})

test_that("notes are drawn in their own text layer", {
  d <- flow_start("People", n = 10, note = "adults only") |>
    flow_split("Site", c("North", "South"))
  p <- autoplot(d)
  expect_true(any(vapply(p$layers, function(l) {
    "note" %in% names(l$data)
  }, logical(1))))
  lay <- flow_layout(d)
  top <- lay$boxes[1, ]
  expect_true(top$note_y < top$text_y)
  expect_true(top$note_y > top$ymin)
})

test_that("flow_save writes PNG and PDF at the natural size", {
  d <- flow_example("lab")
  png <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  expect_equal(flow_save(d, png, dpi = 100), png)
  flow_save(d, pdf)
  expect_gt(file.size(png), 0)
  expect_gt(file.size(pdf), 0)
})

test_that("flow_save refuses other objects", {
  expect_error(flow_save(1, tempfile(fileext = ".png")), "must be a dataflow")
})

test_that("the lab diagram looks right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  vdiffr::expect_doppelganger("lab design", autoplot(flow_example("lab")))
})
