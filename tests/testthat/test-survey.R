survey_layout <- function(...) flow_layout(flow_example("survey"), ...)

test_that("a summary split draws one box per parent, labelled by count", {
  d <- flow_start("Frame") |>
    flow_split("Region", c("North", "South")) |>
    flow_split("District", list(c("d1", "d2", "d3"), c("d4", "d5")),
               display = "summary", summary = "{k} districts",
               n = c(300, 200))
  kids <- d$nodes[d$nodes$stage == 3, ]
  expect_equal(kids$label, c("3 districts", "2 districts"))
  expect_equal(kids$n, c("300", "200"))
  expect_equal(d$stages$display[3], "summary")
  expect_equal(d$leaves, kids$id)
  # one box per parent, joined by a plain arrow
  expect_equal(nrow(d$edges[d$edges$to %in% kids$id, ]), 2)
  expect_true(all(d$edges$kind == "arrow"))
})

test_that("summary counts: numbers, text, lists, and bad input", {
  d <- flow_start("Frame") |> flow_split("Region", c("A", "B"))
  one <- flow_split(d, "D", 12, display = "summary")
  expect_equal(one$nodes$label[4:5], c("12 D", "12 D"))
  per <- flow_split(d, "D", c(4, 9), display = "summary", summary = "{k}")
  expect_equal(per$nodes$label[4:5], c("4", "9"))
  txt <- flow_split(d, "D", c("x", "y", "z"), display = "summary",
                    summary = "{k} in {stage}")
  expect_equal(txt$nodes$label[4:5], c("3 in D", "3 in D"))
  lst <- flow_split(d, "D", list(5, c("p", "q")), display = "summary",
                    summary = "{k}")
  expect_equal(lst$nodes$label[4:5], c("5", "2"))
  expect_error(flow_split(d, "D", c(1, 2, 3), display = "summary"),
               "one per box")
  expect_error(flow_split(d, "D", 2.5, display = "summary"), "whole number")
  expect_error(flow_split(d, "D", 0, display = "summary"), "whole number")
  expect_error(flow_split(d, "D", 3), "display = \"summary\"")
  expect_error(flow_split(d, "D", 3, display = "summary", summary = ""),
               "empty label")
  expect_error(flow_split(d, "D", list(3), display = "summary"),
               "one element per box")
})

test_that("flow_cluster samples clusters, summarised by default", {
  d <- flow_start("Frame", n = 9200) |>
    flow_split("Stratum", c("Urban", "Rural")) |>
    flow_cluster("Clusters", c(40, 60), method = "probability proportional to size")
  cl <- d$nodes[d$nodes$stage == 3, ]
  expect_equal(cl$label, c("40 clusters selected", "60 clusters selected"))
  expect_equal(d$stages$type[3], "cluster")
  expect_equal(d$stages$display[3], "summary")
  expect_equal(d$stages$method[3], "probability proportional to size")
  named <- flow_start("Frame") |>
    flow_cluster("Villages", c("V1", "V2", "V3"), display = "all")
  expect_equal(named$nodes$label[2:4], c("V1", "V2", "V3"))
  expect_error(flow_cluster(flow_start("F"), "C", 3, method = ""),
               "must not be empty")
})

test_that("flow_select adds one box per current box with a method", {
  d <- flow_start("Frame") |>
    flow_split("Stratum", c("Urban", "Rural")) |>
    flow_select("Households", "25 per EA", n = c(1000, 1500),
                method = "systematic random sampling")
  hh <- d$nodes[d$nodes$stage == 3, ]
  expect_equal(hh$label, c("25 per EA", "25 per EA"))
  expect_equal(hh$n, c("1,000", "1,500"))
  expect_equal(d$stages$type[3], "select")
  expect_equal(d$stages$method[3], "systematic random sampling")
  expect_true(is.na(flow_select(flow_start("a"), "b", "c")$stages$method[2]))
})

test_that("flow_assign can be summarised too", {
  d <- flow_start("Schools") |>
    flow_assign("Arm", 20, display = "summary", summary = "{k} arms")
  expect_equal(d$nodes$label[2], "20 arms")
  expect_equal(d$stages$type[2], "assign")
})

test_that("the survey describes methods and summary boxes", {
  txt <- unclass(flow_describe(flow_example("survey")))
  expect_match(txt, "Clusters, selected by probability proportional to size: 40 EAs selected from Urban under North;",
               fixed = TRUE)
  expect_match(txt, "(each box stands for several boxes, not drawn)",
               fixed = TRUE)
  expect_match(txt, "Households, selected by systematic random sampling: 25 per EA (n = 1,000)",
               fixed = TRUE)
  expect_match(txt, "Households not interviewed (n = 412) after Households selected: Dwelling vacant or not found (n = 186)",
               fixed = TRUE)
  expect_match(txt, "Analysis: the outcome is Women interviewed (n = 7,904)",
               fixed = TRUE)
})

test_that("survey layout: no overlaps, no crossings, fits a page when scaled", {
  for (con in c("elbow", "straight")) {
    lay <- survey_layout(connector = con)
    expect_equal(boxes_overlap(all_boxes(lay)), 0)
    expect_equal(crossings(lay), 0)
  }
  lay <- survey_layout()
  # one summary box per stratum, centred under it
  b <- lay$boxes
  cl <- b[b$stage == "Clusters", ]
  st <- b[b$stage == "Stratum", ]
  expect_equal(nrow(cl), 6)
  expect_equal(cl$x, st$x, tolerance = 1e-9)
  # wider than tall pages allow at 9 pt, but readable when scaled to a
  # 6.5 in text width (text stays at 7.5 pt or more)
  expect_true(9 * 6.5 / lay$size[["width"]] >= 7.5)
})

test_that("the wide-row warning points to summary rows", {
  d <- flow_start("All") |> flow_split("Many", as.character(1:25))
  expect_warning(flow_layout(d), "display = \"summary\"")
  # the same stage drawn as a summary is one box
  s <- flow_start("All") |> flow_split("Many", 25, display = "summary")
  expect_silent(flow_layout(s))
})

test_that("the survey diagram looks right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("survey design", autoplot(flow_example("survey")))
})
