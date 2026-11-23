lab_layout <- function(...) flow_layout(flow_example("lab"), ...)

test_that("layout is a set of data frames with one box per node", {
  lay <- lab_layout()
  expect_s3_class(lay, "dataflow_layout")
  expect_equal(nrow(lay$boxes), 27)
  expect_true(all(c("x", "y", "w", "h", "xmin", "xmax") %in% names(lay$boxes)))
  expect_true(all(is.finite(c(lay$boxes$x, lay$boxes$y))))
})

test_that("no two boxes overlap", {
  expect_equal(boxes_overlap(lab_layout()$boxes), 0)
  expect_equal(boxes_overlap(lab_layout(connector = "straight",
                                        base_size = 14)$boxes), 0)
})

test_that("boxes in a row share a height and rows go downwards", {
  b <- lab_layout()$boxes
  for (r in unique(b$row)) expect_equal(length(unique(b$h[b$row == r])), 1)
  ry <- tapply(b$y, b$row, unique)
  expect_true(all(diff(ry) < 0))
})

test_that("each split parent is centred over its children", {
  d <- flow_example("lab")
  b <- lab_layout()$boxes
  for (p in unique(d$edges$from[d$edges$kind == "arrow"])) {
    kids <- d$edges$to[d$edges$from == p & d$edges$kind == "arrow"]
    kx <- b$x[match(kids, b$id)]
    expect_equal(b$x[b$id == p], mean(range(kx)), tolerance = 1e-9)
  }
})

test_that("a bracket spans exactly its sources", {
  d <- flow_example("lab")
  lay <- lab_layout()
  b <- lay$boxes
  for (m in unique(d$edges$to[d$edges$kind == "bracket"])) {
    src <- d$edges$from[d$edges$to == m]
    path <- lay$brackets[lay$brackets$group == m, ]
    expect_equal(min(path$x), min(b$xmin[match(src, b$id)]))
    expect_equal(max(path$x), max(b$xmax[match(src, b$id)]))
    # bracket sits below its sources and above the merged box
    expect_true(max(path$y) < min(b$ymin[match(src, b$id)]))
    expect_true(min(path$y) > b$ymax[b$id == m])
    # merged box is centred under the bracket
    expect_equal(b$x[b$id == m], mean(range(path$x)), tolerance = 1e-9)
  }
})

test_that("brackets of neighbouring groups do not touch", {
  lay <- lab_layout()
  br <- split(lay$brackets$x, lay$brackets$group)
  r <- vapply(br, range, numeric(2))
  r <- r[, order(r[1, ]), drop = FALSE]
  expect_true(all(r[1, -1] - r[2, -ncol(r)] >= 0.3 - 1e-9))
})

test_that("every arrowhead lands on the top edge of a box", {
  lay <- lab_layout()
  b <- lay$boxes
  heads <- lay$segments[lay$segments$head, ]
  on_box <- vapply(seq_len(nrow(heads)), function(i) {
    any(abs(b$ymax - heads$yend[i]) < 1e-9 &
          heads$xend[i] >= b$xmin & heads$xend[i] <= b$xmax)
  }, logical(1))
  expect_true(all(on_box))
  # one arrowhead per box except the top one
  expect_equal(nrow(heads), nrow(b) - 1)
})

test_that("row labels sit left of every box", {
  lay <- lab_layout()
  expect_equal(nrow(lay$row_labels), 7)
  expect_true(all(lay$row_labels$x + lay$row_labels$w <
                    min(lay$boxes$xmin)))
})

test_that("box widths fit their text", {
  b <- lab_layout()$boxes
  w <- with_measure_device(text_width(b$text, 9, "", "plain"))
  expect_true(all(b$w >= w))
})

test_that("very wide rows warn", {
  d <- flow_start("All") |> flow_split("Many", as.character(1:25))
  expect_warning(flow_layout(d), "more than 20 boxes")
})

test_that("layout does not change the open graphics device", {
  before <- grDevices::dev.cur()
  lab_layout()
  expect_equal(grDevices::dev.cur(), before)
})

test_that("irregular designs: no overlaps and no line through a box", {
  for (nm in names(irregular_designs())) {
    lay <- flow_layout(irregular_designs()[[nm]])
    expect_equal(boxes_overlap(lay$boxes), 0, label = nm)
    expect_equal(crossings(lay), 0, label = nm)
    expect_equal(crossings(flow_layout(irregular_designs()[[nm]],
                                       connector = "straight")), 0,
                 label = paste(nm, "straight"))
  }
  expect_equal(crossings(lab_layout()), 0)
})

test_that("a bracket source higher than the others gets a drop line", {
  lay <- flow_layout(irregular_designs()$uneven_bracket)
  b <- lay$boxes
  s2 <- b[b$label == "s2", ]
  expect_equal(nrow(s2), 2)
  for (i in seq_len(nrow(s2))) {
    drop <- lay$segments[abs(lay$segments$x - s2$x[i]) < 1e-9 &
                           abs(lay$segments$y - s2$ymin[i]) < 1e-9, ]
    expect_equal(nrow(drop), 1)
    expect_true(drop$yend < min(b$ymin[b$label == "p1"]))
  }
})

test_that("an unknown font family warns once and still lays out", {
  w <- character()
  lay <- withCallingHandlers(
    lab_layout(family = "NoSuchFontXYZ"),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1)
  expect_match(w, "NoSuchFontXYZ")
  expect_equal(lay$size, lab_layout()$size)
})

test_that("merge_segments draws each stretch of line once", {
  sg <- data.frame(
    x = c(1, 1, 1, 0, 2), y = c(0, 0, -1, -3, -3),
    xend = c(1, 1, 1, 2, 2.5), yend = c(-2, -1, -3, -3, -3),
    head = c(TRUE, FALSE, FALSE, FALSE, TRUE),
    kind = c("arrow", "side", "pool", "pool", "side")
  )
  m <- merge_segments(sg)
  # the stub inside the arrow is gone; the pool line keeps only its part
  # below the arrow, still drawn downwards; the rest is unchanged
  expect_equal(nrow(m), 4)
  expect_equal(m$kind, c("arrow", "pool", "pool", "side"))
  expect_equal(unlist(m[2, c("x", "y", "xend", "yend")]),
               c(x = 1, y = -2, xend = 1, yend = -3))
  expect_equal(m[c(1, 3, 4), ], sg[c(1, 4, 5), ], ignore_attr = TRUE)
  expect_equal(merge_segments(sg[1, ]), sg[1, ])
})

test_that("no stretch of line is drawn twice in the example layouts", {
  designs <- c(lapply(c(lab = "lab", trial = "trial", survey = "survey",
                        review = "review", factorial = "factorial",
                        trial_data = "trial_data"), flow_example),
               trial_designs(), stream_designs())
  for (nm in names(designs)) {
    for (con in c("elbow", "straight")) {
      sg <- flow_layout(designs[[nm]], connector = con)$segments
      for (o in c("v", "h")) {
        on <- if (o == "v") {
          abs(sg$x - sg$xend) < 1e-9 & abs(sg$y - sg$yend) > 1e-9
        } else {
          abs(sg$y - sg$yend) < 1e-9 & abs(sg$x - sg$xend) > 1e-9
        }
        s <- sg[on, ]
        key <- round(if (o == "v") s$x else s$y, 9)
        lo <- if (o == "v") pmin(s$y, s$yend) else pmin(s$x, s$xend)
        hi <- if (o == "v") pmax(s$y, s$yend) else pmax(s$x, s$xend)
        twice <- 0
        for (i in seq_len(nrow(s))) for (j in seq_len(nrow(s))) {
          if (i < j && key[i] == key[j] &&
              min(hi[i], hi[j]) - max(lo[i], lo[j]) > 1e-9) twice <- twice + 1
        }
        expect_equal(twice, 0, label = paste(nm, con, o))
      }
    }
  }
})
