# Several starting streams and pools (milestone 4) --------------------------

test_that("flow_start makes several top boxes side by side", {
  d <- flow_start(c("Cases", "Controls"), n = c(120, 240), note = "adults")
  expect_equal(d$nodes$label, c("Cases", "Controls"))
  expect_equal(d$nodes$n, c("120", "240"))
  expect_equal(d$nodes$note, c("adults", "adults"))
  expect_true(all(is.na(d$nodes$anchor)))
  expect_equal(d$leaves, 1:2)
  expect_equal(d$stages$name, "Start")
  expect_equal(d$stages$row_label, "")
  expect_equal(flow_start(c("a", "b"), stage = "Sources")$stages$name,
               "Sources")
  expect_equal(flow_start(c("a", "b"), n = 5)$nodes$n, c("5", "5"))
  expect_error(flow_start(c("a", "b"), n = 1:3), "2 boxes to fill")
  expect_error(flow_start(c("a", "")), "not empty")
  expect_error(flow_start(character()), "not empty")
  # one label keeps the old rules
  expect_error(flow_start("a", n = 1:2), "single `n`")
  lay <- flow_layout(d)
  expect_equal(lay$boxes$y[1], lay$boxes$y[2])
  expect_equal(lay$boxes$xmin[2] - lay$boxes$xmax[1], 0.3, tolerance = 1e-9)
})

test_that("validator: top boxes on the first stage, others reached", {
  d <- flow_start(c("A", "B")) |> flow_split("S", c("x", "y"), from = "A")
  expect_silent(validate_studydesignflow(d))
  bad <- d
  bad$nodes$anchor[3] <- NA
  expect_error(validate_studydesignflow(bad), "top box must be on the first stage")
  bad <- d
  bad$edges <- rbind(bad$edges, data.frame(from = 1L, to = 2L,
                                           kind = "arrow"))
  expect_error(validate_studydesignflow(bad), "cannot have a box above")
  bad <- d
  bad$nodes <- bad$nodes[0, ]
  bad$edges <- bad$edges[0, ]
  expect_error(validate_studydesignflow(bad), "at least one top box")
  # a pool joining two streams has no common ancestor: allowed
  p <- flow_pool(flow_start(c("A", "B")), "Joined", "All")
  expect_true(is.na(p$nodes$anchor[3]))
  expect_silent(validate_studydesignflow(p))
  bad <- p
  bad$edges <- bad$edges[-1, ]
  expect_error(validate_studydesignflow(bad), "at least two sources")
})

test_that("`from` continues one stream while the other waits", {
  d <- flow_start(c("Left", "Right")) |>
    flow_split("S1", c("a", "b"), from = "Left") |>
    flow_exclude("Out", n = 1, from = "b") |>
    flow_split("S3", c("p", "q"), from = "Right")
  expect_equal(flow_nodes(d)$label[d$leaves], c("a", "b", "p", "q"))
  expect_equal(d$side$parent, 4L)
  lay <- flow_layout(d)
  b <- lay$boxes
  # the right stream keeps its own rows: nothing of it on rows 2 and 3
  right <- b[b$label %in% c("Right", "p", "q"), ]
  expect_equal(sort(unique(right$row)), c(1L, 4L))
  # the streams' top boxes are group_gap apart, and the right stream's lines
  # through rows 2 and 3 stay right of the left stream's boxes on those rows,
  # its exclusion box included
  expect_true(b$xmin[b$label == "Right"] - b$xmax[b$label == "Left"] >=
                0.3 - 1e-9)
  left_r <- max(c(b$xmax[b$label %in% c("a", "b")], lay$side$xmax))
  vert <- lay$segments[abs(lay$segments$x - lay$segments$xend) < 1e-9 &
                         lay$segments$x > b$xmax[b$label == "b"], ]
  expect_true(nrow(vert) >= 2)
  expect_true(all(vert$x - left_r > 0.25))
})

test_that("stream designs: no overlaps and no line through a box", {
  for (nm in names(stream_designs())) {
    for (con in c("elbow", "straight")) {
      lay <- flow_layout(stream_designs()[[nm]], connector = con)
      expect_equal(boxes_overlap(all_boxes(lay)), 0,
                   label = paste(nm, con, "overlaps"))
      expect_equal(crossings(lay), 0, label = paste(nm, con, "crossings"))
    }
  }
})

test_that("long lines through rows with no box keep their column clear", {
  lay <- flow_layout(stream_designs()$wait_then_split)
  b <- lay$boxes
  right <- b[b$label == "Right", ]
  kids <- b[b$label %in% c("Wide box one", "Wide box two"), ]
  # every vertical line of the right stream between its rows 1 and 4 stays
  # clear of all boxes on the rows it passes (rows 2 and 3)
  vert <- lay$segments[abs(lay$segments$x - lay$segments$xend) < 1e-9 &
                         lay$segments$x >= min(kids$xmin) - 1e-9, ]
  passed <- all_boxes(lay)
  passed <- passed[passed$ymax < right$ymin & passed$ymin > max(kids$ymax), ]
  for (i in seq_len(nrow(vert))) {
    inside <- passed$xmin - 0.03 < vert$x[i] & vert$x[i] < passed$xmax + 0.03
    expect_false(any(inside))
  }
})

test_that("a pool is centred under its sources' centres, with one arrow", {
  for (nm in c("review", "factorial", "groups_pool", "pool_far_below",
               "three_streams", "pool_uneven_rows")) {
    d <- stream_designs()[[nm]]
    lay <- flow_layout(d)
    b <- lay$boxes
    sg <- lay$segments[lay$segments$kind == "pool", ]
    for (m in unique(d$edges$to[d$edges$kind == "pool"])) {
      src <- b[match(d$edges$from[d$edges$to == m], b$id), ]
      mb <- b[b$id == m, ]
      expect_equal(mb$x, mean(range(src$x)), tolerance = 1e-9, label = nm)
      heads <- sg[sg$head & abs(sg$xend - mb$x) < 1e-9 &
                    abs(sg$yend - mb$ymax) < 1e-9, ]
      expect_equal(nrow(heads), 1, label = nm)
      # a line runs down from each source's centre to the join line
      yb <- heads$y
      for (i in seq_len(nrow(src))) {
        down <- sg[abs(sg$x - src$x[i]) < 1e-9 & abs(sg$xend - src$x[i]) < 1e-9 &
                     abs(sg$y - src$ymin[i]) < 1e-9 & abs(sg$yend - yb) < 1e-9, ]
        expect_equal(nrow(down), 1, label = nm)
      }
      bar <- sg[!sg$head & abs(sg$y - yb) < 1e-9 & abs(sg$yend - yb) < 1e-9 &
                  sg$x <= mb$x & sg$xend >= mb$x, ]
      expect_equal(nrow(bar), 1, label = nm)
      expect_equal(bar$x, min(src$x), tolerance = 1e-9)
      expect_equal(bar$xend, max(src$x), tolerance = 1e-9)
      # the join sits below all sources, above the pooled box, and below any
      # exclusion box hanging off a source
      expect_true(yb < min(src$ymin) && yb > mb$ymax)
      sides <- lay$side[lay$side$parent %in% src$id, ]
      if (nrow(sides)) expect_true(yb < min(sides$ymin))
    }
    # pools are drawn as lines, never as brackets
    expect_equal(nrow(lay$brackets), 0)
  }
})

test_that("every arrowhead in the stream designs lands on a box", {
  for (nm in names(stream_designs())) {
    lay <- flow_layout(stream_designs()[[nm]])
    b <- lay$boxes
    s <- lay$side
    heads <- lay$segments[lay$segments$head, ]
    ok <- vapply(seq_len(nrow(heads)), function(i) {
      any(abs(b$ymax - heads$yend[i]) < 1e-9 &
            heads$xend[i] >= b$xmin & heads$xend[i] <= b$xmax) ||
        any(abs(s$xmin - heads$xend[i]) < 1e-9 &
              heads$yend[i] > s$ymin & heads$yend[i] < s$ymax)
    }, logical(1))
    expect_true(all(ok), label = nm)
    top <- sum(b$row == 1)
    expect_equal(nrow(heads), nrow(b) - top + nrow(s), label = nm)
  }
})

test_that("flow_pool groups boxes by default, by `over` and by `groups`", {
  base <- flow_start("All") |> flow_split("Arm", c("A", "B")) |>
    flow_split("Diet", c("x", "y"))
  one <- flow_pool(base, "P", "Everything")
  expect_equal(sum(one$edges$to == 8), 4)
  expect_equal(one$leaves, 8L)
  expect_equal(one$stages$type[4], "pool")
  expect_true(all(one$edges$kind[one$edges$to == 8] == "pool"))
  # anchored at the lowest common ancestor
  expect_equal(one$nodes$anchor[8], 1L)
  by_arm <- flow_pool(base, "P", c("Pooled A", "Pooled B"), n = c(10, 20),
                      over = "Arm")
  expect_equal(by_arm$nodes$label[8:9], c("Pooled A", "Pooled B"))
  expect_equal(by_arm$nodes$anchor[8:9], c(2L, 3L))
  expect_equal(by_arm$stages$over[4], "Arm")
  g <- flow_pool(base, "P", "Some", groups = list(c(4, 6)))
  expect_equal(g$edges$from[g$edges$to == 8], c(4L, 6L))
  expect_equal(flow_nodes(g)$label[g$leaves], c("Some", "y", "y"))
  sub <- flow_pool(base, "P", "Two", from = c(4, 5))
  expect_equal(sub$nodes$anchor[8], 2L)
})

test_that("flow_pool refuses pools that are not pools", {
  base <- flow_start("All") |> flow_split("Arm", c("A", "B")) |>
    flow_split("Diet", c("x", "y"), from = "A")
  expect_error(flow_pool(base, "P", "m", over = "Arm"),
               "at least two boxes")
  expect_error(flow_pool(base, "P", "m", from = "B"), "at least two boxes")
  expect_error(flow_pool(base, "P", "m", groups = list(c(4, 5), c(5, 3))),
               "only one pool per call")
  expect_error(flow_pool(base, "P", "m", groups = list(c(4, 5)),
                         over = "Arm"), "not both")
  expect_error(flow_pool(base, "P", "m", groups = c(4, 5)), "must be a list")
  expect_error(flow_pool(base, "P", "m", groups = list(c(2, 4))),
               "box below it")
})

test_that("several starts and pools are described", {
  d <- flow_start(c("Cases", "Controls"), n = c(120, 240)) |>
    flow_pool("Analysis", "All participants", n = 360)
  expect_equal(
    unclass(flow_describe(d)),
    paste("Flow diagram with 2 stages, read from top to bottom.",
          "It starts with 2 boxes side by side: Cases (n = 120) and",
          "Controls (n = 240).",
          "Analysis: Cases and Controls are combined into All participants",
          "(n = 360).")
  )
  txt <- unclass(flow_describe(flow_example("factorial")))
  expect_match(txt, paste("Pooled analysis: Analysed under Diet A under Drug",
                          "and Analysed under Diet B under Drug are combined",
                          "into Pooled over diet (n = 95); Analysed under",
                          "Diet A under Placebo and Analysed under Diet B",
                          "under Placebo are combined into Pooled over diet",
                          "(n = 95)."), fixed = TRUE)
})

test_that("the review example follows PRISMA 2020 and its counts add up", {
  d <- flow_example("review")
  expect_match(attr(d, "source"), "PRISMA 2020")
  expect_match(attr(d, "source"), "Invented")
  n <- function(lbl) as.numeric(gsub(",", "", d$nodes$n[d$nodes$label == lbl]))
  sn <- function(lbl) as.numeric(gsub(",", "", d$side$n[d$side$label == lbl]))
  ident <- n(c("Records identified from\ndatabases and registers",
               "Records identified from\nother sources"))
  expect_equal(ident[1] - sn("Records removed before screening"),
               n("Records screened"))
  expect_equal(n("Records screened") - sn("Records excluded"),
               n("Reports sought for retrieval")[1])
  expect_equal(n("Reports sought for retrieval")[2], ident[2])
  expect_equal(n("Reports sought for retrieval") -
                 sn("Reports not retrieved"),
               n("Reports assessed for eligibility"))
  expect_equal(sum(n("Reports assessed for eligibility") -
                     sn("Reports excluded")),
               n("Studies included in review"))
  r <- d$reasons
  for (s in d$side$id[!is.na(d$side$n)]) {
    if (any(r$side == s)) {
      expect_equal(sum(as.numeric(r$n[r$side == s])),
                   as.numeric(gsub(",", "", d$side$n[d$side$id == s])))
    }
  }
  txt <- unclass(flow_describe(d))
  expect_match(txt, "It starts with 2 boxes side by side in the Identification row: Records identified from databases and registers (n = 1,954;",
               fixed = TRUE)
  expect_match(txt, "are combined into Studies included in review (n = 43;",
               fixed = TRUE)
})

test_that("the factorial example's counts add up", {
  d <- flow_example("factorial")
  expect_match(attr(d, "source"), "Invented 2 x 2 factorial")
  num <- function(v) as.numeric(gsub(",", "", v))
  cells <- d$nodes$id[d$nodes$label %in% c("Diet A", "Diet B")]
  out <- d$nodes$id[d$nodes$label == "Analysed"]
  expect_equal(num(d$nodes$n[match(cells, d$nodes$id)]) -
                 num(d$side$n[match(cells, d$side$parent)]),
               num(d$nodes$n[match(out, d$nodes$id)]))
  pooled <- d$nodes$id[d$nodes$label == "Pooled over diet"]
  for (p in pooled) {
    src <- d$edges$from[d$edges$to == p]
    expect_equal(sum(num(d$nodes$n[match(src, d$nodes$id)])),
                 num(d$nodes$n[d$nodes$id == p]))
  }
})

test_that("print and flow_nodes cope with several streams", {
  expect_output(print(flow_example("review")), "\\[pool\\]")
  expect_equal(sum(flow_nodes(flow_example("review"))$leaf), 1)
})

test_that("the review and factorial diagrams look right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("review design",
                              autoplot(flow_example("review")))
  vdiffr::expect_doppelganger("factorial design",
                              autoplot(flow_example("factorial")))
})

test_that("a pool cannot join a box with a box below it across streams", {
  # found by random testing: the layout tree stops at a pool that joins
  # streams, so the old check missed that box 2 is above box 5
  d <- flow_start(c("Ccc", "Long label here")) |>
    flow_select("S1", "Two lines") |>
    flow_pool("S2", "A")
  expect_error(flow_pool(d, "S5", "Ccc", from = c(2, 5)), "box below it")
})
