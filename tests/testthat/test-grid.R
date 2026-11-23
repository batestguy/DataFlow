# Fold grids (milestone 5) ---------------------------------------------------

num <- function(v) as.numeric(gsub(",", "", v))

# Every row covers periods (or folds) 1..n_pos exactly once, left to right.
check_cover <- function(g) {
  for (s in g$splits$id) {
    b <- g$blocks[g$blocks$split == s, ]
    b <- b[order(b$start), ]
    expect_equal(b$start[1], 1)
    expect_equal(b$end[nrow(b)], g$n_pos)
    expect_equal(b$start[-1], b$end[-nrow(b)] + 1)
    expect_true(all(b$end >= b$start))
  }
}

test_that("typed k-fold: one test fold per row, on the diagonal", {
  g <- flow_folds(k = 5, n = c(80, 81, 82, 83, 84))
  expect_s3_class(g, "dataflow_grid")
  expect_equal(nrow(g$blocks), 25)
  check_cover(g)
  te <- g$blocks[g$blocks$role == "Test", ]
  expect_equal(te$start, te$split)
  expect_equal(te$n, c("80", "81", "82", "83", "84"))
  expect_true(all(g$blocks$role[g$blocks$role != "Test"] == "Training"))
  v <- flow_folds(k = 4, validation = TRUE)
  va <- v$blocks[v$blocks$role == "Validation", ]
  expect_equal(va$start, c(2, 3, 4, 1))
  expect_error(flow_folds(), "Give `k`")
  expect_error(flow_folds(k = 1), "2 or more")
  expect_error(flow_folds(k = 2, validation = TRUE), "3 or more")
  expect_error(flow_folds(k = 3, n = 1:2), "3 boxes to fill")
  expect_equal(
    unclass(flow_describe(flow_folds(k = 5, n = 82, method = "stratified sampling",
                                     group = "patient"))),
    paste("Cross-validation grid with 5 folds, one row per split: in each",
          "split one fold is the test set and the other 4 folds are used for",
          "training; each fold is the test set once. The folds hold n = 82,",
          "n = 82, n = 82, n = 82 and n = 82. All records of each patient",
          "are in one fold (not checked against data). Folds were made by",
          "stratified sampling (as stated; not checked against data).")
  )
})

test_that("k-fold from data: counts per fold, grouped check, rows", {
  d <- data.frame(pid = c(1, 1, 2, 3, 4, 4, 5, 6, 7),
                  fold = c("a", "a", "b", "c", "b", "b", "c", "a", "c"))
  g <- flow_folds(d, fold = "fold", group = "pid")
  check_cover(g)
  expect_equal(g$fold_names, c("a", "b", "c"))
  expect_equal(g$blocks$n[g$blocks$split == 1], c("3", "3", "3"))
  for (s in 1:3) {
    expect_equal(sum(num(g$blocks$n[g$blocks$split == s])), nrow(d))
  }
  te <- g$blocks$id[g$blocks$role == "Test" & g$blocks$split == 2]
  expect_equal(flow_data(g, te), d[d$fold == "b", ])
  expect_match(flow_describe(g), "All records of each pid are in one fold.",
               fixed = TRUE)
  expect_match(flow_describe(g),
               "Counts were computed from the data frame `d` (9 rows).",
               fixed = TRUE)
  leak <- d
  leak$fold[2] <- "c"
  expect_error(flow_folds(leak, fold = "fold", group = "pid"),
               "1 value of \"pid\" (e.g. 1) is in more than one fold",
               fixed = TRUE)
  na <- d
  na$fold[1] <- NA
  expect_error(flow_folds(na, fold = "fold"), "missing values")
  expect_error(flow_folds(d), "need `fold =`")
  expect_error(flow_folds(d, fold = "zz"), "not in `data`")
  expect_error(flow_folds(1:3, fold = "a"), "must be a data frame")
})

test_that("expanding and rolling windows follow the rolling-origin rules", {
  g <- flow_folds(type = "expanding", n = 48, initial = 24, assess = 3,
                  skip = 2, gap = 1, time_unit = "months")
  check_cover(g)
  expect_equal(nrow(g$splits), 7)
  for (s in g$splits$id) {
    b <- g$blocks[g$blocks$split == s, ]
    tr <- b[b$role == "Training", ]
    gp <- b[b$role == "Gap", ]
    te <- b[b$role == "Test", ]
    expect_equal(c(tr$start, tr$end), c(1, 24 + (s - 1) * 3))
    expect_equal(c(gp$start, gp$end), c(tr$end + 1, tr$end + 1))
    expect_equal(c(te$start, te$end), c(tr$end + 2, tr$end + 4))
  }
  expect_equal(
    unclass(flow_describe(g)),
    paste("Time-series evaluation grid with 7 splits, expanding window:",
          "training starts with 24 months and grows by 3 months; each test",
          "window is the next 3 months after a 1-month gap; training always",
          "precedes testing. Data outside a split's windows are marked as",
          "not used.")
  )
  r <- flow_folds(type = "rolling", n = 40, initial = 12, assess = 4,
                  skip = 3)
  check_cover(r)
  tr <- r$blocks[r$blocks$role == "Training", ]
  expect_true(all(tr$end - tr$start + 1 == 12))
  expect_equal(diff(tr$start), rep(4, nrow(tr) - 1))
  expect_match(flow_describe(r),
               "training is a moving window of 12 periods that moves on by 4 periods; each test window is the next 4 periods after training;",
               fixed = TRUE)
  # n_splits instead of n
  m <- flow_folds(type = "expanding", initial = 10, assess = 2, n_splits = 3)
  expect_equal(m$n_pos, 14)
  expect_error(flow_folds(type = "expanding", n = 10, initial = 5),
               "need `initial`")
  expect_error(flow_folds(type = "expanding", n = 5, initial = 5,
                          assess = 1), "No split fits")
  expect_error(flow_folds(type = "expanding", n = 20, initial = 10,
                          assess = 5, n_splits = 8), "only 20")
  expect_error(flow_folds(type = "rolling", initial = 3, assess = 1),
               "Give the number of time periods")
  expect_error(flow_folds(type = "expanding", n = 20, initial = 2.5,
                          assess = 1), "whole number")
})

test_that("blocked cross-validation: contiguous blocks with gaps", {
  g <- flow_folds(type = "blocked", n = 60, k = 5, gap = 2,
                  time_unit = "months")
  check_cover(g)
  te <- g$blocks[g$blocks$role == "Test", ]
  expect_equal(te$start, c(1, 13, 25, 37, 49))
  expect_equal(te$end, c(12, 24, 36, 48, 60))
  for (s in 1:5) {
    b <- g$blocks[g$blocks$split == s, ]
    gaps <- b[b$role == "Gap", ]
    expect_equal(sum(gaps$end - gaps$start + 1),
                 if (s %in% c(1, 5)) 2 else 4)
  }
  expect_match(flow_describe(g), "leaving out a gap of 2 months on each side",
               fixed = TRUE)
  expect_error(flow_folds(type = "blocked", n = 3, k = 5), "more than the 3")
})

test_that("time grids from data: windows by time, counts, order", {
  set.seed(5)
  d <- data.frame(day = as.Date("2021-01-01") + sample(0:59, 300, TRUE))
  g <- flow_folds(d, type = "expanding", time = "day", initial = 30,
                  assess = 5, skip = 4, gap = 2, time_unit = "days")
  times <- sort(unique(d$day))
  expect_equal(g$n_pos, length(times))
  check_cover(g)
  for (s in g$splits$id) {
    b <- g$blocks[g$blocks$split == s, ]
    expect_equal(sum(num(b$n)), nrow(d))
    tr <- flow_data(g, b$id[b$role == "Training"])
    te <- flow_data(g, b$id[b$role == "Test"])
    expect_true(max(tr$day) < min(te$day))
    expect_equal(nrow(te), sum(d$day %in% times[b$start[b$role == "Test"]:
                                                   b$end[b$role == "Test"]]))
  }
  expect_match(flow_describe(g), "Time runs from 2021-01-01 to 2021-03-01",
               fixed = TRUE)
  expect_error(flow_folds(d, type = "expanding", initial = 3, assess = 1),
               "need `time =`")
  expect_error(flow_folds(d, type = "expanding", time = "day", n = 4,
                          initial = 3, assess = 1), "leave `n` out")
})

test_that("random grids: windows never overlap and counts add up", {
  set.seed(20261127)
  for (it in 1:60) {
    type <- sample(c("expanding", "rolling", "blocked"), 1)
    use_data <- runif(1) < 0.5
    total <- sample(20:80, 1)
    d <- data.frame(t = sample(seq_len(total), sample(50:200, 1), TRUE))
    P <- if (use_data) length(unique(d$t)) else total
    args <- if (type == "blocked") {
      list(type = type, k = sample(2:min(6, P), 1), gap = sample(0:2, 1))
    } else {
      init <- sample(3:max(3, P %/% 2), 1)
      list(type = type, initial = init, assess = sample(1:4, 1),
           skip = sample(0:3, 1), gap = sample(0:2, 1))
    }
    g <- tryCatch(
      if (use_data) do.call(flow_folds, c(list(d, time = "t"), args)) else
        do.call(flow_folds, c(list(n = total), args)),
      error = function(e) NULL)
    if (is.null(g)) next
    check_cover(g)
    for (s in g$splits$id) {
      b <- g$blocks[g$blocks$split == s, ]
      te <- b[b$role == "Test", ]
      tr <- b[b$role == "Training", ]
      expect_equal(nrow(te), 1)
      if (type != "blocked") {
        gp <- b[b$role == "Gap", ]
        expect_true(max(tr$end) < te$start)
        expect_equal(te$start - max(tr$end) - 1, args$gap)
        if (nrow(gp)) expect_true(gp$start > max(tr$end) && gp$end < te$start)
      } else {
        # training never touches the test block
        expect_true(all(tr$end < te$start - args$gap |
                          tr$start > te$end + args$gap))
      }
      if (use_data) expect_equal(sum(num(b$n)), nrow(d))
    }
    lay <- grid_layout(g)
    bl <- lay$blocks
    for (s in unique(bl$split)) {
      r <- bl[bl$split == s, ]
      r <- r[order(r$xmin), ]
      expect_true(all(r$xmin[-1] >= r$xmax[-nrow(r)] - 1e-9))
    }
  }
})

test_that("grid layout: text fits, legend when abbreviated, labels left", {
  g <- flow_folds(type = "expanding", n = 48, initial = 24, assess = 3,
                  skip = 2, gap = 1, time_unit = "months")
  lay <- grid_layout(g)
  b <- lay$blocks
  w <- with_measure_device(text_width(b$text, 9, "", "plain"))
  expect_true(all(w <= b$xmax - b$xmin))
  expect_true(any(b$shown %in% c("short", "none")))
  expect_equal(sort(lay$legend$role), sort(unique(b$role)))
  expect_true(all(lay$row_labels$x + lay$row_labels$w < min(b$xmin)))
  expect_true(all(lay$ticks$x >= 0 & lay$ticks$x <= 5.5))
  # roles are never shown by fill alone: every block has text or a legend
  expect_true(all(b$text != "" | b$role %in% lay$legend$role))
  # k-fold blocks are wide enough for full labels: no legend
  k <- grid_layout(flow_folds(k = 5, n = 82))
  expect_true(all(k$blocks$shown == "full"))
  expect_equal(nrow(k$legend), 0)
  expect_equal(k$headers$label, paste("Fold", 1:5))
  # greyscale-safe, distinct fills; gaps dashed
  expect_equal(length(unique(grid_fill)), 5)
  expect_true(all(b$linetype[b$role == "Gap"] == "dashed"))
  expect_error(grid_layout(flow_example("lab")), "fold grid")
})

test_that("grids can be edited by hand; edits are marked as typed", {
  g <- flow_example("cv")
  id <- g$blocks$id[g$blocks$split == 1 & g$blocks$role == "Test"]
  e <- flow_edit(g, id, n = 999, label = "Held out")
  expect_equal(e$blocks$n[e$blocks$id == id], "999")
  expect_equal(e$blocks$label[e$blocks$id == id], "Held out")
  expect_identical(flow_data(e, id), flow_data(g, id))
  expect_match(flow_describe(e), "except 1 typed by hand: Test in split 1 (n = 999).",
               fixed = TRUE)
  expect_true("Held out" %in% grid_layout(e)$blocks$text |
                any(grepl("Held out", grid_layout(e)$blocks$text)))
  expect_error(flow_edit(g, "a", n = 1), "block ids")
  expect_error(flow_data(flow_folds(k = 3), 1), "no data")
})

test_that("grids draw, save and carry alt text", {
  g <- flow_example("forecast")
  p <- autoplot(g)
  expect_true(ggplot2::is_ggplot(p))
  expect_match(ggplot2::get_alt_text(p), "^Time-series evaluation grid")
  f <- tempfile(fileext = ".png")
  expect_equal(flow_save(g, f, dpi = 50), f)
  expect_gt(file.size(f), 0)
  expect_output(print(g), "expanding: 5 splits over 96 months")
  expect_output(print(grid_layout(g)), "20 blocks in 5 rows")
})

test_that("the cv and forecast examples trace to their data", {
  cv <- flow_example("cv")
  expect_match(attr(cv, "source"), "Invented grouped 5-fold")
  for (s in 1:5) {
    expect_equal(sum(num(cv$blocks$n[cv$blocks$split == s])),
                 nrow(cv$data$df))
  }
  fc <- flow_example("forecast")
  expect_match(attr(fc, "source"), "rsample")
  expect_equal(nrow(fc$splits), 5)
  expect_match(flow_describe(fc), "split 1: training n = 180, test n = 18;",
               fixed = TRUE)
})

test_that("the cv and forecast grids look right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("cv grid", autoplot(flow_example("cv")))
  vdiffr::expect_doppelganger("forecast grid",
                              autoplot(flow_example("forecast")))
})
