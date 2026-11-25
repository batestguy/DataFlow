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
  expect_s3_class(g, "studydesignflow_grid")
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
          "training; each fold is the test set once. Each fold holds n = 82.",
          "All records of each patient",
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
          "precedes testing (as these settings define the windows). Data",
          "outside a split's windows are marked as not used. At the default",
          "size, narrow gap blocks are drawn wider than scale.")
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
      expect_equal(r$xmin[-1], r$xmax[-nrow(r)])
      # as drawn: blocks tile the row, and none is hidden
      expect_equal(r$draw_xmin[-1], r$draw_xmax[-nrow(r)])
      expect_equal(range(r$draw_xmin, r$draw_xmax), range(r$xmin, r$xmax))
      w <- r$draw_xmax - r$draw_xmin
      expect_true(all(w[r$role != "Not used"] >= 0.12 - 1e-9))
    }
  }
})

test_that("grid layout: text fits, legend when abbreviated, labels left", {
  g <- flow_folds(type = "expanding", n = 48, initial = 24, assess = 3,
                  skip = 2, gap = 1, time_unit = "months")
  lay <- grid_layout(g)
  b <- lay$blocks
  w <- with_measure_device(text_width(b$text, 9, "", "plain"))
  expect_true(all(w <= b$draw_xmax - b$draw_xmin))
  expect_true(any(b$shown %in% c("short", "none")))
  # the legend lists the roles shown short, left blank or widened, only
  expect_equal(sort(lay$legend$role),
               sort(unique(b$role[b$shown %in% c("short", "none") |
                                    b$widened])))
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
  expect_match(flow_describe(e), "except 1 fold size typed by hand: fold 1 (n = 999).",
               fixed = TRUE)
  expect_true("Held out" %in% grid_layout(e)$blocks$text |
                any(grepl("Held out", grid_layout(e)$blocks$text)))
  # a fold has one size: it changes in every split
  expect_equal(e$blocks$n[e$blocks$start == 1], rep("999", 5))
  f <- flow_example("forecast")
  fid <- f$blocks$id[f$blocks$split == 2 & f$blocks$role == "Test"]
  expect_match(flow_describe(flow_edit(f, fid, n = 20)),
               "except 1 typed by hand: test in split 2 (n = 20).",
               fixed = TRUE)
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
  expect_match(flow_describe(fc), "split 1: training (n = 180), gap (n = 3), test (n = 18), not used (n = 87);",
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

test_that("a thin gap never hides the thin test block after it", {
  d <- data.frame(day = as.Date("2021-01-01") + 0:364)
  g <- flow_folds(d, type = "rolling", time = "day", initial = 180,
                  assess = 1, gap = 1, skip = 29, time_unit = "days")
  lay <- grid_layout(g)
  b <- lay$blocks
  te <- b[b$role == "Test", ]
  gp <- b[b$role == "Gap", ]
  expect_equal(te$draw_xmax - te$draw_xmin, rep(0.12, nrow(te)))
  expect_equal(gp$draw_xmax, te$draw_xmin)
  expect_true(all(grepl("drawn wider than scale",
                        lay$legend$label[lay$legend$role %in%
                                           c("Test", "Gap")])))
  expect_match(flow_describe(g), "narrow test and gap blocks are drawn wider",
               fixed = TRUE)
  # more than 8 splits: the description lists three
  m <- flow_folds(d, type = "rolling", time = "day", initial = 180,
                  assess = 1, gap = 1, skip = 9)
  expect_match(flow_describe(m), "for the first two and the last of 19 splits")
  # ticks show months only, so the axis title gives the year
  expect_equal(lay$axis_title$label, "Day (2021)")
})

test_that("grids with many folds stay readable", {
  # wide enough for the sizes in the blocks
  a <- grid_layout(flow_folds(k = 10, n = 120))
  expect_true(all(a$blocks$shown == "full"))
  expect_equal(nrow(a$row_counts), 0)
  expect_gt(a$size[["width"]], 5.5)
  # a narrower width leaves the sizes out of the picture and says so
  expect_warning(b <- grid_layout(flow_folds(k = 10, n = 120), width = 5),
                 "fold sizes do not fit")
  expect_equal(nrow(b$row_counts), 0)
  expect_false(any(grepl("n = ", b$blocks$text)))
  expect_true(all(b$blocks$text != "" |
                    b$blocks$role %in% b$legend$role))
  expect_match(flow_describe(flow_folds(k = 10, n = 120)), "n = 120",
               fixed = TRUE)
})

test_that("group and time checks of grids: missing values, text, k, training", {
  d <- data.frame(g = c(1, NA, 2, 3), f = c(1, 1, 2, 2), t = c("a", "b", "c", "d"))
  expect_warning(g <- flow_folds(d, fold = "f", group = "g"),
                 "1 row has a missing \"g\"", fixed = TRUE)
  expect_match(flow_describe(g), "(1 row with missing g not checked)",
               fixed = TRUE)
  expect_error(flow_folds(d, fold = "f", k = 5), "has 2 folds", fixed = TRUE)
  expect_error(flow_folds(d, type = "expanding", time = "t", initial = 2,
                          assess = 1), "must hold dates")
  expect_error(flow_folds(type = "blocked", n = 10, k = 5, gap = 5),
               "split 3 has no training data left", fixed = TRUE)
  ir <- data.frame(m = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01",
                                 "2020-07-01", "2020-08-01")))
  expect_match(flow_describe(flow_folds(ir, type = "expanding", time = "m",
                                        initial = 3, assess = 1)),
               "their spacing is not regular", fixed = TRUE)
  expect_false(grepl("not regular", flow_describe(flow_folds(
    ir[1:3, , drop = FALSE], type = "expanding", time = "m", initial = 1,
    assess = 1))))
})

# The user's own assignments: 3 expanding splits over 12 months, a gap of one
# month, test 2 months; 2 rows per month. Month 12 is in no split.
own <- function() {
  do.call(rbind, lapply(1:3, function(s) {
    tr <- rep(1:(4 + 2 * (s - 1)), each = 2)
    te <- rep((6 + 2 * (s - 1)):(7 + 2 * (s - 1)), each = 2)
    data.frame(split = s, role = rep(c("analysis", "assessment"),
                                     c(length(tr), length(te))),
               month = c(tr, te))
  }))
}
own_roles <- c(Training = "analysis", Test = "assessment")

test_that("own assignments: blocks, counts and gaps are the user's", {
  a <- own()
  g <- flow_folds(a, split = "split", role = "role", roles = own_roles,
                  time = "month", all_times = 1:12)
  check_cover(g)
  expect_equal(g$n_pos, 12)
  for (s in 1:3) {
    b <- g$blocks[g$blocks$split == s, ]
    expect_equal(num(b$n[b$role == "Training"]),
                 sum(a$split == s & a$role == "analysis"))
    expect_equal(num(b$n[b$role == "Test"]),
                 sum(a$split == s & a$role == "assessment"))
    gp <- b[b$role == "Gap", ]
    expect_equal(c(gp$start, gp$end), rep(5 + 2 * (s - 1), 2))
    # rows left out are not in the data: no count, never 0
    expect_true(all(is.na(b$n[b$role %in% c("Gap", "Not used")])))
    expect_equal(flow_data(g, b$id[b$role == "Test"]),
                 a[a$split == s & a$role == "assessment", ])
  }
  expect_false(any(grepl("n = 0", grid_layout(g)$blocks$text)))
  expect_match(flow_describe(g),
               "split 1: training (n = 8), gap (1 value of month), test (n = 4);",
               fixed = TRUE)
  # without a time column each bar is as long as its number of rows
  r <- flow_folds(a, split = "split", role = "role", roles = own_roles)
  expect_equal(r$blocks$end[r$blocks$role == "Test"], c(12, 16, 20))
  expect_output(print(r), "20 rows (longest split)", fixed = TRUE)
  # a role given as Gap keeps its count
  gap <- rbind(a, data.frame(split = 1, role = "embargo", month = 5))
  w <- flow_folds(gap, split = "split", role = "role", time = "month",
                  roles = c(own_roles, Gap = "embargo"), all_times = 1:12)
  expect_equal(w$blocks$n[w$blocks$role == "Gap" & w$blocks$split == 1], "1")
  # split names given by the user are kept
  n <- flow_folds(transform(a, split = paste0("Fold", split)),
                  split = "split", role = "role", roles = own_roles)
  expect_equal(n$splits$label, c("Fold1", "Fold2", "Fold3"))
})

test_that("own assignments: a gap that cannot be seen is warned about", {
  # split 3 alone: months 1 to 8, then 10 and 11; month 9 is in no split
  a <- own()
  a <- a[a$split == 3, ]
  expect_warning(g <- flow_folds(a, split = "split", role = "role",
                                 roles = own_roles, time = "month"),
                 "In split 3 there is time between training and testing")
  expect_match(flow_describe(g), "may not show as a gap", fixed = TRUE)
  expect_false("Gap" %in% g$blocks$role)
  expect_no_warning(w <- flow_folds(a, split = "split", role = "role",
                                    roles = own_roles, time = "month",
                                    all_times = 1:12))
  expect_equal(w$blocks$start[w$blocks$role == "Gap"], 9)
  # no jump, no warning
  full <- data.frame(split = 1, role = own_roles[c(1, 1, 1, 2)], month = 1:4)
  expect_no_warning(flow_folds(full, split = "split", role = "role",
                               roles = own_roles, time = "month"))
  f <- function(x) {
    flow_folds(a, split = "split", role = "role", roles = own_roles,
               time = "month", all_times = x)
  }
  expect_error(f(1:5), "every value of that column")
  expect_error(f(as.Date("2020-01-01") + 1:12), "same kind of values")
  expect_error(f(as.character(1:12)), "must hold dates")
})

test_that("own assignments: group and time checks are real", {
  a <- transform(own(), pid = seq_along(month))
  mk <- function(d, ...) {
    flow_folds(d, split = "split", role = "role", roles = own_roles, ...)
  }
  expect_match(flow_describe(mk(a, group = "pid")),
               "Checked: within each split, all records of each pid have one role.",
               fixed = TRUE)
  leak <- a
  leak$pid[which(leak$split == 2)[c(1, sum(leak$split == 2))]] <- 999
  expect_error(mk(leak, group = "pid"),
               "more than one role within a split: split 2: 1 (e.g. 999)",
               fixed = TRUE)
  late <- a
  late$month[late$split == 3 & late$role == "analysis"][1] <- 11
  expect_error(mk(late, time = "month", all_times = 1:12),
               "In split 3, Training rows run to 11 but Test rows start on 10",
               fixed = TRUE)
  expect_error(mk(transform(a, role = "other")), "not mapped by `roles`")
  expect_error(flow_folds(a, split = "split"), "together")
  expect_error(mk(a, initial = 3), "leave out")
  expect_error(flow_folds(k = 3, all_times = 1:3), "go with `split`")
  expect_error(mk(a, all_times = 1:12), "needs `time`")
})

test_that("a check is claimed only when there was something to check", {
  one <- data.frame(split = 1, role = "Training", t = 1:4, g = 1:4)
  txt <- flow_describe(flow_folds(one, split = "split", role = "role",
                                  time = "t", group = "g"))
  expect_false(grepl("Checked", txt))
  expect_match(txt, "nothing to check", fixed = TRUE)
  two <- transform(one, role = c("Training", "Training", "Validation", "Test"))
  expect_match(flow_describe(flow_folds(two, split = "split", role = "role",
                                        time = "t")),
               "rows are in time order by t, training, then validation, then test.",
               fixed = TRUE)
  # split names that are numbers written as text keep number order
  many <- do.call(rbind, lapply(c("1", "2", "10"), function(s) {
    transform(two, split = s)
  }))
  expect_equal(flow_folds(many, split = "split", role = "role")$splits$label,
               paste("Split", c(1, 2, 10)))
  # both ends of the time range are written alike
  tm <- data.frame(t = as.POSIXct("2023-01-01", tz = "UTC") + 3600 * (0:9))
  expect_match(flow_describe(flow_folds(tm, type = "expanding", time = "t",
                                        initial = 5, assess = 1)),
               "from 2023-01-01 00:00:00 to 2023-01-01 09:00:00",
               fixed = TRUE)
})

test_that("thin blocks with no wide neighbour keep their true width", {
  b <- data.frame(split = 1, role = rep(c("Training", "Test"), 50),
                  start = 1:100)
  b$xmin <- (b$start - 1) * 0.055
  b$xmax <- b$start * 0.055
  w <- widen_blocks(b, 0.12)
  expect_false(any(w$widened))
  expect_equal(w$draw_xmin, b$xmin)
  expect_equal(w$draw_xmax, b$xmax)
})
