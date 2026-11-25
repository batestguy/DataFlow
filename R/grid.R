# Fold grids: cross-validation and time-series evaluation designs -----------
#
# A second diagram type next to the flow diagram. A `studydesignflow_grid` is pure
# data: one row per split (fold or forecast origin), each made of blocks
# with a role (Training, Validation, Test, Gap, Not used), a start and end
# position (fold number, or time period), and a count. Like the flow
# diagram, it documents a split the user made; it never makes one.
#
# * type: "kfold", "blocked", "expanding", "rolling" or "assigned" (the
#   user's own assignments, R/grid_assign.R).
# * axis: "folds" (columns = folds), "time" (positions = time periods) or
#   "rows" (assignments without a time column).
# * splits: id, label ("Split 1").
# * blocks: id, split, role, start, end, n (text), label (text drawn, NA
#   for the role name), plus rows behind each block in data mode.
# * times: in data mode with a time axis, the distinct time values (one per
#   period), so positions can be drawn as dates.
# * settings: what the design claims (initial, assess, skip, gap, k, method,
#   group, time unit), for the description.

grid_roles <- c("Training", "Validation", "Test", "Gap", "Not used")

new_grid <- function(type, axis, blocks, settings, n_pos, times = NULL,
                     data = NULL) {
  splits <- unique(blocks$split)
  rows <- attr(blocks, "rows")
  fold_names <- attr(blocks, "fold_names")
  attr(blocks, "rows") <- NULL
  attr(blocks, "fold_names") <- NULL
  if (!is.null(data)) data$rows <- rows
  structure(list(
    type = type, axis = axis,
    splits = data.frame(id = splits, label = paste("Split", splits),
                        stringsAsFactors = FALSE),
    blocks = blocks, n_pos = n_pos, times = times, settings = settings,
    fold_names = fold_names %||% as.character(seq_len(n_pos)),
    data = data, edited = integer()
  ), class = "studydesignflow_grid")
}

whole_number <- function(v, arg, min = 0) {
  if (!is.numeric(v) || length(v) != 1 || is.na(v) || v != round(v) ||
      v < min) {
    stop("`", arg, "` must be a whole number of ", min, " or more.",
         call. = FALSE)
  }
  as.integer(v)
}

#' Draw cross-validation folds or time-series evaluation splits as a grid
#'
#' A second kind of diagram: one row per split, showing which part of the
#' data is used for training, validation and testing. studydesignflow documents
#' the split you made; it does not split data or fit models.
#'
#' **Cross-validation** (`type = "kfold"`): `k` folds; in row `i`, fold `i`
#' is the test set and the other folds are used for training (with
#' `validation = TRUE`, the next fold is the validation set). Typed: give
#' `k` and, optionally, the rows per fold in `n`. From data: `fold` names
#' the column holding the fold each row is tested in; counts are computed
#' and, with `group`, checked: no group (e.g. patient) may sit in two folds.
#'
#' **Time series**:
#'
#' * `type = "expanding"`: training starts with the first `initial`
#'   periods and grows; `type = "rolling"`: training is a window of
#'   `initial` periods that moves forward.
#' * Each test window holds the next `assess` periods, after `gap` periods
#'   left out between training and test (an embargo).
#' * Successive forecast origins are `skip + 1` periods apart.
#' * `type = "blocked"`: the periods are cut into `k` contiguous blocks;
#'   each block is the test set once, the others are used for training, and
#'   `gap` periods on each side of the test block are left out. Note that
#'   blocked cross-validation trains on data after the test block too, so
#'   it is not an evaluation of forecasting.
#'
#' How this compares with the rsample package: `initial`, `assess`, `skip`
#' and `type = "expanding"` / `"rolling"` match `rolling_origin(initial,
#' assess, skip, cumulative = TRUE / FALSE)`. studydesignflow's `gap` is an embargo
#' between training and test, comparable to the `sliding_*()` functions'
#' `assess_start = gap + 1`; it is not `rolling_origin(lag =)`, which makes
#' the test window overlap training. `skip` follows `rolling_origin()`, not
#' the `sliding_*()` functions. rsample counts rows; studydesignflow's data mode
#' counts distinct time values, so with several rows per time value a
#' period is a time value, not a row.
#'
#' Typed: `n` is the number of time periods (or give `n_splits`). From data:
#' `time` names a date, date-time or number column; each distinct value is
#' one period, and counts are the rows in each window. These settings
#' describe a design: studydesignflow draws the windows they define, it does not
#' read your resampling code, so check the picture against it.
#'
#' **Your own split assignments** (`split =`, `role =`): to document the
#' split you actually made, pass a data frame with one row per record per
#' split (for example the output of your resampling code reshaped to long
#' form), with the split in column `split` and the record's role in column
#' `role`. `roles` maps the values of `role` to `"Training"`,
#' `"Validation"`, `"Test"` and `"Gap"`, e.g.
#' `c(Training = "analysis", Test = "assessment")`; every value must be
#' mapped. Then the checks are real: with `group`, no group may have two
#' roles within a split; with `time`, in every split all training rows come
#' before all validation and test rows. With `time`, the grid is drawn on a
#' time axis: time values a split does not use are shown as not used, or as
#' a gap between training and validation or test, without a count (the rows
#' left out are not in `data`). A time value that is in no split at all
#' cannot be seen in `data`; give `all_times`, all time values of the
#' original data, so that it is drawn too. Without `time`, each bar is as
#' long as the split's number of rows.
#'
#' Every block carries its role as text (abbreviated, or explained in a
#' legend when it does not fit), so the grid can be read without colour.
#' Counts can be changed by hand with [flow_edit()]; they are then marked as
#' typed.
#'
#' @param data Optional data frame to compute the blocks from.
#' @param type `"kfold"`, `"expanding"`, `"rolling"` or `"blocked"`
#'   (ignored with `split`).
#' @param k Number of folds or blocks (`"kfold"` without data, `"blocked"`).
#'   With `data` and `fold`, it must agree with the data if given.
#' @param split,role Your own assignments: columns of `data` giving the
#'   split and each record's role in it (see Details).
#' @param roles With `split` and `role`: named vector mapping role names
#'   (`Training`, `Validation`, `Test`, `Gap`) to values of `role`. By
#'   default the values must be those names.
#' @param all_times With `split`, `role` and `time`: all time values of the
#'   original data, e.g. `unique(visits$date)`. Needed to draw a gap made of
#'   time values that no split uses; a warning says when one may be missing.
#' @param n Typed mode: for `"kfold"`, rows per fold (one value, or one per
#'   fold); for time types, the number of time periods.
#' @param fold Data mode, `"kfold"`: column with the fold each row is in.
#' @param time Data mode, time types: date or time column.
#' @param initial,assess Time types: periods in the first training window
#'   and in each test window.
#' @param skip Time types: forecast origins are `skip + 1` periods apart.
#' @param gap Time types: periods left out between training and test (on
#'   both sides of the test block for `"blocked"`).
#' @param n_splits Time types: number of splits; by default as many as fit.
#' @param time_unit Word for one period in the description and axis title,
#'   e.g. `"months"`.
#' @param labels Text for the roles, a named vector overriding any of
#'   `c(Training = "Training", Validation = "Validation", Test = "Test",
#'   Gap = "Gap", "Not used" = "Not used")`.
#' @param validation `"kfold"`: also mark the next fold as validation set.
#' @param method Optional text saying how folds were made (e.g.
#'   `"stratified by outcome"`), for the description only.
#' @param group Optional column (with `fold`, or with `split` and `role`:
#'   checked) or text (otherwise; described as not checked) saying that all
#'   rows of each group stay together. Rows with a missing group value
#'   cannot be checked; a warning and the description say how many.
#' @return A `studydesignflow_grid`. Draw it with `plot()` or [autoplot()], save it
#'   with [flow_save()], describe it with [flow_describe()].
#' @export
#' @examples
#' flow_folds(k = 5, n = 82) |> flow_describe()
#'
#' # monthly data, expanding window with a one-month gap
#' g <- flow_folds(type = "expanding", n = 48, initial = 24, assess = 3,
#'                 skip = 2, gap = 1, time_unit = "months")
#' flow_describe(g)
flow_folds <- function(data = NULL, type = c("kfold", "expanding",
                                             "rolling", "blocked"),
                       k = NULL, n = NULL, fold = NULL, time = NULL,
                       initial = NULL, assess = NULL, skip = 0, gap = 0,
                       n_splits = NULL, time_unit = "periods",
                       labels = NULL, validation = FALSE, method = NULL,
                       group = NULL, split = NULL, role = NULL,
                       roles = NULL, all_times = NULL) {
  type <- match.arg(type)
  check_string(time_unit, "time_unit")
  if (!is.null(method)) check_string(method, "method")
  if (!is.null(group)) check_string(group, "group")
  role_text <- grid_role_text(labels)
  dname <- NULL
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("`data` must be a data frame.", call. = FALSE)
    }
    dname <- paste(deparse(substitute(data), width.cutoff = 500L),
                   collapse = " ")
  }
  settings <- list(k = k, initial = initial, assess = assess, skip = skip,
                   gap = gap, time_unit = time_unit, method = method,
                   group = group, group_checked = FALSE,
                   role_text = role_text, validation = validation,
                   fold = fold, time = time, na_group = 0)
  if (!is.null(split) || !is.null(role)) {
    if (is.null(data) || is.null(split) || is.null(role)) {
      stop("Your own assignments need `data`, `split` and `role` together.",
           call. = FALSE)
    }
    if (!is.null(c(k, n, fold, initial, assess, n_splits)) ||
        !identical(as.numeric(skip), 0) || !identical(as.numeric(gap), 0) ||
        isTRUE(validation)) {
      stop("With `split` and `role` the grid comes from your own ",
           "assignments; leave out `k`, `n`, `fold`, `initial`, `assess`, ",
           "`skip`, `gap`, `n_splits` and `validation`.", call. = FALSE)
    }
    return(grid_from_assignments(data, dname, split, role, roles, time,
                                 group, settings, all_times))
  }
  if (!is.null(roles) || !is.null(all_times)) {
    stop("`roles` and `all_times` go with `split` and `role`.", call. = FALSE)
  }
  if (type == "kfold") {
    out <- if (is.null(data)) {
      kfold_typed(k, n, validation)
    } else {
      kfold_data(data, fold, group, validation, k)
    }
    settings$k <- out$k
    settings$group_checked <- !is.null(data) && !is.null(group)
    settings$na_group <- out$na_group %||% 0
    g <- new_grid("kfold", "folds", out$blocks, settings, out$k,
                  data = if (!is.null(data)) list(name = dname, df = data))
    return(g)
  }
  if (!is.null(data)) {
    if (is.null(time)) {
      stop("With `data`, time-series grids need `time =` a date or time ",
           "column.", call. = FALSE)
    }
    if (!time %in% names(data)) {
      stop("Column \"", time, "\" is not in `data`.", call. = FALSE)
    }
    tv <- check_time_values(data[[time]], time)
    if (anyNA(tv)) {
      stop("Column \"", time, "\" has missing values; remove them first.",
           call. = FALSE)
    }
    times <- sort(unique(tv))
    pos <- match(tv, times)
    total <- length(times)
    if (!is.null(n)) {
      stop("With `data`, the number of periods comes from the data; leave ",
           "`n` out.", call. = FALSE)
    }
  } else {
    times <- NULL
    pos <- NULL
    total <- if (is.null(n)) NULL else whole_number(n, "n", 1)
  }
  win <- if (type == "blocked") {
    blocked_windows(total, k, gap)
  } else {
    time_windows(type, total, initial, assess, skip, gap, n_splits)
  }
  settings$k <- k
  blocks <- windows_to_blocks(win$windows, win$total)
  if (!is.null(pos)) {
    rows <- lapply(seq_len(nrow(blocks)), function(i) {
      which(pos >= blocks$start[i] & pos <= blocks$end[i])
    })
    blocks$n <- format_n(lengths(rows))
    attr(blocks, "rows") <- rows
    # training must end before testing starts, in every split
    for (s in unique(blocks$split)) {
      b <- blocks[blocks$split == s, ]
      tr <- b$end[b$role == "Training"]
      te <- b$start[b$role == "Test"]
      if (type != "blocked" && length(tr) && length(te) && max(tr) >= min(te)) {
        stop("Internal error: training does not end before testing ",
             "starts in split ", s, ". Please report this.", call. = FALSE)
      }
    }
  }
  new_grid(type, "time", blocks, settings, win$total, times = times,
           data = if (!is.null(data)) list(name = dname, df = data))
}

grid_role_text <- function(labels) {
  out <- stats::setNames(grid_roles, grid_roles)
  if (is.null(labels)) return(out)
  if (!is.character(labels) || is.null(names(labels)) ||
      !all(names(labels) %in% grid_roles) || anyNA(labels) ||
      any(labels == "")) {
    stop("`labels` must be text named by roles: ",
         paste0("\"", grid_roles, "\"", collapse = ", "), ".", call. = FALSE)
  }
  out[names(labels)] <- labels
  out
}

kfold_typed <- function(k, n, validation) {
  if (is.null(k)) stop("Give `k`, the number of folds.", call. = FALSE)
  k <- whole_number(k, "k", 2)
  if (isTRUE(validation) && k < 3) {
    stop("A validation fold needs `k` of 3 or more.", call. = FALSE)
  }
  n_vals <- recycle_values(format_n(n), k, "n")
  blocks <- kfold_blocks(k, validation)
  blocks$n <- n_vals[blocks$start]
  list(k = k, blocks = blocks)
}

kfold_blocks <- function(k, validation) {
  b <- expand.grid(start = seq_len(k), split = seq_len(k))
  b$end <- b$start
  b$role <- ifelse(b$start == b$split, "Test", "Training")
  if (isTRUE(validation)) {
    b$role[b$start == (b$split %% k) + 1] <- "Validation"
  }
  data.frame(id = seq_len(nrow(b)), split = b$split, role = b$role,
             start = b$start, end = b$end, n = NA_character_,
             label = NA_character_, stringsAsFactors = FALSE)
}

kfold_data <- function(data, fold, group, validation, k_given = NULL) {
  if (is.null(fold)) {
    stop("With `data`, cross-validation grids need `fold =` the column ",
         "giving each row's fold.", call. = FALSE)
  }
  check_string(fold, "fold")
  if (!fold %in% names(data)) {
    stop("Column \"", fold, "\" is not in `data`.", call. = FALSE)
  }
  f <- data[[fold]]
  if (anyNA(f)) {
    stop("Column \"", fold, "\" has missing values; every row needs a fold.",
         call. = FALSE)
  }
  lv <- if (is.factor(f)) levels(f)[levels(f) %in% as.character(f)] else
    unique(as.character(sort(f)))
  k <- length(lv)
  if (k < 2) stop("`fold` must have at least 2 folds.", call. = FALSE)
  if (!is.null(k_given) && !identical(as.numeric(k_given), as.numeric(k))) {
    stop("`k` is ", k_given, " but column \"", fold, "\" has ", k,
         " folds. Leave `k` out, or check the fold column.", call. = FALSE)
  }
  if (isTRUE(validation) && k < 3) {
    stop("A validation fold needs 3 or more folds.", call. = FALSE)
  }
  fc <- as.character(f)
  rows_by_fold <- lapply(lv, function(l) which(fc == l))
  na_group <- 0
  if (!is.null(group)) {
    if (!group %in% names(data)) {
      stop("Column \"", group, "\" is not in `data`.", call. = FALSE)
    }
    na_group <- warn_missing_groups(data[[group]], group)
    gv <- lapply(rows_by_fold, function(r) {
      v <- data[[group]][r]
      unique(as.character(v[!is.na(v)]))
    })
    all_g <- unlist(gv)
    shared <- unique(all_g[duplicated(all_g)])
    if (length(shared) > 0) {
      stop(length(shared), " value", if (length(shared) != 1) "s",
           " of \"", group, "\" (e.g. ", shared[1], ") ",
           if (length(shared) != 1) "are" else "is",
           " in more than one fold. Grouped cross-validation keeps all rows ",
           "of each ", group, " in one fold.", call. = FALSE)
    }
  }
  blocks <- kfold_blocks(k, validation)
  blocks$n <- format_n(lengths(rows_by_fold))[blocks$start]
  attr(blocks, "rows") <- rows_by_fold[blocks$start]
  attr(blocks, "fold_names") <- lv
  list(k = k, blocks = blocks, na_group = na_group)
}

warn_missing_groups <- function(v, group) {
  n_na <- sum(is.na(v))
  if (n_na > 0) {
    warning(n_na, " row", if (n_na != 1) "s have" else " has",
            " a missing \"", group, "\"; ", if (n_na != 1) "they" else "it",
            " cannot be checked for a grouped split.", call. = FALSE)
  }
  n_na
}

# Training and test windows (in periods) for expanding / rolling designs.
time_windows <- function(type, total, initial, assess, skip, gap, n_splits) {
  if (is.null(initial) || is.null(assess)) {
    stop("Time-series grids need `initial` (periods in the first training ",
         "window) and `assess` (periods in each test window).",
         call. = FALSE)
  }
  initial <- whole_number(initial, "initial", 1)
  assess <- whole_number(assess, "assess", 1)
  skip <- whole_number(skip, "skip", 0)
  gap <- whole_number(gap, "gap", 0)
  step <- skip + 1L
  if (!is.null(n_splits)) n_splits <- whole_number(n_splits, "n_splits", 1)
  if (is.null(total) && is.null(n_splits)) {
    stop("Give the number of time periods `n`, or `n_splits`.", call. = FALSE)
  }
  test_end <- function(s) initial + s * step + gap + assess
  if (is.null(n_splits)) {
    n_splits <- 0L
    while (test_end(n_splits) <= total) n_splits <- n_splits + 1L
    if (n_splits == 0) {
      stop("No split fits: ", total, " periods are fewer than initial + gap ",
           "+ assess = ", initial + gap + assess, ".", call. = FALSE)
    }
  }
  last <- test_end(n_splits - 1L)
  if (is.null(total)) total <- last
  if (last > total) {
    stop(n_splits, " splits need ", last, " periods, but there are only ",
         total, ".", call. = FALSE)
  }
  s <- seq_len(n_splits) - 1L
  tr_end <- initial + s * step
  tr_start <- if (type == "expanding") rep(1L, n_splits) else tr_end - initial + 1L
  windows <- data.frame(split = s + 1L, tr_start = tr_start, tr_end = tr_end,
                        te_start = tr_end + gap + 1L,
                        te_end = tr_end + gap + assess)
  list(windows = windows, total = as.integer(total))
}

# Blocked cross-validation for time-ordered data: k contiguous blocks.
blocked_windows <- function(total, k, gap) {
  if (is.null(total)) {
    stop("Give the number of time periods `n` (or `data` and `time`).",
         call. = FALSE)
  }
  if (is.null(k)) stop("Give `k`, the number of blocks.", call. = FALSE)
  k <- whole_number(k, "k", 2)
  gap <- whole_number(gap, "gap", 0)
  if (k > total) {
    stop("`k` (", k, ") is more than the ", total, " periods.", call. = FALSE)
  }
  ends <- round(seq_len(k) * total / k)
  starts <- c(1L, utils::head(ends, -1) + 1L)
  # every split must keep some training data
  left <- pmax(0, starts - gap - 1) + pmax(0, total - (ends + gap))
  if (any(left == 0)) {
    s <- which(left == 0)[1]
    stop("With k = ", k, " and gap = ", gap, ", split ", s, " has no ",
         "training data left (its test block and gaps cover all ", total,
         " periods). Use a smaller `gap` or more periods.", call. = FALSE)
  }
  windows <- data.frame(split = seq_len(k), te_start = starts, te_end = ends,
                        tr_start = NA, tr_end = NA)
  attr(windows, "gap") <- gap
  attr(windows, "blocked") <- TRUE
  list(windows = windows, total = as.integer(total))
}

# One row of blocks per split covering periods 1..total, left to right.
windows_to_blocks <- function(w, total) {
  out <- list()
  add <- function(split, role, a, b) {
    if (b >= a) out[[length(out) + 1]] <<- data.frame(
      split = split, role = role, start = a, end = b, stringsAsFactors = FALSE)
  }
  blocked <- isTRUE(attr(w, "blocked"))
  gap <- attr(w, "gap") %||% 0L
  for (i in seq_len(nrow(w))) {
    s <- w$split[i]
    if (blocked) {
      a <- w$te_start[i]
      b <- w$te_end[i]
      gl <- max(1L, a - gap)
      gr <- min(total, b + gap)
      add(s, "Training", 1L, gl - 1L)
      add(s, "Gap", gl, a - 1L)
      add(s, "Test", a, b)
      add(s, "Gap", b + 1L, gr)
      add(s, "Training", gr + 1L, total)
    } else {
      add(s, "Not used", 1L, w$tr_start[i] - 1L)
      add(s, "Training", w$tr_start[i], w$tr_end[i])
      add(s, "Gap", w$tr_end[i] + 1L, w$te_start[i] - 1L)
      add(s, "Test", w$te_start[i], w$te_end[i])
      add(s, "Not used", w$te_end[i] + 1L, total)
    }
  }
  b <- do.call(rbind, out)
  data.frame(id = seq_len(nrow(b)), b, n = NA_character_,
             label = NA_character_, stringsAsFactors = FALSE)
}

#' @export
print.studydesignflow_grid <- function(x, ...) {
  cat("<studydesignflow_grid> ", x$type, ": ", nrow(x$splits), " splits over ",
      x$n_pos, if (x$axis == "folds") " folds" else
        if (x$axis == "rows") " rows (longest split)" else
          paste0(" ", x$settings$time_unit), "\n", sep = "")
  if (!is.null(x$data)) {
    cat("  counts from data: ", x$data$name, " (", nrow(x$data$df),
        " rows)\n", sep = "")
  }
  invisible(x)
}

# flow_edit() for grids: block text and counts; counts marked as typed.
grid_edit <- function(x, box, label, n) {
  if (is.null(box) || !is.numeric(box) || anyNA(box) ||
      !all(box %in% x$blocks$id)) {
    stop("For a fold grid, `box` must be block ids (see x$blocks$id).",
         call. = FALSE)
  }
  i <- match(box, x$blocks$id)
  if (!is.null(label)) {
    if (!is.character(label) || anyNA(label) || any(label == "")) {
      stop("`label` must be text and not empty.", call. = FALSE)
    }
    x$blocks$label[i] <- recycle_values(label, length(i), "label")
  }
  if (!is.null(n)) {
    if (is.logical(n) && all(is.na(n))) n <- as.character(n)
    vals <- recycle_values(format_n(n), length(i), "n")
    if (x$type == "kfold") {
      # a fold has one size: change it in every split where it appears
      for (j in seq_along(i)) {
        same <- which(x$blocks$start == x$blocks$start[i[j]])
        x$blocks$n[same] <- vals[j]
        x$edited <- unique(c(x$edited, x$blocks$id[same]))
      }
    } else {
      x$blocks$n[i] <- vals
      x$edited <- unique(c(x$edited, as.integer(box)))
    }
  }
  x
}

# flow_data() for grids: the rows behind one block.
grid_data <- function(x, box) {
  if (is.null(x$data)) {
    stop("This grid has no data; counts were typed.", call. = FALSE)
  }
  if (length(box) != 1 || !is.numeric(box) || !box %in% x$blocks$id) {
    stop("`box` must be one block id (see x$blocks$id).", call. = FALSE)
  }
  x$data$df[x$data$rows[[match(box, x$blocks$id)]], , drop = FALSE]
}

