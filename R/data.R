# Counts computed from data ---------------------------------------------------
#
# A flow started with `data =` keeps, in `x$data`:
# * sources: the data frames, stored once; names: what each was called;
# * offsets: row numbers are global (source k's row i is offsets[k] + i), so a
#   pool across streams can hold rows from several data frames;
# * count: NULL to count rows, or a column whose distinct non-missing values
#   are counted (e.g. patients, not visits);
# * rows: per box id (as text), the row numbers behind the box (`box`) and
#   those still in the flow below it (`left`; exclusions remove rows here);
#   per exclusion box id, the rows it removed (`side`). A box made without
#   data has no entry: its count is typed, and automatic counts below it stay
#   blank.
# Flows without data have no `data` element, so they are unchanged.

has_data <- function(x) !is.null(x$data)

# Build the store from flow_start()'s `data` argument. `names` are the
# deparsed expressions, for provenance.
new_data_store <- function(data, k, names, count) {
  if (is.data.frame(data)) data <- list(data)
  if (!is.list(data) || !all(vapply(data, is.data.frame, NA))) {
    stop("`data` must be a data frame, or a list of data frames (one per ",
         "top box).", call. = FALSE)
  }
  if (length(data) != k) {
    stop("There ", if (k == 1) "is 1 top box" else paste("are", k,
                                                         "top boxes"),
         " but ", length(data), " data frame", if (length(data) != 1) "s",
         ". Give one data frame per top box.", call. = FALSE)
  }
  if (!is.null(count)) {
    check_string(count, "count")
    for (i in seq_along(data)) {
      if (!count %in% names(data[[i]])) {
        stop("`count` column \"", count, "\" is not in the data frame `",
             names[i], "`.", call. = FALSE)
      }
    }
  }
  sizes <- vapply(data, nrow, integer(1))
  list(sources = unname(data), names = names,
       offsets = c(0L, cumsum(sizes))[seq_len(k)], count = count,
       rows = list(box = list(), left = list(), side = list()))
}

# Names of the data frames given to flow_start(), from the unevaluated
# argument: `df` -> "df"; `list(a, b)` -> c("a", "b"); a named list -> its
# names; otherwise "<expr>[[i]]".
data_names <- function(expr, data) {
  one <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")
  if (is.data.frame(data)) return(one(expr))
  if (is.call(expr) && identical(expr[[1]], as.name("list"))) {
    args <- as.list(expr)[-1]
    nm <- names(args) %||% rep("", length(args))
    return(ifelse(nm != "", nm, vapply(args, one, "")))
  }
  nm <- names(data) %||% rep("", length(data))
  ifelse(nm != "", nm, paste0(one(expr), "[[", seq_along(data), "]]"))
}

# Which source each global row number belongs to.
row_source <- function(x, ids) findInterval(ids, x$data$offsets + 1L)

# Values of `col` for global rows `ids`, in that order. Rows are kept in
# ascending order, so the parts from each source follow one another. When
# the parts come from several data frames and are not all factors, factors
# are turned into text first (c() of a factor and text would give the
# factor's integer codes).
data_values <- function(x, ids, col) {
  src <- row_source(x, ids)
  parts <- list()
  for (k in unique(src)) {
    d <- x$data$sources[[k]]
    if (!col %in% names(d)) {
      stop("Column \"", col, "\" is not in the data frame `",
           x$data$names[k], "`.", call. = FALSE)
    }
    parts[[length(parts) + 1]] <- d[[col]][ids[src == k] - x$data$offsets[k]]
  }
  if (length(parts) > 1 && !all(vapply(parts, is.factor, NA))) {
    parts <- lapply(parts, function(p) if (is.factor(p)) as.character(p) else p)
  }
  out <- if (length(parts) == 0) NULL else do.call(c, parts)
  if (is.null(out)) {
    # no rows: an empty vector of the column's type
    d <- x$data$sources[[1]]
    if (!col %in% names(d)) {
      stop("Column \"", col, "\" is not in the data frame `",
           x$data$names[1], "`.", call. = FALSE)
    }
    out <- d[[col]][0]
  }
  out
}

check_condition <- function(f, arg) {
  if (!inherits(f, "formula") || length(f) != 2) {
    stop("`", arg, "` must be a one-sided formula, e.g. ~ age < 18 ",
         "(note the ~).", call. = FALSE)
  }
}

# TRUE/FALSE per global row in `ids`: the condition evaluated in the data,
# with the formula's environment as enclosure. Missing results count as FALSE
# (the row does not match), as in subset().
eval_condition <- function(x, ids, f, arg) {
  check_condition(f, arg)
  src <- row_source(x, ids)
  out <- logical(length(ids))
  for (k in unique(src)) {
    sel <- src == k
    d <- x$data$sources[[k]][ids[sel] - x$data$offsets[k], , drop = FALSE]
    v <- tryCatch(eval(f[[2]], d, environment(f)), error = function(e) {
      stop("Could not evaluate `", arg, "` (", deparse(f), ") in the data ",
           "frame `", x$data$names[k], "`: ", conditionMessage(e),
           call. = FALSE)
    })
    if (!is.logical(v) || !(length(v) %in% c(1L, nrow(d)))) {
      stop("`", arg, "` (", deparse(f), ") must give TRUE or FALSE for ",
           "each row.", call. = FALSE)
    }
    v <- rep_len(v, nrow(d))
    out[sel] <- !is.na(v) & v
  }
  out
}

# The count shown for a set of rows: rows, or distinct non-missing values of
# the count column. NULL rows (no data behind the box) give NA.
count_rows <- function(x, ids) {
  if (is.null(ids)) return(NA_real_)
  if (is.null(x$data$count)) return(length(ids))
  v <- data_values(x, ids, x$data$count)
  length(unique(v[!is.na(v)]))
}

box_rows <- function(x, id) x$data$rows$box[[as.character(id)]]
left_rows <- function(x, id) x$data$rows$left[[as.character(id)]]

# Record the rows behind new boxes (NULL for none).
set_rows <- function(x, ids, rows) {
  for (i in seq_along(ids)) {
    key <- as.character(ids[i])
    x$data$rows$box[key] <- list(rows[[i]])
    x$data$rows$left[key] <- list(rows[[i]])
  }
  x
}

# Rows of each target box that are still in the flow; an error naming the
# box if it has none (made without data).
rows_of_targets <- function(x, targets, what) {
  lapply(targets, function(t) {
    r <- left_rows(x, t)
    if (is.null(r)) {
      stop(what, " needs the rows behind box \"",
           x$nodes$label[x$nodes$id == t], "\", but it was made without ",
           "data (typed labels or counts).", call. = FALSE)
    }
    r
  })
}

need_data <- function(x, arg) {
  if (!has_data(x)) {
    stop("`", arg, "` needs a flow with data; start it with ",
         "flow_start(data = ...).", call. = FALSE)
  }
}

#' The rows of data behind a box
#'
#' For flows started with `flow_start(data = ...)`: returns the rows of the
#' data that a box stands for, so every number in the diagram can be traced
#' back to the data and the code that drew it.
#'
#' For a flow box these are the rows that reached it (before any exclusion
#' box hanging off it); for an exclusion box (`excluded = TRUE`), the rows it
#' removed from the flow.
#'
#' @param x A `dataflow` with data.
#' @param box A box id or label (see [flow_nodes()]). A label must match
#'   exactly one box.
#' @param excluded If `TRUE`, `box` names an exclusion box (an id from
#'   `x$side$id`, or its label) instead of a flow box.
#' @return If the flow was started from one data frame, a data frame: the
#'   rows of it behind the box. If it was started from several (one per
#'   starting stream), always a list named by the data frames, with one data
#'   frame per source (empty for sources with no rows in the box), whichever
#'   box is asked for.
#' @export
#' @examples
#' d <- flow_example("trial_data")
#' head(flow_data(d, "Randomised"))
#' nrow(flow_data(d, "Excluded", excluded = TRUE))
flow_data <- function(x, box, excluded = FALSE) {
  check_flow(x)
  if (!has_data(x)) {
    stop("This flow has no data; counts were typed. Start a flow with ",
         "flow_start(data = ...) to trace counts to rows.", call. = FALSE)
  }
  if (length(box) != 1 || is.na(box)) {
    stop("`box` must be one box id or label.", call. = FALSE)
  }
  tab <- if (isTRUE(excluded)) x$side else x$nodes
  what <- if (isTRUE(excluded)) "exclusion box" else "box"
  if (is.numeric(box)) {
    if (!box %in% tab$id) {
      stop("No ", what, " with id ", box, ".", call. = FALSE)
    }
    id <- box
  } else if (is.character(box)) {
    id <- tab$id[tab$label == box]
    if (length(id) == 0) {
      stop("No ", what, " labelled \"", box, "\".", call. = FALSE)
    }
    if (length(id) > 1) {
      stop("Several ", what, "es are labelled \"", box, "\" (ids ",
           paste(id, collapse = ", "), "); give the id instead.",
           call. = FALSE)
    }
  } else {
    stop("`box` must be a box id (number) or label (text).", call. = FALSE)
  }
  rows <- if (isTRUE(excluded)) x$data$rows$side[[as.character(id)]] else
    box_rows(x, id)
  if (is.null(rows)) {
    stop("The ", what, " \"", tab$label[tab$id == id], "\" has no rows ",
         "behind it: it was made without data (typed labels or counts).",
         call. = FALSE)
  }
  src <- row_source(x, rows)
  parts <- lapply(seq_along(x$data$sources), function(k) {
    x$data$sources[[k]][rows[src == k] - x$data$offsets[k], , drop = FALSE]
  })
  if (length(parts) == 1) return(parts[[1]])
  stats::setNames(parts, x$data$names)
}

# "Counts were computed from the data frame `trial_data` (412 rows)."
describe_data <- function(x) {
  dd <- x$data
  sizes <- vapply(dd$sources, nrow, integer(1))
  each <- paste0("`", dd$names, "` (", format_n(sizes),
                 ifelse(sizes == 1, " row)", " rows)"))
  tc <- typed_counts(x)
  flat <- function(s) gsub("[[:space:]]*\n[[:space:]]*", " ", s)
  paste0("Counts were computed from the data frame",
         if (length(each) > 1) "s", " ", and_list(each),
         if (!is.null(dd$count)) paste0(", as numbers of distinct values of `",
                                        dd$count, "`"),
         if (nrow(tc) > 0) paste0(", except ", nrow(tc), " typed by hand: ",
                                  and_list(paste0(flat(tc$label), " (n = ",
                                                  tc$n, ")"))),
         ".")
}

#' Draw the nested structure of a data frame
#'
#' Builds a flow whose whole nested structure and all counts come from the
#' data: a top box for all rows, then one split per column in `levels`
#' (outermost first), each with [flow_split()]`(by = column)`. The result is
#' an ordinary `dataflow`, so you can keep adding steps
#' ([flow_exclude()], [flow_apply()], [flow_outcome()], [flow_pool()]) and
#' change any text or count with [flow_edit()].
#'
#' @param data A data frame.
#' @param levels Column names, outermost first. Each becomes a stage (and a
#'   row label) of the same name.
#' @param label Text of the top box.
#' @param count `NULL` to count rows, or a column whose distinct values are
#'   counted (see [flow_start()]).
#' @param row_labels Optional row labels, one per column in `levels`
#'   (`""` hides one). Default: the column names.
#' @param labels Optional named list giving, for some columns, the `labels`
#'   of [flow_split()] with `by`: the order of the values and readable text,
#'   e.g. `list(exp = c("Ex. 1" = "exp1", "Ex. 2" = "exp2"))`. Planned values
#'   with no rows may be included (count 0).
#' @return A `dataflow` with data.
#' @export
#' @examples
#' cultures <- data.frame(
#'   experiment = rep(c("Ex. 1", "Ex. 2"), each = 50),
#'   batch = rep(rep(c("Batch 1", "Batch 2"), each = 25), 2)
#' )
#' flow_from_data(cultures, c("experiment", "batch"), label = "Cultures") |>
#'   flow_describe()
flow_from_data <- function(data, levels, label = "All", count = NULL,
                           row_labels = NULL, labels = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(levels) || length(levels) == 0 || anyNA(levels) ||
      anyDuplicated(levels)) {
    stop("`levels` must be column names, each once.", call. = FALSE)
  }
  missing <- setdiff(levels, names(data))
  if (length(missing) > 0) {
    stop("Not columns of `data`: ", paste0("\"", missing, "\"",
                                            collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.null(row_labels) &&
      (!is.character(row_labels) || length(row_labels) != length(levels))) {
    stop("`row_labels` must be text, one per column in `levels`.",
         call. = FALSE)
  }
  if (!is.null(labels) && (!is.list(labels) || is.null(names(labels)) ||
                           !all(names(labels) %in% levels))) {
    stop("`labels` must be a list named by columns in `levels`.",
         call. = FALSE)
  }
  x <- flow_start(label, data = data, count = count)
  x$data$names <- paste(deparse(substitute(data), width.cutoff = 500L),
                        collapse = " ")
  for (i in seq_along(levels)) {
    x <- flow_split(x, levels[i], labels = labels[[levels[i]]],
                    by = levels[i], row_label = row_labels[i])
  }
  x
}

# With `count =` a column, the same value (e.g. a patient id) can sit in
# several boxes made from one box (sibling boxes, or an exclusion box and the
# rows that continue). Their counts then add up to more than the box above.
# Warn, so the arithmetic is not silently off.
warn_shared_ids <- function(x, sets, what) {
  col <- x$data$count
  if (is.null(col) || length(sets) < 2) return(invisible())
  vals <- lapply(sets, function(r) {
    v <- data_values(x, r, col)
    unique(as.character(v[!is.na(v)]))
  })
  all_v <- unlist(vals)
  shared <- unique(all_v[duplicated(all_v)])
  if (length(shared) > 0) {
    warning("With count = \"", col, "\", ", length(shared), " value",
            if (length(shared) != 1) "s", " of \"", col, "\" (e.g. ",
            shared[1], ") ", if (length(shared) != 1) "are" else "is",
            " in more than one of ", what, ", so their counts add up to ",
            "more than the box above.", call. = FALSE)
  }
  invisible()
}

# A `where` or `reasons` argument that could not be evaluated: usually a
# condition written without the ~.
bad_condition <- function(arg, e) {
  stop("`", arg, "` must be ",
       if (arg == "reasons") "a named list of one-sided formulas, e.g. list(\"Under 18\" = ~ age < 18)"
       else "a one-sided formula, e.g. ~ age < 18",
       " (note the ~). As written it could not be evaluated: ",
       conditionMessage(e), call. = FALSE)
}
