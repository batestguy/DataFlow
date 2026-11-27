# Machine-learning steps: data splits (partitions) and outcome events -------
#
# studydesignflow documents a split the user already made; it never splits data at
# random or fits models. In data mode it checks what can be checked (no
# group in two sets, sets in time order) and computes counts and events.
#
# * x$events: outcome events per box, a character vector named by box id
#   (only present when events are used, so other flows are unchanged).
# * x$events_typed: ids of boxes whose events were typed in a flow with data.
# * x$partition: per partition stage id (as text), what the split claims:
#   group, time (column names or text), whether that was checked against
#   data.

# Numbers of rows (or `count` units) among `rows` with and without the
# event. Rows where the outcome is missing (the formula gives NA) are
# neither: they count in n only.
count_events <- function(x, rows, col = x$data$count) {
  if (length(rows) == 0) return(c(events = 0, non_events = 0))
  v <- eval_condition(x, rows, x$data$events, "events", keep_na = TRUE)
  c(events = count_rows(x, rows[!is.na(v) & v], col),
    non_events = count_rows(x, rows[!is.na(v) & !v], col))
}

# Rows of all data frames whose outcome is missing under `events`.
count_missing_outcome <- function(x) {
  all_rows <- seq_len(sum(vapply(x$data$sources, nrow, integer(1))))
  if (length(all_rows) == 0) return(0)
  sum(is.na(eval_condition(x, all_rows, x$data$events, "events",
                           keep_na = TRUE)))
}

# Rows with a missing outcome still in the flow: those left in the end boxes.
# Boxes without data have no rows and add none.
missing_outcome_left <- function(x) {
  if (is.null(x$data$events)) return(0)
  r <- unique(unlist(lapply(x$leaves, function(id) left_rows(x, id))))
  if (length(r) == 0) return(0)
  sum(is.na(eval_condition(x, r, x$data$events, "events", keep_na = TRUE)))
}

# Typed events for boxes `ids`: one value, or one per box.
set_events <- function(x, ids, events, per = NULL, per_target = NULL) {
  vals <- if (!is.null(per_target)) {
    per_box_values(events, format_n, per_target, "events", per)
  } else {
    recycle_values(format_n(events), length(ids), "events")
  }
  x$events[as.character(ids)] <- vals
  if (has_data(x)) {
    x$events_typed <- unique(c(x$events_typed, ids[!is.na(vals)]))
  }
  warn_events(x, ids)
  x
}

# Typed events that cannot be right: negative, or more than the box count.
warn_events <- function(x, ids) {
  num <- function(v) suppressWarnings(as.numeric(gsub(",", "", v)))
  e <- num(x$events[as.character(ids)])
  n <- num(x$nodes$n[match(ids, x$nodes$id)])
  bad <- which((!is.na(e) & e < 0) | (!is.na(e) & !is.na(n) & e > n))
  if (length(bad) > 0) {
    i <- bad[1]
    warning("Box \"", x$nodes$label[x$nodes$id == ids[i]], "\" has ",
            x$events[[as.character(ids[i])]], " events",
            if (!is.na(n[i])) paste0(" but n = ", x$nodes$n[x$nodes$id == ids[i]]),
            "; events must be between 0 and n.", call. = FALSE)
  }
  invisible()
}

#' Document a data split for a machine-learning model
#'
#' Draws the split of the data into sets, such as training, validation and
#' test, as one box per set. studydesignflow does not make the split: it shows the
#' one you made. In a flow with data, `by` names the column that says which
#' set each row is in, the counts are computed, and the split is checked:
#'
#' * `group`: a column such as `"patient_id"`. Every group must sit in one
#'   set only (no patient in both training and test); otherwise an error says
#'   which sets share how many groups.
#' * `time`: a date or time column. The sets must follow each other in time,
#'   in the order of the boxes: every row of an earlier set strictly before
#'   every row of the next. Each box gets its date range as its note, unless
#'   a `note` is given.
#'
#' Without data, `group` and `time` are text recorded for the description,
#' which says they were not checked against data.
#'
#' @inheritParams flow_split
#' @param stage Name of the new stage (row label), by default `"Data split"`.
#' @param labels The sets, e.g. `c("Training", "Test")`. With `by`, as in
#'   [flow_split()]: optional; may map readable names to the values of the
#'   column, e.g. `c("Training" = "train", "Test" = "test")`.
#' @param by For a flow with data: the column that says which set each row
#'   is in.
#' @param group Optional column (data mode) or text (typed mode) for grouped
#'   splits, e.g. `"patient_id"`.
#' @param time Optional date or time column (data mode) or text (typed mode)
#'   for time-ordered splits.
#' @param events Optional typed numbers of outcome events per new box,
#'   recycled like `n`. In a flow started with `events =` as a formula they
#'   are computed instead; typed values win and are marked as typed.
#' @return The updated `studydesignflow`.
#' @export
#' @examples
#' flow_start("Patients", n = 1000) |>
#'   flow_partition(labels = c("Training", "Test"), n = c(800, 200),
#'                  group = "patient") |>
#'   flow_describe()
#'
#' visits <- data.frame(
#'   patient = c(1, 1, 2, 3, 3, 4, 5, 6),
#'   date = as.Date("2020-01-01") + c(0, 30, 60, 90, 95, 400, 420, 500),
#'   set = c("train", "train", "train", "train", "train", "test", "test",
#'           "test")
#' )
#' flow_start("Visits", data = visits) |>
#'   flow_partition(labels = c("Training" = "train", "Test" = "test"),
#'                  by = "set", group = "patient", time = "date")
flow_partition <- function(.flow, stage = "Data split", labels = NULL,
                           n = NULL, note = NULL, by = NULL, group = NULL,
                           time = NULL, from = NULL, row_label = NULL,
                           events = NULL, unit = NULL, count = NULL,
                           details = NULL, kind = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  check_details_arg(details)
  check_kind_arg(kind)
  if (!is.null(group)) check_string(group, "group")
  if (!is.null(time)) check_string(time, "time")
  x <- split_boxes(.flow, stage, labels, n, note, from,
                   pick_row_label(row_label, stage), "partition", "all",
                   by = by, events = events, unit = unit, count = count)
  sid <- max(x$stages$id)
  ids <- x$nodes$id[x$nodes$stage == sid]
  checked <- has_data(x) && !is.null(by)
  na_group <- 0
  if (checked && (!is.null(group) || !is.null(time))) {
    # The checks run across every box of the new stage: boxes with the same
    # label (e.g. "Training" under two arms) form one set, so a patient in
    # Training under one arm and Test under another is caught.
    labs <- x$nodes$label[match(ids, x$nodes$id)]
    sets <- unique(labs)
    set_rows <- lapply(sets, function(l) {
      unlist(lapply(ids[labs == l], function(k) box_rows(x, k)))
    })
    where <- paste0("the boxes of stage \"", stage, "\"")
    if (!is.null(group)) {
      na_group <- check_partition_groups(x, set_rows, sets, group, where)
    }
    if (!is.null(time)) {
      check_partition_time(x, set_rows, sets, time, where)
      if (is.null(note)) {
        x$nodes$note[match(ids, x$nodes$id)] <- vapply(ids, function(k) {
          time_range_text(x, box_rows(x, k), time)
        }, character(1))
      }
    }
  }
  x$partition[[as.character(sid)]] <- list(group = group, time = time,
                                          checked = checked,
                                          na_group = na_group)
  add_kind(add_details(validate_studydesignflow(x), details), kind)
}

# No group (e.g. patient) may have rows in two sets. Returns the number of
# rows with a missing group value (not checked; a warning says so).
check_partition_groups <- function(x, rows, labs, group, where) {
  n_na <- 0
  vals <- lapply(rows, function(r) {
    v <- data_values(x, r, group)
    n_na <<- n_na + sum(is.na(v))
    unique(as.character(v[!is.na(v)]))
  })
  if (n_na > 0) {
    warning(n_na, " row", if (n_na != 1) "s have" else " has",
            " a missing \"", group, "\"; ", if (n_na != 1) "they" else "it",
            " cannot be checked for a grouped split.", call. = FALSE)
  }
  pairs <- character()
  total <- character()
  for (i in seq_along(vals)) for (j in seq_along(vals)) {
    if (i < j) {
      shared <- intersect(vals[[i]], vals[[j]])
      if (length(shared) > 0) {
        pairs <- c(pairs, paste0(labs[i], " and ", labs[j], " share ",
                                 length(shared)))
        total <- union(total, shared)
      }
    }
  }
  if (length(pairs) > 0) {
    stop(length(total), " value", if (length(total) != 1) "s",
         " of \"", group, "\" (e.g. ", total[1], ") ",
         if (length(total) != 1) "are" else "is",
         " in more than one set across ", where, ": ",
         paste(pairs, collapse = "; "), ". A grouped split keeps all rows ",
         "of each ", group, " in one set.", call. = FALSE)
  }
  n_na
}

# A time column must be a date, a date-time or a number: text or factors
# would be ordered alphabetically ("10" before "9").
check_time_values <- function(v, time) {
  if (is.factor(v) || is.character(v) || is.logical(v) ||
      !(is.numeric(v) || inherits(v, c("Date", "POSIXt")))) {
    stop("Column \"", time, "\" must hold dates, date-times or numbers, ",
         "so that it can be put in time order; it holds ",
         class(v)[1], " values. Convert it first, e.g. with as.Date() or ",
         "as.numeric().", call. = FALSE)
  }
  invisible(v)
}

time_range_text <- function(x, r, time) {
  if (length(r) == 0) return(NA_character_)
  v <- data_values(x, r, time)
  f <- format_time(c(min(v), max(v)))
  paste(f[1], "to", f[2])
}

# Sets must follow each other in time, in box order: every row of an
# earlier set strictly before every row of the next.
check_partition_time <- function(x, rows, labs, time, where) {
  rng <- lapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    if (length(r) == 0) return(NULL)
    v <- check_time_values(data_values(x, r, time), time)
    if (anyNA(v)) {
      stop("Column \"", time, "\" has missing values in the set \"",
           labs[i], "\".", call. = FALSE)
    }
    range(v)
  })
  filled <- which(!vapply(rng, is.null, NA))
  for (a in seq_along(filled)[-1]) {
    i <- filled[a - 1]
    j <- filled[a]
    if (!(rng[[i]][2] < rng[[j]][1])) {
      stop("The sets across ", where, " are not in time order by \"",
           time, "\": ", labs[i], " runs to ", format_time(rng[[i]][2]),
           " but ", labs[j], " starts on ", format_time(rng[[j]][1]),
           ". Each set must end before the next one starts.",
           call. = FALSE)
    }
  }
  invisible()
}

# Times as text: numbers without thousands separators (years read 2001,
# not 2,001); dates and date-times formatted together so both ends of a
# range look alike.
format_time <- function(v) {
  if (is.numeric(v) && !inherits(v, c("Date", "POSIXt"))) {
    return(format(v, digits = 15, scientific = FALSE, trim = TRUE))
  }
  format(v)
}
