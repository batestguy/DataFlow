# Machine-learning steps: data splits (partitions) and outcome events -------
#
# DataFlow documents a split the user already made; it never splits data at
# random or fits models. In data mode it checks what can be checked (no
# group in two sets, sets in time order) and computes counts and events.
#
# * x$events: outcome events per box, a character vector named by box id
#   (only present when events are used, so other flows are unchanged).
# * x$events_typed: ids of boxes whose events were typed in a flow with data.
# * x$partition: per partition stage id (as text), what the split claims:
#   group, time (column names or text), whether that was checked against
#   data.

# Number of rows (or `count` units) among `rows` with the event.
count_events <- function(x, rows) {
  if (length(rows) == 0) return(0)
  hit <- eval_condition(x, rows, x$data$events, "events")
  count_rows(x, rows[hit])
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
  x
}

#' Document a data split for a machine-learning model
#'
#' Draws the split of the data into sets, such as training, validation and
#' test, as one box per set. DataFlow does not make the split: it shows the
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
#' @return The updated `dataflow`.
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
                           events = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  if (!is.null(group)) check_string(group, "group")
  if (!is.null(time)) check_string(time, "time")
  x <- split_boxes(.flow, stage, labels, n, note, from,
                   pick_row_label(row_label, stage), "partition", "all",
                   by = by, events = events)
  sid <- max(x$stages$id)
  ids <- x$nodes$id[x$nodes$stage == sid]
  checked <- has_data(x) && !is.null(by)
  if (checked && (!is.null(group) || !is.null(time))) {
    parents <- x$edges$from[match(ids, x$edges$to)]
    for (p in unique(parents)) {
      kids <- ids[parents == p]
      rows <- lapply(kids, function(k) box_rows(x, k))
      labs <- x$nodes$label[match(kids, x$nodes$id)]
      where <- x$nodes$label[x$nodes$id == p]
      if (!is.null(group)) check_partition_groups(x, rows, labs, group, where)
      if (!is.null(time)) {
        ranges <- check_partition_time(x, rows, labs, time, where)
        if (is.null(note)) {
          x$nodes$note[match(kids, x$nodes$id)] <- ranges
        }
      }
    }
  }
  x$partition[[as.character(sid)]] <- list(group = group, time = time,
                                          checked = checked)
  validate_dataflow(x)
}

# No group (e.g. patient) may have rows in two sets.
check_partition_groups <- function(x, rows, labs, group, where) {
  vals <- lapply(rows, function(r) {
    v <- data_values(x, r, group)
    unique(as.character(v[!is.na(v)]))
  })
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
         " in more than one set under \"", where, "\": ",
         paste(pairs, collapse = "; "), ". A grouped split keeps all rows ",
         "of each ", group, " in one set.", call. = FALSE)
  }
  invisible()
}

# Sets must follow each other in time, in box order; returns each box's
# date range as text ("2019-01-01 to 2021-12-31"), NA for empty boxes.
check_partition_time <- function(x, rows, labs, time, where) {
  rng <- lapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    if (length(r) == 0) return(NULL)
    v <- data_values(x, r, time)
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
      stop("The sets under \"", where, "\" are not in time order by \"",
           time, "\": ", labs[i], " runs to ", format_time(rng[[i]][2]),
           " but ", labs[j], " starts on ", format_time(rng[[j]][1]),
           ". Each set must end before the next one starts.",
           call. = FALSE)
    }
  }
  vapply(rng, function(r) {
    if (is.null(r)) NA_character_ else
      paste(format_time(r[1]), "to", format_time(r[2]))
  }, character(1))
}

format_time <- function(v) {
  if (is.numeric(v) && !inherits(v, c("Date", "POSIXt"))) format_n(v) else
    format(v)
}
