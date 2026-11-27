# Editing boxes after they are built ----------------------------------------

#' Edit the text, count or note of existing boxes, or a row label
#'
#' Changes what existing boxes say without rebuilding the flow: useful when a
#' diagram drawn from data needs readable names, a planned group, or a count
#' the author must override. Works the same for flows with typed counts.
#'
#' A count changed here is recorded as typed by hand: [flow_nodes()] shows it
#' in its `n_edited` column, and for a flow with data [flow_describe()] lists
#' it among the counts typed rather than computed. Editing never changes the
#' rows of data behind a box, so [flow_data()] still returns them.
#'
#' For a fold grid from [flow_folds()], `box` gives block ids (see
#' `x$blocks`), and `label` and `n` change the text and count of those
#' blocks; edited counts are marked as typed.
#'
#' `flow_edit()` changes text only. It cannot remove boxes or branches, or
#' move them: change the structure with the verbs and their `from =`
#' argument instead.
#'
#' @param .flow A `studydesignflow` object.
#' @param box Box ids or labels (see [flow_nodes()]). A label edits every box
#'   with that label. With `excluded = TRUE`, exclusion boxes instead (ids
#'   from `.flow$side$id`).
#' @param events New number of outcome events (see [flow_start()]), marked
#'   as typed like an edited count. Not for exclusion boxes.
#' @param label,n,note New text, count or note: one value for all chosen
#'   boxes, or one per box, in the order of `box`. When a label in `box`
#'   matches several boxes, give one value only (or use ids). `NULL` leaves it unchanged; `NA` clears a count or
#'   note. Counts are drawn as given, like typed counts.
#' @param unit New unit (what the count counts, see [flow_split()]): one
#'   value for all chosen boxes, or one per box. Boxes below that have no unit
#'   of their own take the new one. `NA` removes a box's own unit, so it takes
#'   the unit of the box above again; `""` gives it none.
#' @param stage,row_label To change a step instead of boxes: the stage name,
#'   and its new row label (`""` hides it). With `stage`, give `row_label`,
#'   `details`, or both.
#' @param details Details of how a step or box was done (see
#'   [step_details()]). With `stage`: one `step_details()` for that step.
#'   With `box`: one for all chosen boxes, or a list with one per box; a box's
#'   own details are for where it differs from its step. `NA` removes the
#'   details (of the step, or of the boxes).
#' @param excluded If `TRUE`, `box` names exclusion boxes.
#' @return The updated `studydesignflow`.
#' @export
#' @examples
#' d <- flow_start("Assessed", n = 412) |>
#'   flow_split("Arm", c("arm_1", "arm_2"), n = 206)
#' d |>
#'   flow_edit(c("arm_1", "arm_2"), label = c("Exercise", "Usual care")) |>
#'   flow_edit(stage = "Arm", row_label = "Allocation") |>
#'   flow_nodes()
#'
#' # who did a step, and a box that used another tool
#' d |>
#'   flow_edit(stage = "Arm", details = step_details(who = "trial nurse")) |>
#'   flow_edit("arm_2", details = step_details(tool = "sealed envelopes")) |>
#'   flow_describe()
flow_edit <- function(.flow, box = NULL, label = NULL, n = NULL, note = NULL,
                      stage = NULL, row_label = NULL, excluded = FALSE,
                      events = NULL, unit = NULL, details = NULL) {
  if (inherits(.flow, "studydesignflow_grid")) {
    if (!is.null(details)) {
      stop("Fold grids do not take step details.", call. = FALSE)
    }
    return(grid_edit(.flow, box, label, n))
  }
  check_flow(.flow)
  if (!is.null(stage) || !is.null(row_label)) {
    if (!is.null(box)) {
      stop("Edit boxes (`box`) or a step (`stage`, with `row_label` or ",
           "`details`) in one call, not both.", call. = FALSE)
    }
    given <- c(label = !is.null(label), n = !is.null(n),
               note = !is.null(note), events = !is.null(events),
               unit = !is.null(unit))
    if (any(given)) {
      stop("`stage` changes a row label or the details of a step only; ",
           and_list(paste0("`", names(given)[given], "`")),
           " need `box`. Edit them in a separate call.", call. = FALSE)
    }
    check_string(stage %||% NA_character_, "stage")
    if (is.null(row_label) && is.null(details)) {
      stop("With `stage`, give `row_label`, `details`, or both.",
           call. = FALSE)
    }
    i <- match(stage, .flow$stages$name)
    if (is.na(i)) {
      stop("There is no stage \"", stage, "\".", call. = FALSE)
    }
    if (!is.null(row_label)) {
      check_string(row_label, "row_label")
      .flow$stages$row_label[i] <- row_label
    }
    if (!is.null(details)) {
      d <- edit_details_value(details, 0)[[1]]
      .flow <- put_details(.flow, "steps", .flow$stages$id[i], d)
    }
    return(validate_studydesignflow(.flow))
  }
  if (is.null(box)) {
    stop("Give `box` (ids or labels), or `stage` with `row_label` or ",
         "`details`.", call. = FALSE)
  }
  what <- if (isTRUE(excluded)) "side" else "nodes"
  tab <- .flow[[what]]
  if (length(box) == 0 || anyNA(box)) {
    stop("`box` is empty or has missing values.", call. = FALSE)
  }
  ids <- if (is.numeric(box)) {
    bad <- setdiff(box, tab$id)
    if (length(bad) > 0) {
      stop("No ", if (isTRUE(excluded)) "exclusion ", "box with id ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
    as.integer(unique(box))
  } else if (is.character(box)) {
    bad <- setdiff(box, tab$label)
    if (length(bad) > 0) {
      stop("No ", if (isTRUE(excluded)) "exclusion ", "box labelled ",
           paste0("\"", bad, "\"", collapse = ", "), ".", call. = FALSE)
    }
    hits <- lapply(box, function(b) tab$id[tab$label == b])
    many <- box[lengths(hits) > 1]
    per_box <- function(v) !is.null(v) && length(v) > 1
    per_box_details <- is.list(details) && !is_details(details) &&
      length(details) > 1
    if (length(many) > 0 && (per_box(label) || per_box(n) || per_box(note) ||
                             per_box(events) || per_box(unit) ||
                             per_box_details)) {
      stop("The label \"", many[1], "\" matches ", max(lengths(hits)),
           " boxes, so one value per box is ambiguous. Give box ids ",
           "(see flow_nodes()) to set different values.", call. = FALSE)
    }
    # in the order given in `box`
    unique(unlist(hits))
  } else {
    stop("`box` must be box ids (numbers) or labels (text).", call. = FALSE)
  }
  rows <- match(ids, tab$id)
  k <- length(ids)
  if (!is.null(label)) {
    if (!is.character(label) || anyNA(label) || any(label == "")) {
      stop("`label` must be text and not empty.", call. = FALSE)
    }
    tab$label[rows] <- recycle_values(label, k, "label")
  }
  if (!is.null(n)) {
    # NA clears a count; a logical NA is not a number or text
    if (is.logical(n) && all(is.na(n))) n <- as.character(n)
    tab$n[rows] <- recycle_values(format_n(n), k, "n")
    # the computed rows without the event do not go with a typed n
    if (!isTRUE(excluded) && !is.null(.flow$non_events)) {
      .flow$non_events[as.character(ids)] <- NA_character_
    }
    rec <- data.frame(what = what, id = ids, stringsAsFactors = FALSE)
    .flow$edited <- unique(rbind(.flow$edited, rec))
  }
  if (!is.null(events)) {
    if (isTRUE(excluded)) {
      stop("Exclusion boxes do not show events.", call. = FALSE)
    }
    if (is.logical(events) && all(is.na(events))) {
      events <- as.character(events)
    }
    vals <- recycle_values(format_n(events), k, "events")
    .flow$events[as.character(ids)] <- vals
    # rows without the event are no longer known for an edited box
    if (!is.null(.flow$non_events)) {
      .flow$non_events[as.character(ids)] <- NA_character_
    }
    warn_events(.flow, ids)
    rec <- data.frame(what = "events", id = ids, stringsAsFactors = FALSE)
    .flow$edited <- unique(rbind(.flow$edited, rec))
  }
  if (!is.null(note)) {
    if (!is.character(note) && !all(is.na(note))) {
      stop("`note` must be text.", call. = FALSE)
    }
    tab$note[rows] <- recycle_values(as.character(note), k, "note")
  }
  .flow[[what]] <- tab
  if (!is.null(unit)) {
    store <- if (isTRUE(excluded)) "side_units" else "units"
    u <- .flow[[store]] %||% character()
    u[as.character(ids)] <- recycle_values(format_unit(unit), k, "unit")
    u <- u[!is.na(u)]
    .flow[[store]] <- if (length(u) > 0) u
  }
  if (!is.null(details)) {
    vals <- edit_details_value(details, k)
    part <- if (isTRUE(excluded)) "side" else "boxes"
    for (j in seq_len(k)) .flow <- put_details(.flow, part, ids[j], vals[[j]])
  }
  validate_studydesignflow(.flow)
}

# The `details` of flow_edit() for `k` steps or boxes: a list of k entries,
# each a step_details() object or NULL (remove). Accepts one value for all
# (step_details() or NA), or a list of k such values.
edit_details_value <- function(details, k) {
  one <- function(d) {
    if (is_details(d)) return(d)
    if (is.null(d) || (is.atomic(d) && length(d) == 1 && is.na(d))) {
      return(NULL)
    }
    stop("`details` must be step_details(), or NA to remove details.",
         call. = FALSE)
  }
  if (is_details(details) || !is.list(details)) {
    return(rep(list(one(details)), max(k, 1)))  # k = 0: a step
  }
  if (!is.null(names(details))) {
    stop("In flow_edit(), `details` is not a named list: choose the step ",
         "with `stage` and the boxes with `box`.", call. = FALSE)
  }
  if (k == 0) {
    stop("A step takes one step_details() (or NA), not a list.",
         call. = FALSE)
  }
  if (length(details) != k) {
    stop("`details` is a list of ", length(details), " but there ",
         if (k == 1) "is 1 box" else paste("are", k, "boxes"), " to edit. ",
         "Give one value, or one per box.", call. = FALSE)
  }
  lapply(details, one)
}

# Ids of boxes whose count was edited by hand ("nodes" or "side").
edited_ids <- function(x, what) {
  e <- x$edited
  if (is.null(e)) integer() else e$id[e$what == what]
}

# Counts drawn in a flow with data that were not computed from it: edited by
# hand, or typed for a box (with or without rows) where the typed value
# differs from, or replaces, the count of its rows; and typed reason counts.
typed_counts <- function(x) {
  cols <- box_count_cols(x)
  # `col_of`: the column each box counts (exclusion boxes: their parent's)
  one <- function(tab, what, rows_of, col_of) {
    ed <- edited_ids(x, what)
    keep <- vapply(seq_len(nrow(tab)), function(i) {
      if (is.na(tab$n[i])) return(FALSE)
      if (tab$id[i] %in% ed) return(TRUE)
      r <- rows_of(tab$id[i])
      is.null(r) ||
        !identical(tab$n[i], format_n(count_rows(x, r, col_of(i))))
    }, NA)
    tab[keep, c("id", "label", "n")]
  }
  # reasons with typed counts (not computed from formulas), e.g. typed
  # reasons listed in an exclusion box computed with `where`
  rs <- x$reasons %||% empty_reasons()
  rs <- rs[!is.na(rs$n) & !rs$side %in% x$data$computed_reasons, ]
  sd <- x$side %||% empty_side()
  reasons <- data.frame(
    id = rs$side,
    label = if (nrow(rs) > 0) {
      paste0(rs$label, " in ", sd$label[match(rs$side, sd$id)])
    } else {
      character()
    },
    n = rs$n, stringsAsFactors = FALSE
  )
  # events typed when the flow was built, or edited, or not matching rows
  ev <- x$events %||% character()
  ev_ids <- as.integer(names(ev))
  ev_typed <- vapply(seq_along(ev), function(i) {
    if (is.na(ev[i])) return(FALSE)
    id <- ev_ids[i]
    if (id %in% c(x$events_typed, edited_ids(x, "events"))) return(TRUE)
    r <- box_rows(x, id)
    is.null(r) || is.null(x$data$events) ||
        !identical(unname(ev[i]),
                 format_n(count_events(x, r, cols[[as.character(id)]])[["events"]]))
  }, NA)
  events <- data.frame(
    id = ev_ids[ev_typed],
    label = x$nodes$label[match(ev_ids[ev_typed], x$nodes$id)],
    n = unname(ev[ev_typed]), stringsAsFactors = FALSE
  )
  out <- rbind(one(x$nodes, "nodes", function(id) box_rows(x, id),
                   function(i) cols[[as.character(x$nodes$id[i])]]),
               one(sd, "side",
                   function(id) x$data$rows$side[[as.character(id)]],
                   function(i) cols[[as.character(sd$parent[i])]]),
               reasons)
  out$what <- rep("n", nrow(out))
  events$what <- rep("events", nrow(events))
  rbind(out, events)
}
