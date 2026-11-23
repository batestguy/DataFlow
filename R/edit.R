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
#' `flow_edit()` changes text only. It cannot remove boxes or branches, or
#' move them: change the structure with the verbs and their `from =`
#' argument instead.
#'
#' @param .flow A `dataflow` object.
#' @param box Box ids or labels (see [flow_nodes()]). A label edits every box
#'   with that label. With `excluded = TRUE`, exclusion boxes instead (ids
#'   from `.flow$side$id`).
#' @param label,n,note New text, count or note: one value for all chosen
#'   boxes, or one per box, in the order of `box`. When a label in `box`
#'   matches several boxes, give one value only (or use ids). `NULL` leaves it unchanged; `NA` clears a count or
#'   note. Counts are drawn as given, like typed counts.
#' @param stage,row_label To change a row label instead: the stage name and
#'   its new row label (`""` hides it).
#' @param excluded If `TRUE`, `box` names exclusion boxes.
#' @return The updated `dataflow`.
#' @export
#' @examples
#' d <- flow_start("Assessed", n = 412) |>
#'   flow_split("Arm", c("arm_1", "arm_2"), n = 206)
#' d |>
#'   flow_edit(c("arm_1", "arm_2"), label = c("Exercise", "Usual care")) |>
#'   flow_edit(stage = "Arm", row_label = "Allocation") |>
#'   flow_nodes()
flow_edit <- function(.flow, box = NULL, label = NULL, n = NULL, note = NULL,
                      stage = NULL, row_label = NULL, excluded = FALSE) {
  check_flow(.flow)
  if (!is.null(stage) || !is.null(row_label)) {
    if (!is.null(box)) {
      stop("Edit boxes (`box`) or a row label (`stage`, `row_label`) in ",
           "one call, not both.", call. = FALSE)
    }
    check_string(stage %||% NA_character_, "stage")
    check_string(row_label %||% NA_character_, "row_label")
    i <- match(stage, .flow$stages$name)
    if (is.na(i)) {
      stop("There is no stage \"", stage, "\".", call. = FALSE)
    }
    .flow$stages$row_label[i] <- row_label
    return(validate_dataflow(.flow))
  }
  if (is.null(box)) {
    stop("Give `box` (ids or labels), or `stage` and `row_label`.",
         call. = FALSE)
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
    if (length(many) > 0 && (per_box(label) || per_box(n) || per_box(note))) {
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
    rec <- data.frame(what = what, id = ids, stringsAsFactors = FALSE)
    .flow$edited <- unique(rbind(.flow$edited, rec))
  }
  if (!is.null(note)) {
    if (!is.character(note) && !all(is.na(note))) {
      stop("`note` must be text.", call. = FALSE)
    }
    tab$note[rows] <- recycle_values(as.character(note), k, "note")
  }
  .flow[[what]] <- tab
  validate_dataflow(.flow)
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
  one <- function(tab, what, rows_of) {
    ed <- edited_ids(x, what)
    keep <- vapply(seq_len(nrow(tab)), function(i) {
      if (is.na(tab$n[i])) return(FALSE)
      if (tab$id[i] %in% ed) return(TRUE)
      r <- rows_of(tab$id[i])
      is.null(r) || !identical(tab$n[i], format_n(count_rows(x, r)))
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
  rbind(one(x$nodes, "nodes", function(id) box_rows(x, id)),
        one(sd, "side", function(id) x$data$rows$side[[as.character(id)]]),
        reasons)
}
