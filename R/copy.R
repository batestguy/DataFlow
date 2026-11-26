# Copies: every new box gets all the units of the box above ---------------------

#' Give every unit to each of several boxes (copies)
#'
#' Like [flow_split()], but each new box receives **all** the units still in
#' the box above, not a share of them: two people label the same records,
#' every person tries every treatment, every model is tested on the same
#' cases. The picture marks each copied box with a line in italics, "the same
#' 1,960 as above" (or "copied from the box above" when its count is not what
#' is left there); the description says that each box receives all of them;
#' [flow_check()] expects each copy to have the count of the box above, not a
#' share of it; and a [flow_pool()] that joins copies is expected to count
#' each unit once (a larger count is reported as a possible change of unit).
#' Summary boxes cannot be copied.
#'
#' @inheritParams flow_split
#' @param labels The new boxes: a character vector used under every current
#'   box, or a list with one character vector per current box.
#' @param n Optional counts. By default each copy has the count of what is
#'   left in the box above (its count minus the exclusion boxes hanging off
#'   it), or, in a flow with data, the count of its rows. Give `n` only for
#'   boxes made without data, e.g. when not every unit reached every box;
#'   recycled as in [flow_split()].
#' @param by For a flow with data whose rows are measurements of the same
#'   units (one row per person and treatment, say): a column naming the copy
#'   each row belongs to. Each copy then holds only its own rows, and its
#'   count is of the units in them, so give `count =` in [flow_start()] (for
#'   example the person's id). Labels and order follow the values as in
#'   [flow_split()]; unlike there, the same units under several copies is
#'   what is expected, so no warning is given about it.
#' @return The updated `studydesignflow`.
#' @export
#' @examples
#' flow_start("Records", n = 2000) |>
#'   flow_exclude("Removed", n = 40) |>
#'   flow_copy("Labelling", c("Annotator 1", "Annotator 2")) |>
#'   flow_pool("Adjudication", "Disagreements resolved") |>
#'   flow_describe()
#'
#' # every boy wears both shoe materials, one on each foot
#' flow_start("Boys", n = 10) |>
#'   flow_copy("Shoe material", c("Material A", "Material B")) |>
#'   flow_outcome("Measured", "Wear")
#'
#' # one row per person and stool: each stool box holds its own rows,
#' # and counts people
#' stools <- data.frame(person = rep(1:9, each = 4),
#'                      stool = rep(c("T1", "T2", "T3", "T4"), 9))
#' flow_start("People", data = stools, count = "person") |>
#'   flow_copy("Stool type", by = "stool") |>
#'   flow_nodes()
flow_copy <- function(.flow, stage, labels = NULL, n = NULL, note = NULL,
                      from = NULL, row_label = NULL, events = NULL,
                      by = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  targets <- resolve_targets(.flow, from)
  st_of <- match(.flow$nodes$stage[match(targets, .flow$nodes$id)],
                 .flow$stages$id)
  if (any(.flow$stages$display[st_of] == "summary")) {
    stop("A summary box stands for several boxes, so it cannot be copied as ",
         "one. Draw its stage with display = \"all\", or copy the boxes ",
         "below it.", call. = FALSE)
  }
  if (!is.null(by)) {
    sp <- split_by(.flow, targets, by, labels, "all", n, warn_shared = FALSE)
    targets <- targets[sp$keep]
    labels <- sp$labels[sp$keep]
    n <- sp$n[sp$keep]
    child_rows <- unlist(sp$rows[sp$keep], recursive = FALSE)
  } else {
    if (is.null(labels)) {
      stop("Give the `labels` of the copies (or, for a flow with data, ",
           "`by =` a column).", call. = FALSE)
    }
    k <- if (is.list(labels)) lengths(labels) else
      rep(length(labels), length(targets))
    rows <- if (has_data(.flow)) {
      lapply(targets, function(t) left_rows(.flow, t))
    }
    with_rows <- !vapply(rows %||% list(), is.null, NA)
    if (!is.null(n) && any(with_rows)) {
      stop("Give `n` only for boxes made without data: copies of a box with ",
           "rows have its rows, and their counts are computed.", call. = FALSE)
    }
    if (is.null(n)) {
      n <- lapply(seq_along(targets), function(i) {
        if (isTRUE(with_rows[i])) format_n(count_rows(.flow, rows[[i]]))
        else left_count_text(.flow, targets[i])
      })
    }
    child_rows <- if (has_data(.flow)) rep(rows, k)
  }
  x <- split_boxes(.flow, stage, labels, n, note, targets,
                   pick_row_label(row_label, stage), "copy", "all")
  ids <- x$nodes$id[x$nodes$stage == max(x$stages$id)]
  if (has_data(x)) x <- set_rows(x, ids, child_rows)
  # typed events after the rows, so that typed values win
  if (!is.null(events)) {
    per_target <- if (is.list(labels)) lapply(labels, as.character) else
      rep(list(labels), length(targets))
    x <- set_events(x, ids, events, per = if (!is.list(labels)) length(labels),
                    per_target = per_target)
  }
  validate_studydesignflow(x)
}

# What is left in box `id`, as text: its count minus the exclusion boxes
# hanging off it. A count that is not a plain number is kept as typed when
# nothing was excluded; NA when it cannot be worked out.
left_count_text <- function(x, id) {
  n <- x$nodes$n[x$nodes$id == id]
  sd <- x$side %||% empty_side()
  out_n <- sd$n[sd$parent == id]
  if (length(out_n) == 0) return(n)
  v <- parse_count(c(n, out_n))
  if (anyNA(v)) return(NA_character_)
  format_n(v[1] - sum(v[-1]))
}
