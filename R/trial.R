# Verbs for trials: assignment to arms and exclusion side boxes -------------

#' Assign the current boxes to groups (arms)
#'
#' Like [flow_split()], but records that the boxes were *assigned* (for
#' example randomised) to the groups. The picture is the same as a split; the
#' method appears in the plain-language description from [flow_describe()]
#' ("randomly assigned to ...").
#'
#' @inheritParams flow_split
#' @param labels The groups (arms), e.g. `c("Intervention", "Control")`.
#'   A list gives different groups under different boxes, as in
#'   [flow_split()].
#' @param method How boxes were assigned. `"random"` (the default) is
#'   described as "randomly assigned"; any other text is described as
#'   "assigned ... by <method>", e.g. `method = "minimisation"`; `NULL` says
#'   only "assigned".
#' @return The updated `dataflow`.
#' @export
#' @examples
#' flow_start("Randomised", n = 300) |>
#'   flow_assign("Allocation", c("Intervention", "Control"), n = 150) |>
#'   flow_describe()
flow_assign <- function(.flow, stage, labels, n = NULL, note = NULL,
                        method = "random", from = NULL, row_label = NULL,
                        display = "all", summary = "{k} {stage}") {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display, summary_ok = TRUE)
  split_boxes(.flow, stage, labels, n, note, from,
              pick_row_label(row_label, stage), "assign", display,
              method = check_method(method), summary = summary)
}

# A method as stored on the stage: NA for none.
check_method <- function(method) {
  if (is.null(method)) return(NA_character_)
  check_string(method, "method")
  if (method == "") {
    stop("`method` must not be empty; use NULL for no method.",
         call. = FALSE)
  }
  method
}

#' Add an exclusion box beside the flow
#'
#' Draws a box to the right of the line leaving each current end box (or each
#' box chosen with `from`), joined to that line by an arrow: the people or
#' units who left the flow at this point, with optional reasons. The flow
#' itself continues below the same boxes, so the next verb attaches to them
#' as before.
#'
#' Counts are drawn exactly as given; they are never checked against the
#' boxes above or below.
#'
#' @inheritParams flow_split
#' @param label Text for the exclusion box(es): one value, or one per box.
#' @param n Optional count for each exclusion box: one value, or one per box.
#' @param reasons Optional reasons, listed in the box under the heading. A
#'   named vector gives reasons with counts, e.g.
#'   `c("Declined" = 12, "Not eligible" = 30)`; an unnamed character vector
#'   gives reasons without counts. Used for every box, or give a list with
#'   one such vector per box.
#' @param note Optional extra line(s) in smaller italic text.
#' @param stage Optional row label for the exclusion row (e.g.
#'   `"Follow-up"`). By default the row has no label.
#' @param from Optional ids or labels of current end boxes to attach to,
#'   instead of all of them. Only end boxes can take an exclusion box.
#' @return The updated `dataflow`.
#' @export
#' @examples
#' flow_start("Assessed for eligibility", n = 412) |>
#'   flow_exclude("Excluded", n = 112,
#'                reasons = c("Did not meet inclusion criteria" = 68,
#'                            "Declined to participate" = 44)) |>
#'   flow_split("Randomisation", "Randomised", n = 300)
flow_exclude <- function(.flow, label = "Excluded", n = NULL, reasons = NULL,
                         note = NULL, stage = NULL, from = NULL) {
  check_flow(.flow)
  targets <- resolve_targets(.flow, from)
  if (!all(targets %in% .flow$leaves)) {
    stop("Exclusion boxes can only hang off current end boxes (see the ",
         "`leaf` column of flow_nodes()).", call. = FALSE)
  }
  k <- length(targets)
  if (!is.character(label) || anyNA(label) || any(label == "")) {
    stop("`label` must be text and not empty.", call. = FALSE)
  }
  labels <- recycle_values(label, k, "label")
  n_vals <- recycle_values(format_n(n), k, "n")
  note_vals <- recycle_values(format_note(note), k, "note")
  reason_list <- if (is.list(reasons)) {
    if (length(reasons) != k) {
      stop("`reasons` is a list of ", length(reasons), " but there are ", k,
           " exclusion boxes. Give one element per box.", call. = FALSE)
    }
    reasons
  } else {
    rep(list(reasons), k)
  }
  reason_list <- lapply(reason_list, parse_reasons)

  if (is.null(stage)) {
    name <- unique_stage_name(.flow, labels[1])
    row_label <- ""
  } else {
    check_string(stage, "stage")
    if (stage == "") {
      stop("`stage` must not be empty; leave it out for no row label.",
           call. = FALSE)
    }
    name <- stage
    row_label <- stage
  }
  s <- add_stage(.flow, name, row_label, "exclude", "all")
  x <- s$x
  sd <- x$side
  start <- if (nrow(sd) == 0) 1L else max(sd$id) + 1L
  ids <- seq.int(start, length.out = k)
  x$side <- rbind(sd, data.frame(
    id = ids, stage = s$id, parent = as.integer(targets), label = labels,
    n = n_vals, note = note_vals, stringsAsFactors = FALSE
  ))
  for (i in seq_len(k)) {
    r <- reason_list[[i]]
    if (nrow(r) > 0) {
      x$reasons <- rbind(x$reasons, data.frame(
        side = ids[i], label = r$label, n = r$n, stringsAsFactors = FALSE
      ))
    }
  }
  validate_dataflow(x)
}

# Reasons as a data frame (label, n) from a named or unnamed vector.
parse_reasons <- function(r) {
  if (is.null(r) || length(r) == 0) {
    return(data.frame(label = character(), n = character(),
                      stringsAsFactors = FALSE))
  }
  if (is.null(names(r))) {
    if (!is.character(r) || anyNA(r) || any(r == "")) {
      stop("Unnamed `reasons` must be text, e.g. c(\"Declined\", ",
           "\"Moved away\"). Name them to add counts.", call. = FALSE)
    }
    return(data.frame(label = unname(r), n = NA_character_,
                      stringsAsFactors = FALSE))
  }
  if (anyNA(names(r)) || any(names(r) == "")) {
    stop("Every reason needs a name, e.g. c(\"Declined\" = 12).",
         call. = FALSE)
  }
  if (anyNA(r)) {
    stop("A named reason has a missing count. Give every reason a count, ",
         "or give the reasons as unnamed text without counts.", call. = FALSE)
  }
  data.frame(label = names(r), n = format_n(unname(r)),
             stringsAsFactors = FALSE)
}

# A stage name not yet used: `base`, then "base 2", "base 3", ...
unique_stage_name <- function(x, base) {
  base <- gsub("[[:space:]]*\n[[:space:]]*", " ", base)
  name <- base
  i <- 1
  while (name %in% x$stages$name) {
    i <- i + 1
    name <- paste(base, i)
  }
  name
}
