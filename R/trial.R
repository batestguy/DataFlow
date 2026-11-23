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
flow_assign <- function(.flow, stage, labels = NULL, n = NULL, note = NULL,
                        method = "random", from = NULL, row_label = NULL,
                        display = "all", summary = "{k} {stage}",
                        by = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display, summary_ok = TRUE)
  split_boxes(.flow, stage, labels, n, note, from,
              pick_row_label(row_label, stage), "assign", display,
              method = check_method(method), summary = summary, by = by)
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
#' @param where For a flow with data (see [flow_start()]): a one-sided
#'   formula, e.g. `~ is.na(outcome)`, for an exclusion box without reasons.
#'   Matching rows leave the flow; the count is computed.
#' @section Counts from data: In a flow with data, `reasons` may instead be
#'   a named list of one-sided formulas, e.g.
#'   `list("Under 18" = ~ age < 18, "Declined" = ~ consent == "no")`,
#'   evaluated in the rows still in the flow at each box. Rows matching any
#'   reason leave the flow and the rest continue. A row matching several
#'   reasons is counted under the **first** one that matches, so the reason
#'   counts add up to the box count (when counting rows). Rows where a
#'   condition is missing (`NA`) do not match it. Giving `n` together with
#'   formulas is an error. An exclusion box with typed counts only (no
#'   formulas, no `where`) does not say which rows left, so automatic counts
#'   below it stay blank.
#' @export
#' @examples
#' people <- data.frame(age = c(17, 25, 40, 33, 15, 52),
#'                      consent = c("yes", "yes", "no", "yes", "yes", "no"))
#' flow_start("Assessed", data = people) |>
#'   flow_exclude("Excluded", reasons = list("Under 18" = ~ age < 18,
#'                                           "Declined" = ~ consent == "no")) |>
#'   flow_nodes()
#'
#' flow_start("Assessed for eligibility", n = 412) |>
#'   flow_exclude("Excluded", n = 112,
#'                reasons = c("Did not meet inclusion criteria" = 68,
#'                            "Declined to participate" = 44)) |>
#'   flow_split("Randomisation", "Randomised", n = 300)
flow_exclude <- function(.flow, label = "Excluded", n = NULL, reasons = NULL,
                         note = NULL, stage = NULL, from = NULL,
                         where = NULL) {
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
  # evaluated here, in the order the arguments were always used
  reasons <- tryCatch(reasons, error = function(e) bad_condition("reasons", e))
  where <- tryCatch(where, error = function(e) bad_condition("where", e))
  is_formula <- if (is.list(reasons)) {
    vapply(reasons, function(r) inherits(r, "formula"), NA)
  } else {
    FALSE
  }
  if (any(is_formula) && !all(is_formula)) {
    stop("`reasons` mixes formulas with other values. Give every reason as ",
         "a formula (counts computed from data), or every reason as typed ",
         "text or counts.", call. = FALSE)
  }
  formula_reasons <- length(reasons) > 0 && all(is_formula)
  computed <- NULL
  if (formula_reasons || !is.null(where)) {
    computed <- exclude_from_data(.flow, targets, n, reasons, where,
                                  formula_reasons)
    n <- computed$n
    if (formula_reasons) reasons <- NULL
  }
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
  if (!is.null(computed$reasons)) reason_list <- computed$reasons

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
  if (has_data(x)) {
    for (i in seq_len(k)) {
      key <- as.character(targets[i])
      if (is.null(x$data$rows$left[[key]])) next
      if (is.null(computed)) {
        # removed rows not known: counts below this box are left blank
        x$data$rows$left[key] <- list(NULL)
      } else {
        out <- computed$rows[[i]]
        x$data$rows$side[as.character(ids[i])] <- list(out)
        if (formula_reasons) {
          x$data$computed_reasons <- c(x$data$computed_reasons, ids[i])
        }
        x$data$rows$left[key] <- list(setdiff(x$data$rows$left[[key]], out))
      }
    }
  }
  validate_dataflow(x)
}

# Rows, counts and reasons of exclusion boxes computed from data: each row
# matching any reason (or `where`) leaves the flow; a row matching several
# reasons is counted under the first.
exclude_from_data <- function(x, targets, n, reasons, where,
                              formula_reasons) {
  need_data(x, if (formula_reasons) "reasons" else "where")
  if (!is.null(n)) {
    stop("Give `n`, or reasons/`where` as formulas, not both: with ",
         "formulas, counts are computed.", call. = FALSE)
  }
  if (formula_reasons && !is.null(where)) {
    stop("Give formula `reasons` or `where`, not both; `where` is for an ",
         "exclusion box without reasons.", call. = FALSE)
  }
  if (formula_reasons) {
    nm <- names(reasons)
    if (is.null(nm) || anyNA(nm) || any(nm == "") || anyDuplicated(nm)) {
      stop("Formula `reasons` need a unique name each, e.g. ",
           "list(\"Under 18\" = ~ age < 18).", call. = FALSE)
    }
  }
  rows <- rows_of_targets(x, targets, "An exclusion computed from data")
  out <- list(n = numeric(), rows = list(), reasons = NULL)
  for (i in seq_along(targets)) {
    r <- rows[[i]]
    if (formula_reasons) {
      first <- integer(length(r))
      for (j in seq_along(reasons)) {
        hit <- eval_condition(x, r, reasons[[j]], "reasons")
        first[first == 0L & hit] <- j
      }
      gone <- r[first > 0L]
      counts <- vapply(seq_along(reasons), function(j) {
        count_rows(x, r[first == j])
      }, numeric(1))
      out$reasons[[i]] <- data.frame(label = nm, n = format_n(counts),
                                     stringsAsFactors = FALSE)
    } else {
      gone <- r[eval_condition(x, r, where, "where")]
    }
    warn_shared_ids(x, list(gone, setdiff(r, gone)), paste0(
      "the rows excluded under \"",
      x$nodes$label[x$nodes$id == targets[i]], "\" and those that continue"))
    out$n[i] <- count_rows(x, gone)
    out$rows[[i]] <- gone
  }
  out
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
