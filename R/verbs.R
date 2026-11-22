# Verbs: each takes and returns a dataflow --------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`", arg, "` must be a single piece of text.", call. = FALSE)
  }
}

# The row label drawn in the left margin: the stage name unless the caller
# gives another, or "" for none.
pick_row_label <- function(row_label, stage) {
  if (is.null(row_label)) return(stage)
  check_string(row_label, "row_label")
  row_label
}

check_flow <- function(x) {
  if (!inherits(x, "dataflow")) {
    stop("The first argument must be a dataflow. Start one with flow_start().",
         call. = FALSE)
  }
}

check_display <- function(display) {
  display <- match.arg(display, c("all", "summary"))
  if (display == "summary") {
    stop("display = \"summary\" is not available yet; it arrives in a later ",
         "version. Use display = \"all\".", call. = FALSE)
  }
  display
}

# Counts are drawn exactly as given. Numbers are only formatted (no
# scientific notation, thousands separated by commas); text is kept verbatim.
format_n <- function(n) {
  if (is.null(n)) return(NA_character_)
  if (is.numeric(n)) {
    return(vapply(n, function(v) {
      if (is.na(v)) NA_character_
      else format(v, digits = 15, big.mark = ",", scientific = FALSE,
                  trim = TRUE)
    }, character(1)))
  }
  if (is.character(n)) return(n)
  stop("Counts (`n`) must be numbers or text.", call. = FALSE)
}

format_note <- function(note) {
  if (is.null(note)) return(NA_character_)
  if (!is.character(note)) stop("`note` must be text.", call. = FALSE)
  note
}

# Recycle a per-box value (n or note) to `total` boxes. Accepts a single
# value, one value per box, or one value per label in `per` (repeated for
# every target).
recycle_values <- function(values, total, arg, per = NULL) {
  if (is.list(values)) values <- unlist(lapply(values, function(v) v %||% NA))
  len <- length(values)
  if (len == 1) return(rep(values, total))
  if (len == total) return(values)
  if (!is.null(per) && len == per && total %% per == 0) {
    return(rep(values, total / per))
  }
  stop("`", arg, "` has ", len, " values but there are ", total,
       " boxes to fill. Give one value, or one per box",
       if (!is.null(per)) paste0(", or one per label (", per, ")"), ".",
       call. = FALSE)
}

add_stage <- function(x, name, row_label, type, display,
                      over = NA_character_, method = NA_character_) {
  if (name %in% x$stages$name) {
    stop("There is already a stage called \"", name,
         "\". Stage names must be unique.", call. = FALSE)
  }
  id <- if (nrow(x$stages) == 0) 1L else max(x$stages$id) + 1L
  x$stages <- rbind(x$stages, data.frame(
    id = id, name = name, row_label = row_label, type = type,
    display = display, over = over, method = method,
    stringsAsFactors = FALSE
  ))
  list(x = x, id = id)
}

add_nodes <- function(x, stage, label, n, note, anchor) {
  start <- if (nrow(x$nodes) == 0) 1L else max(x$nodes$id) + 1L
  ids <- seq.int(start, length.out = length(label))
  x$nodes <- rbind(x$nodes, data.frame(
    id = ids, stage = stage, label = label, n = n, note = note,
    anchor = as.integer(anchor), stringsAsFactors = FALSE
  ))
  list(x = x, ids = ids)
}

add_edges <- function(x, from, to, kind) {
  if (length(from) == 0) return(x)
  x$edges <- rbind(x$edges, data.frame(
    from = as.integer(from), to = as.integer(to), kind = kind,
    stringsAsFactors = FALSE
  ))
  x
}

# Which boxes a verb attaches to: the current leaves, or those picked by
# `from` (box ids, or labels of current leaves).
resolve_targets <- function(x, from) {
  if (is.null(from)) return(x$leaves)
  if (length(from) == 0 || anyNA(from)) {
    stop("`from` is empty or has missing values.", call. = FALSE)
  }
  from <- unique(from)
  if (is.numeric(from)) {
    if (any(from != round(from))) {
      stop("Box ids in `from` must be whole numbers.", call. = FALSE)
    }
    bad <- setdiff(from, x$nodes$id)
    if (length(bad) > 0) {
      stop("No box with id ", paste(bad, collapse = ", "),
           ". See flow_nodes() for ids.", call. = FALSE)
    }
    return(as.integer(from))
  }
  if (is.character(from)) {
    hit <- x$leaves[x$nodes$label[match(x$leaves, x$nodes$id)] %in% from]
    if (length(hit) == 0) {
      stop("`from` matched none of the current end boxes. See flow_nodes().",
           call. = FALSE)
    }
    return(hit)
  }
  stop("`from` must be box ids (numbers) or box labels (text).", call. = FALSE)
}

# Replace each target in the leaf list by the boxes that now follow it,
# keeping left-to-right order.
replace_leaves <- function(leaves, targets, new_by_target) {
  out <- integer()
  for (l in leaves) {
    i <- match(l, targets)
    out <- c(out, if (is.na(i)) l else new_by_target[[i]])
  }
  # targets picked by id that were not leaves: append their new boxes
  extra <- setdiff(unlist(new_by_target), out)
  unique(c(out, extra))
}

#' Start a flow diagram
#'
#' Creates the top box of the diagram.
#'
#' @param label Text for the top box, e.g. `"Cultures"`.
#' @param n Optional count, drawn exactly as given (a number or text).
#' @param note Optional extra line drawn in smaller italic text in the box.
#' @param stage Optional row label for the top row. By default the top row
#'   has no label.
#' @return A `dataflow` object.
#' @export
#' @examples
#' flow_start("Cultures", n = 100)
flow_start <- function(label, n = NULL, note = NULL, stage = NULL) {
  check_string(label, "label")
  if (!is.null(stage)) check_string(stage, "stage")
  if (length(n) > 1 || length(note) > 1) {
    stop("The top box takes a single `n` and `note`.", call. = FALSE)
  }
  x <- new_dataflow(
    stages = data.frame(id = integer(), name = character(),
                        row_label = character(), type = character(),
                        display = character(), over = character(),
                        method = character(), stringsAsFactors = FALSE),
    nodes = data.frame(id = integer(), stage = integer(), label = character(),
                       n = character(), note = character(),
                       anchor = integer(), stringsAsFactors = FALSE),
    edges = data.frame(from = integer(), to = integer(), kind = character(),
                       stringsAsFactors = FALSE),
    side = empty_side(),
    leaves = integer()
  )
  s <- add_stage(x, stage %||% label, stage %||% "", "start", "all")
  a <- add_nodes(s$x, s$id, label, format_n(n), format_note(note), NA)
  x <- a$x
  x$leaves <- a$ids
  validate_dataflow(x)
}

#' Split every current box into several
#'
#' Each current end box (or each box chosen with `from`) gets one child box
#' per label. Arrows join each box to its children.
#'
#' @param .flow A `dataflow` object.
#' @param stage Name of the new stage; drawn as the row label.
#' @param labels Either a character vector, used for every current box, or a
#'   list with one character vector per current box (for different children
#'   under different boxes, including one-to-one steps).
#' @param n Optional counts, drawn exactly as given: one value for all new
#'   boxes, one per label, one per new box, or a list matching `labels`.
#' @param note Optional notes, recycled like `n`.
#' @param display `"all"` draws every box. `"summary"` is planned for a later
#'   version.
#' @param from Optional box ids or labels to split instead of all current end
#'   boxes. See [flow_nodes()].
#' @param row_label Text drawn in the left margin for this row. Defaults to
#'   `stage`; use `""` for no label (e.g. when a row continues the section
#'   labelled above it).
#' @return The updated `dataflow`.
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_split("Culture type", list("Type A", "Type B"), n = 50)
flow_split <- function(.flow, stage, labels, n = NULL, note = NULL,
                       display = "all", from = NULL, row_label = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display)
  split_boxes(.flow, stage, labels, n, note, from,
              pick_row_label(row_label, stage), "split", display)
}

# Shared engine for one-to-many steps (split, assign).
split_boxes <- function(.flow, stage, labels, n, note, from, row_label, type,
                        display, method = NA_character_) {
  targets <- resolve_targets(.flow, from)
  if (is.list(labels)) {
    if (length(labels) != length(targets)) {
      stop("`labels` is a list of ", length(labels), " but there are ",
           length(targets), " boxes to split. Give one element per box.",
           call. = FALSE)
    }
    per_target <- lapply(labels, as.character)
    per <- NULL
  } else {
    if (!is.character(labels) || length(labels) == 0) {
      stop("`labels` must be text (a character vector or a list).",
           call. = FALSE)
    }
    per_target <- rep(list(labels), length(targets))
    per <- length(labels)
  }
  if (any(lengths(per_target) == 0)) {
    stop("Every box needs at least one label to split into.", call. = FALSE)
  }
  if (anyNA(unlist(per_target)) || any(unlist(per_target) == "")) {
    stop("Labels must not be missing or empty.", call. = FALSE)
  }
  total <- sum(lengths(per_target))
  n_vals <- per_box_values(n, format_n, per_target, "n", per)
  note_vals <- per_box_values(note, format_note, per_target, "note", per)

  s <- add_stage(.flow, stage, row_label, type, display, method = method)
  x <- s$x
  anchors <- rep(targets, lengths(per_target))
  a <- add_nodes(x, s$id, unlist(per_target), n_vals, note_vals, anchors)
  x <- add_edges(a$x, anchors, a$ids, "arrow")
  x$leaves <- replace_leaves(x$leaves, targets,
                             split(a$ids, factor(anchors, levels = targets)))
  validate_dataflow(x)
}

# Values for the boxes of a split. A list gives one element per box being
# split, each recycled to that box's children; anything else goes through
# recycle_values().
per_box_values <- function(values, fmt, per_target, arg, per) {
  total <- sum(lengths(per_target))
  if (!is.list(values)) return(recycle_values(fmt(values), total, arg, per))
  if (length(values) != length(per_target)) {
    stop("`", arg, "` is a list of ", length(values), " but there are ",
         length(per_target), " boxes to split. Give one element per box.",
         call. = FALSE)
  }
  unlist(lapply(seq_along(values), function(i) {
    recycle_values(fmt(values[[i]]), length(per_target[[i]]), arg)
  }))
}

# Shared engine for verbs that add one box per group of current boxes.
# Groups of one are joined by an arrow; larger groups by a bracket.
attach_per_group <- function(x, stage, label, n, note, over, from, type,
                             display, row_label = stage) {
  targets <- resolve_targets(x, from)
  if (is.null(over)) {
    groups <- as.list(targets)
  } else {
    check_string(over, "over")
    over_id <- x$stages$id[x$stages$name == over]
    if (length(over_id) == 0) {
      stop("`over` must name an earlier stage; there is no stage \"", over,
           "\".", call. = FALSE)
    }
    key <- vapply(targets, function(t) {
      chain <- c(t, node_ancestors(x, t))
      hit <- chain[x$nodes$stage[match(chain, x$nodes$id)] == over_id]
      if (length(hit) == 0) NA_integer_ else hit[1]
    }, integer(1))
    if (anyNA(key)) {
      stop("Some boxes do not sit under the stage \"", over, "\".",
           call. = FALSE)
    }
    groups <- unname(split(targets, factor(key, levels = unique(key))))
  }
  for (g in groups) {
    nested <- vapply(g, function(m) any(g %in% node_ancestors(x, m)), NA)
    if (any(nested)) {
      stop("A group cannot contain a box together with a box below it. ",
           "Choose boxes on the same branch level with `from`.", call. = FALSE)
    }
  }
  k <- length(groups)
  if (!is.character(label) || anyNA(label) || any(label == "")) {
    stop("`label` must be text and not empty.", call. = FALSE)
  }
  labels <- recycle_values(label, k, "label")
  n_vals <- recycle_values(format_n(n), k, "n")
  note_vals <- recycle_values(format_note(note), k, "note")
  anchors <- vapply(groups, function(g) {
    if (length(g) == 1) g else lowest_common_ancestor(x, g)
  }, integer(1))

  s <- add_stage(x, stage, row_label, type, display,
                 over %||% NA_character_)
  a <- add_nodes(s$x, s$id, labels, n_vals, note_vals, anchors)
  x <- a$x
  for (i in seq_len(k)) {
    kind <- if (length(groups[[i]]) == 1) "arrow" else "bracket"
    x <- add_edges(x, groups[[i]], rep(a$ids[i], length(groups[[i]])), kind)
  }
  # each group's boxes are replaced by the one new box, at the first position
  new_by_target <- vector("list", length(targets))
  for (i in seq_len(k)) {
    g <- groups[[i]]
    new_by_target[[match(g[1], targets)]] <- a$ids[i]
    for (j in g[-1]) new_by_target[[match(j, targets)]] <- integer()
  }
  x$leaves <- replace_leaves(x$leaves, targets, new_by_target)
  validate_dataflow(x)
}

#' Apply something to every current box, or to groups of them
#'
#' Adds a box for a treatment, condition or task applied to the current
#' boxes. With `over = NULL` each current box gets its own new box, joined by
#' an arrow. With `over` naming an earlier stage, the current boxes are
#' grouped by the box they sit under in that stage, and each group is joined
#' by a bracket to a single new box.
#'
#' @inheritParams flow_split
#' @param label Text for the new box(es): one value, or one per group.
#' @param n,note Optional count and note: one value, or one per group.
#' @param over Optional name of an earlier stage to group by (draws brackets).
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_split("Composition", c("A", "B", "C")) |>
#'   flow_apply("Condition", "9 conditions each", over = "Experiment ID")
flow_apply <- function(.flow, stage, label, n = NULL, note = NULL,
                       over = NULL, display = "all", from = NULL,
                       row_label = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display)
  attach_per_group(.flow, stage, label, n, note, over, from, "apply", display,
                   pick_row_label(row_label, stage))
}

#' Add the outcome or measurement at the end of each branch
#'
#' @inheritParams flow_apply
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_outcome("Number of trials", "600 each")
flow_outcome <- function(.flow, stage, label, n = NULL, note = NULL,
                         display = "all", from = NULL, row_label = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display)
  attach_per_group(.flow, stage, label, n, note, NULL, from, "outcome",
                   display, pick_row_label(row_label, stage))
}
