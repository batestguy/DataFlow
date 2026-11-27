# The `studydesignflow` object --------------------------------------------
#
# A studydesignflow is pure data: no coordinates, no drawing.
#
# * stages: one row per stage (= one row of the diagram), in drawing order.
#     id, name (used by `over =`), row_label (drawn in the left margin; "" for
#     none), type, display, over, method (how boxes were assigned, for
#     "assign" stages; used in the description only).
# * nodes: one row per box.
#     id, stage, label, n (character, drawn exactly as given), note,
#     anchor (the node whose layout block this node belongs to: its parent for
#     single-parent nodes, the lowest common ancestor of its sources for
#     brackets and pools). NA for the top boxes (all on the first stage), and
#     for a pool whose sources come from different starting streams.
# * edges: from, to, kind ("arrow", "bracket", "pool" or "measure": an
#     outcome measured in every branch, drawn once, see flow_outcome()).
# * side: exclusion side boxes. Each sits on its own "exclude" stage row,
#     to the right of the line leaving its parent box. Side boxes never
#     change `leaves`: the flow continues below the parent.
#     id, stage, parent, label, n, note.
# * reasons: reasons listed in a side box, in order. side, label, n.
# * leaves: ids of the nodes the next verb attaches to by default.
# * data (only when counts come from data, see R/data.R): the data frames,
#     stored once, and `rows`: per box, the row numbers behind it.
# * details (only when given, see R/details.R): who, with what, when, where
#     and how, per step (stage id) or per box.
# * kind (only when given, see R/kind.R): steps done to physical things or
#     to data, per step (stage id) or per box.

edge_kinds <- c("arrow", "bracket", "pool", "measure")

stage_types <- c(
  "start", "select", "cluster", "split", "assign",
  "apply", "pool", "exclude", "outcome", "partition", "copy"
)

new_studydesignflow <- function(stages, nodes, edges, side, leaves,
                         reasons = empty_reasons()) {
  structure(
    list(stages = stages, nodes = nodes, edges = edges, side = side,
         reasons = reasons, leaves = leaves),
    class = "studydesignflow"
  )
}

empty_reasons <- function() {
  data.frame(side = integer(), label = character(), n = character(),
             stringsAsFactors = FALSE)
}

empty_side <- function() {
  data.frame(
    id = integer(), stage = integer(), parent = integer(),
    label = character(), n = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

#' Check that an object is a valid studydesignflow
#'
#' Checks structure only: every stage has a known type, there is at least
#' one top box and every top box is on the first stage, every other box is
#' reached by an arrow, bracket, pool or measurement line, every edge points
#' at real boxes and is of a known kind, pools have at least two sources, and
#' an outcome measured in every branch (see [flow_outcome()]) is joined from at
#' least two boxes. Counts are never checked.
#'
#' @param x A `studydesignflow` object.
#' @return `x`, invisibly. Signals an error describing the first problem found.
#' @export
validate_studydesignflow <- function(x) {
  if (!inherits(x, "studydesignflow")) {
    stop("`x` must be a studydesignflow object (start one with flow_start()).",
         call. = FALSE)
  }
  st <- x$stages
  nd <- x$nodes
  ed <- x$edges
  bad_type <- setdiff(st$type, stage_types)
  if (length(bad_type) > 0) {
    stop("Unknown stage type: ", paste(bad_type, collapse = ", "), ".",
         call. = FALSE)
  }
  if (anyDuplicated(st$name)) {
    dup <- unique(st$name[duplicated(st$name)])
    stop("Stage names must be unique; repeated: ",
         paste0('"', dup, '"', collapse = ", "), ".", call. = FALSE)
  }
  if (!all(nd$stage %in% st$id)) {
    stop("Some boxes belong to a stage that does not exist.", call. = FALSE)
  }
  if (!all(c(ed$from, ed$to) %in% nd$id)) {
    stop("Some arrows point to or from a box that does not exist.",
         call. = FALSE)
  }
  bad_kind <- setdiff(ed$kind, edge_kinds)
  if (length(bad_kind) > 0) {
    stop("Unknown kind of line: ", paste(bad_kind, collapse = ", "), ".",
         call. = FALSE)
  }
  for (id in unique(ed$to[ed$kind == "measure"])) {
    if (sum(ed$to == id) < 2 || any(ed$kind[ed$to == id] != "measure") ||
        st$type[match(nd$stage[nd$id == id], st$id)] != "outcome") {
      stop("A box measured in every branch must be an outcome box joined ",
           "from at least two boxes (box \"", nd$label[nd$id == id], "\").",
           call. = FALSE)
    }
  }
  root <- nd$id[nd$stage == st$id[1]]
  if (length(root) == 0) {
    stop("A studydesignflow must have at least one top box.", call. = FALSE)
  }
  if (any(!is.na(nd$anchor[nd$id %in% root])) || any(ed$to %in% root)) {
    stop("Top boxes (the boxes of the first stage) cannot have a box above ",
         "them.", call. = FALSE)
  }
  orphans <- setdiff(nd$id, c(root, ed$to))
  if (length(orphans) > 0) {
    stop("Boxes with no incoming arrow: ",
         paste(nd$label[match(orphans, nd$id)], collapse = ", "), ".",
         call. = FALSE)
  }
  pool_stages <- st$id[st$type == "pool"]
  for (id in nd$id[nd$stage %in% pool_stages]) {
    if (sum(ed$to == id) < 2) {
      stop("A pool needs at least two sources (box \"",
           nd$label[nd$id == id], "\").", call. = FALSE)
    }
  }
  # off the first row, only a box joined from several starting streams has
  # no box above it in the layout tree
  floating <- nd$id[is.na(nd$anchor) & !nd$id %in% root]
  lone <- floating[vapply(floating, function(id) sum(ed$to == id) < 2, NA)]
  if (length(lone) > 0) {
    stop("Every top box must be on the first stage; \"",
         nd$label[nd$id == lone[1]], "\" is not.", call. = FALSE)
  }
  sd <- x$side
  if (!is.null(sd) && nrow(sd) > 0) {
    if (!all(sd$parent %in% nd$id)) {
      stop("Every exclusion box must hang off an existing box.",
           call. = FALSE)
    }
    if (!all(st$type[match(sd$stage, st$id)] %in% "exclude")) {
      stop("Exclusion boxes must belong to an exclusion stage.",
           call. = FALSE)
    }
    if (any(nd$stage %in% st$id[st$type == "exclude"])) {
      stop("An exclusion stage holds only exclusion boxes.", call. = FALSE)
    }
    parent_row <- match(nd$stage[match(sd$parent, nd$id)], st$id)
    if (any(match(sd$stage, st$id) <= parent_row)) {
      stop("An exclusion box must come after the box it hangs off.",
           call. = FALSE)
    }
  }
  rs <- x$reasons
  if (!is.null(rs) && nrow(rs) > 0 && !all(rs$side %in% sd$id)) {
    stop("Every exclusion reason must belong to an exclusion box.",
         call. = FALSE)
  }
  # units and count columns (R/units.R), present only when given
  check_unit_store(x$units, nd$id, "units of boxes")
  check_unit_store(x$side_units, (sd %||% empty_side())$id,
                   "units of exclusion boxes")
  check_unit_store(x$data$counts, nd$id, "count columns of boxes")
  # step details (R/details.R), present only when given
  check_details_store(x$details, st$id, nd$id, (sd %||% empty_side())$id)
  # kinds of step (R/kind.R), present only when given
  check_kind_store(x$kind, st$id, nd$id, (sd %||% empty_side())$id)
  invisible(x)
}

#' List the boxes in a studydesignflow
#'
#' Useful for finding the ids to pass to the `from` argument of the verbs.
#'
#' @param x A `studydesignflow` object.
#' @return A data frame with one row per box: `id`, `stage` (the stage name),
#'   `label`, `n`, `note`, `leaf` (`TRUE` if the next verb will attach to
#'   it by default), `n_edited` (`TRUE` if its count was changed with
#'   [flow_edit()]), and `events` (outcome events, if any). In a flow where
#'   some step was given a `unit`, also `unit`: what each box's count counts,
#'   its own or taken from the box above (`NA` for none).
#' @export
#' @examples
#' flow_start("Participants", n = 120) |>
#'   flow_split("Site", c("North", "South"), n = c(60, 60)) |>
#'   flow_nodes()
flow_nodes <- function(x) {
  validate_studydesignflow(x)
  nd <- x$nodes
  out <- data.frame(
    id = nd$id,
    stage = x$stages$name[match(nd$stage, x$stages$id)],
    label = nd$label,
    n = nd$n,
    note = nd$note,
    leaf = nd$id %in% x$leaves,
    n_edited = nd$id %in% edited_ids(x, "nodes"),
    events = if (is.null(x$events)) NA_character_ else
      unname(x$events[as.character(nd$id)]),
    stringsAsFactors = FALSE
  )
  if (!is.null(x$units)) out$unit <- unname(box_units(x)[as.character(nd$id)])
  out
}

#' @export
print.studydesignflow <- function(x, ...) {
  st <- x$stages
  ns <- if (is.null(x$side)) 0 else nrow(x$side)
  cat("<studydesignflow> ", nrow(st), " stage", if (nrow(st) != 1) "s", ", ",
      nrow(x$nodes), " box", if (nrow(x$nodes) != 1) "es",
      if (ns > 0) paste0(", ", ns, " exclusion box", if (ns != 1) "es"),
      "\n", sep = "")
  if (!is.null(x$data)) {
    sizes <- vapply(x$data$sources, nrow, integer(1))
    cat("  counts from data: ",
        paste0(x$data$names, " (", sizes, " rows)", collapse = ", "),
        if (!is.null(x$data$count)) paste0("; counting distinct ",
                                           x$data$count),
        "\n", sep = "")
  }
  for (i in seq_len(nrow(st))) {
    labs <- if (st$type[i] == "exclude") {
      x$side$label[x$side$stage == st$id[i]]
    } else {
      x$nodes$label[x$nodes$stage == st$id[i]]
    }
    shown <- if (length(labs) > 6) {
      c(utils::head(labs, 5), paste0("... (", length(labs), " boxes)"))
    } else {
      labs
    }
    shown <- gsub("[[:space:]]*\n[[:space:]]*", " ", shown)
    cat(sprintf("  %-9s %s: %s\n", paste0("[", st$type[i], "]"),
                st$name[i], paste(shown, collapse = ", ")))
  }
  if (!is.null(x$details)) {
    k <- lengths(x$details[c("steps", "boxes", "side")])
    cat("  step details: ", and_list(c(
      if (k[1] > 0) paste0(k[1], " step", if (k[1] != 1) "s"),
      if (k[2] + k[3] > 0) paste0(k[2] + k[3], " box",
                                  if (k[2] + k[3] != 1) "es"))),
      " (see flow_describe())\n", sep = "")
  }
  if (!is.null(x$kind)) {
    kd <- c(box_kinds(x), side_kinds(x))
    k <- c(data = sum(kd == "data", na.rm = TRUE),
           physical = sum(kd == "physical", na.rm = TRUE))
    cat("  kinds of step: ", and_list(c(
      if (k[["data"]] > 0) paste0(k[["data"]], " box",
                                  if (k[["data"]] != 1) "es",
                                  " done to data (dashed edge)"),
      if (k[["physical"]] > 0) paste0(k[["physical"]], " box",
                                      if (k[["physical"]] != 1) "es",
                                      " done to physical things"))),
      "\n", sep = "")
  }
  invisible(x)
}

# Ancestors of a node, nearest first, following anchors (the layout tree).
node_ancestors <- function(x, id) {
  out <- integer()
  anc <- x$nodes$anchor[x$nodes$id == id]
  while (length(anc) == 1 && !is.na(anc)) {
    out <- c(out, anc)
    anc <- x$nodes$anchor[x$nodes$id == anc]
  }
  out
}

# Lowest common ancestor (in the layout tree) of a set of nodes.
lowest_common_ancestor <- function(x, ids) {
  if (length(ids) == 1) {
    return(x$nodes$anchor[x$nodes$id == ids])
  }
  paths <- lapply(ids, function(i) node_ancestors(x, i))
  common <- Reduce(intersect, paths)
  # paths are nearest-first, so the first common element is the lowest
  common[1]
}

# Every box upstream of a node, following the drawn lines (all sources of
# brackets and pools, not only the layout tree, which stops at a pool that
# joins streams).
node_upstream <- function(x, id) {
  out <- integer()
  todo <- id
  while (length(todo) > 0) {
    src <- setdiff(x$edges$from[x$edges$to %in% todo], out)
    out <- c(out, src)
    todo <- src
  }
  out
}
