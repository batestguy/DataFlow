# The `dataflow` object ---------------------------------------------------
#
# A dataflow is pure data: no coordinates, no drawing.
#
# * stages: one row per stage (= one row of the diagram), in drawing order.
#     id, name (used by `over =`), row_label (drawn in the left margin),
#     type, display.
# * nodes: one row per box.
#     id, stage, label, n (character, drawn exactly as given), note,
#     anchor (the node whose layout block this node belongs to: its parent for
#     single-parent nodes, the lowest common ancestor of its sources for
#     brackets and pools).
# * edges: from, to, kind ("arrow" or "bracket").
# * side: exclusion side boxes (used from milestone 2 on).
# * leaves: ids of the nodes the next verb attaches to by default.

stage_types <- c(
  "start", "select", "cluster", "split", "assign",
  "apply", "pool", "exclude", "outcome"
)

new_dataflow <- function(stages, nodes, edges, side, leaves) {
  structure(
    list(stages = stages, nodes = nodes, edges = edges, side = side,
         leaves = leaves),
    class = "dataflow"
  )
}

empty_side <- function() {
  data.frame(
    id = integer(), stage = integer(), parent = integer(),
    label = character(), n = character(), note = character(),
    stringsAsFactors = FALSE
  )
}

#' Check that an object is a valid dataflow
#'
#' Checks structure only: every stage has a known type, every box after the
#' first is reached by an arrow or bracket, every edge points at real boxes,
#' and pools have at least two sources. Counts are never checked.
#'
#' @param x A `dataflow` object.
#' @return `x`, invisibly. Signals an error describing the first problem found.
#' @export
validate_dataflow <- function(x) {
  if (!inherits(x, "dataflow")) {
    stop("`x` must be a dataflow object (start one with flow_start()).",
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
  root <- nd$id[nd$stage == st$id[1]]
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
  if (!is.null(x$side) && nrow(x$side) > 0 &&
      !all(x$side$parent %in% nd$id)) {
    stop("Every exclusion box must hang off an existing box.", call. = FALSE)
  }
  invisible(x)
}

#' List the boxes in a dataflow
#'
#' Useful for finding the ids to pass to the `from` argument of the verbs.
#'
#' @param x A `dataflow` object.
#' @return A data frame with one row per box: `id`, `stage` (the stage name),
#'   `label`, `n`, `note`, and `leaf` (`TRUE` if the next verb will attach to
#'   it by default).
#' @export
#' @examples
#' flow_start("Participants", n = 120) |>
#'   flow_split("Site", c("North", "South"), n = c(60, 60)) |>
#'   flow_nodes()
flow_nodes <- function(x) {
  validate_dataflow(x)
  nd <- x$nodes
  data.frame(
    id = nd$id,
    stage = x$stages$name[match(nd$stage, x$stages$id)],
    label = nd$label,
    n = nd$n,
    note = nd$note,
    leaf = nd$id %in% x$leaves,
    stringsAsFactors = FALSE
  )
}

#' @export
print.dataflow <- function(x, ...) {
  st <- x$stages
  cat("<dataflow> ", nrow(st), " stage", if (nrow(st) != 1) "s", ", ",
      nrow(x$nodes), " box", if (nrow(x$nodes) != 1) "es", "\n", sep = "")
  for (i in seq_len(nrow(st))) {
    labs <- x$nodes$label[x$nodes$stage == st$id[i]]
    shown <- if (length(labs) > 6) {
      c(utils::head(labs, 5), paste0("... (", length(labs), " boxes)"))
    } else {
      labs
    }
    shown <- gsub("[[:space:]]*\n[[:space:]]*", " ", shown)
    cat(sprintf("  %-8s %s: %s\n", paste0("[", st$type[i], "]"),
                st$name[i], paste(shown, collapse = ", ")))
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
