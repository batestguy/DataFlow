# Pooling: several boxes merge into one --------------------------------------

#' Pool several boxes into one
#'
#' Merges boxes into a single new box, drawn with converging lines: a line
#' down from each source to a shared horizontal line, then one arrow into the
#' pooled box. Use it to join starting streams (for example the two columns
#' of a PRISMA 2020 flow diagram), or to pool the cells of a factorial design
#' for an analysis per level of one factor.
#'
#' By default all current end boxes (or all boxes in `from`) merge into one
#' box. With `over`, they are grouped by the box they sit under in an earlier
#' stage, one pool per group, as in [flow_apply()]. With `groups`, each
#' element of the list gives the boxes of one pool.
#'
#' Every pool needs at least two sources, and a box can feed only one pool
#' per call (so the diagram stays a tree; overlapping pools, such as both
#' margins of a factorial design, are not supported). The pooled box is
#' centred under its sources.
#'
#' @inheritParams flow_apply
#' @param label Text for the pooled box(es): one value, or one per pool.
#' @param n,note Optional count and note: one value, or one per pool. In a
#'   flow with data (see [flow_start()]), a missing `n` is computed from the
#'   rows of the pooled boxes, also across data frames of different streams;
#'   a typed `n` always wins.
#' @param groups Optional list of vectors of box ids or labels, one vector
#'   per pool, e.g. `list(c("A", "B"), c("C", "D"))`. Labels match current
#'   end boxes. Cannot be combined with `over` or `from`.
#' @param over Optional name of an earlier stage: one pool per box of that
#'   stage, holding the current boxes under it.
#' @return The updated `studydesignflow`.
#' @export
#' @examples
#' flow_start(c("Records from databases", "Records from other sources"),
#'            n = c(1200, 80)) |>
#'   flow_pool("Screening", "Records screened", n = 1280)
#'
#' # one pool per level of the first factor of a 2 x 2 design
#' flow_start("Participants", n = 200) |>
#'   flow_split("Drug", c("Drug", "No drug"), n = 100) |>
#'   flow_split("Diet", c("Diet A", "Diet B"), n = 50) |>
#'   flow_pool("Pooled", "Pooled over diet", n = 100, over = "Drug")
flow_pool <- function(.flow, stage, label, n = NULL, note = NULL,
                      groups = NULL, over = NULL, from = NULL,
                      row_label = NULL, events = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  if (!is.null(groups)) {
    if (!is.null(over) || !is.null(from)) {
      stop("Give `groups`, or `over`/`from`, not both.", call. = FALSE)
    }
    if (!is.list(groups) || length(groups) == 0) {
      stop("`groups` must be a list with one vector of box ids or labels ",
           "per pool.", call. = FALSE)
    }
    groups <- lapply(groups, function(g) resolve_targets(.flow, g))
  } else {
    targets <- resolve_targets(.flow, from)
    groups <- if (is.null(over)) list(targets) else
      group_over(.flow, targets, over)
  }
  ids <- unlist(groups)
  if (anyDuplicated(ids)) {
    dup <- ids[duplicated(ids)][1]
    stop("A box can feed only one pool per call; \"",
         .flow$nodes$label[.flow$nodes$id == dup],
         "\" is in more than one group.", call. = FALSE)
  }
  small <- which(lengths(groups) < 2)
  if (length(small) > 0) {
    stop("Every pool needs at least two boxes; pool ", small[1],
         " has only \"", .flow$nodes$label[.flow$nodes$id == groups[[small[1]]]],
         "\".", call. = FALSE)
  }
  attach_per_group(.flow, stage, label, n, note, over, NULL, "pool", "all",
                   pick_row_label(row_label, stage), groups = groups,
                   join = "pool", events = events)
}
