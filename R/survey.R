# Verbs for surveys: cluster sampling and selection ------------------------

#' Sample clusters within each current box
#'
#' For cluster sampling: selects clusters (for example enumeration areas,
#' villages or schools) within each current box. Clusters are usually too
#' many to draw, so by default each current box gets one summary box such as
#' "40 EAs selected". With `display = "all"` and text `labels`, every cluster
#' is drawn, as in [flow_split()].
#'
#' The `method` appears in the plain-language description from
#' [flow_describe()] ("selected by probability proportional to size").
#'
#' @inheritParams flow_split
#' @param labels The number of clusters selected under each current box (one
#'   number, or one per box), or their labels as text (as in [flow_split()]).
#' @param method Optional sampling method, e.g.
#'   `"probability proportional to size"`.
#' @param display `"summary"` (the default) draws one box per current box;
#'   `"all"` draws every cluster (text `labels` only).
#' @param summary Label of each summary box; `{k}` is replaced by the number
#'   of clusters and `{stage}` by the stage name.
#' @return The updated `studydesignflow`.
#' @export
#' @examples
#' flow_start("Enumeration areas in frame", n = 9200) |>
#'   flow_split("Stratum", c("Urban", "Rural")) |>
#'   flow_cluster("Clusters", c(40, 60), summary = "{k} EAs selected",
#'                method = "probability proportional to size")
flow_cluster <- function(.flow, stage, labels = NULL, n = NULL, note = NULL,
                         method = NULL, display = "summary",
                         summary = "{k} clusters selected", from = NULL,
                         row_label = NULL, by = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display, summary_ok = TRUE)
  split_boxes(.flow, stage, labels, n, note, from,
              pick_row_label(row_label, stage), "cluster", display,
              method = check_method(method), summary = summary, by = by)
}

#' Select units within each current box
#'
#' For the units sampled or selected within each current box, for example
#' "25 households per EA" or "All eligible women". Like [flow_apply()]
#' without `over`: each current box gets its own new box, joined by an arrow.
#' The `method` appears in the plain-language description.
#'
#' @inheritParams flow_apply
#' @param method Optional selection method, e.g.
#'   `"systematic random sampling"`.
#' @param where For a flow with data (see [flow_start()]): a one-sided
#'   formula such as `~ age >= 18`, evaluated in the data (with the
#'   formula's environment for other names). Only the matching rows are
#'   selected and continue; the count is computed. Rows where the condition
#'   is missing (`NA`) are not selected. Without `where`, a selection in a
#'   flow with data has no rows behind it (the data do not say which units
#'   were selected), so its count is as typed.
#' @return The updated `studydesignflow`.
#' @export
#' @examples
#' flow_start("EA", n = 1) |>
#'   flow_select("Households", "25 households", n = 25,
#'               method = "systematic random sampling")
#'
#' people <- data.frame(age = c(12, 30, 45, 16, 70))
#' flow_start("Household members", data = people) |>
#'   flow_select("Adults", "Aged 18 or over", where = ~ age >= 18)
flow_select <- function(.flow, stage, label, n = NULL, note = NULL,
                        method = NULL, from = NULL, row_label = NULL,
                        where = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  where <- tryCatch(where, error = function(e) bad_condition("where", e))
  attach_per_group(.flow, stage, label, n, note, NULL, from, "select", "all",
                   pick_row_label(row_label, stage),
                   method = check_method(method), where = where)
}
