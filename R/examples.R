# Built-in example designs --------------------------------------------------

#' Example study designs
#'
#' Ready-made designs used in the documentation and tests. Each records where
#' it came from and what was simplified, in its `"source"` attribute.
#'
#' * `"lab"`: a laboratory design. 100 cultures split into two experiments,
#'   each with one culture type, two batches and three culture compositions;
#'   nine conditions are applied across all compositions within each
#'   experiment, then two tasks per condition. Counts are the design author's
#'   own figures, drawn as given.
#'
#' @param name Which example.
#' @return A `dataflow` object.
#' @export
#' @examples
#' flow_example("lab")
flow_example <- function(name = c("lab")) {
  name <- match.arg(name)
  x <- switch(name,
    lab = flow_start("Cultures", n = 100) |>
      flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = c(50, 50)) |>
      flow_split("Culture type", list("Type A", "Type B"), n = 50) |>
      flow_split("Batch", c("Batch 1", "Batch 2"), n = 25) |>
      flow_split("Cell culture composition", c("A", "B", "C")) |>
      flow_apply("Condition", "9 conditions each", over = "Experiment ID") |>
      flow_apply("Task", "2 tasks each\n(per condition)") |>
      flow_outcome("Number of trials", "600 each")
  )
  attr(x, "source") <- switch(name,
    lab = paste(
      "Hand-drawn laboratory design supplied by the package author (2026).",
      "Counts are the author's own and are drawn as given, not recomputed."
    )
  )
  x
}
