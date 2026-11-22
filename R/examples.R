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
#' * `"trial"`: a made-up two-arm randomised trial laid out like the CONSORT
#'   2025 flow diagram (enrolment, allocation, follow-up, analysis), with an
#'   exclusion box at screening and losses to follow-up in each arm. All
#'   numbers are invented for illustration.
#'
#' @param name Which example.
#' @return A `dataflow` object.
#' @export
#' @examples
#' flow_example("lab")
#' flow_example("trial")
flow_example <- function(name = c("lab", "trial")) {
  name <- match.arg(name)
  x <- switch(name,
    lab = flow_start("Cultures", n = 100) |>
      flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = c(50, 50)) |>
      flow_split("Culture type", list("Type A", "Type B"), n = 50) |>
      flow_split("Batch", c("Batch 1", "Batch 2"), n = 25) |>
      flow_split("Cell culture composition", c("A", "B", "C")) |>
      flow_apply("Condition", "9 conditions each", over = "Experiment ID") |>
      flow_apply("Task", "2 tasks each\n(per condition)") |>
      flow_outcome("Number of trials", "600 each"),
    trial = flow_start("Assessed for eligibility", n = 412,
                       stage = "Enrolment") |>
      flow_exclude("Excluded", n = 112,
                   reasons = c("Did not meet inclusion criteria" = 68,
                               "Declined to participate" = 31,
                               "Other reasons" = 13)) |>
      flow_split("Randomisation", "Randomised", n = 300, row_label = "") |>
      flow_assign("Allocation", c("Exercise programme", "Usual care"),
                  n = 150, method = "random",
                  note = c("Received allocated intervention (n = 146)\nDid not receive it (n = 4)",
                           "Received usual care (n = 150)")) |>
      flow_exclude("Lost to follow-up", n = c(9, 6), stage = "Follow-up",
                   reasons = list(c("Moved away" = 5, "Withdrew consent" = 4),
                                  c("Moved away" = 2, "Withdrew consent" = 3,
                                    "Died" = 1))) |>
      flow_outcome("Analysis", "Analysed", n = c(141, 144))
  )
  attr(x, "source") <- switch(name,
    lab = paste(
      "Hand-drawn laboratory design supplied by the package author (2026).",
      "Counts are the author's own and are drawn as given, not recomputed."
    ),
    trial = paste(
      "Invented two-arm trial following the layout of the CONSORT 2025 flow",
      "diagram (Hopewell et al., BMJ 2025;388:e081123). Numbers are made up",
      "for illustration and do not describe a real study."
    )
  )
  x
}
