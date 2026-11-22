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
#' * `"survey"`: a made-up two-stage household survey in the style of a
#'   Demographic and Health Survey: enumeration areas (clusters) selected
#'   within urban and rural strata of three regions, households sampled in
#'   each, then eligible women interviewed, with non-response shown beside
#'   the flow. Clusters are drawn as summary boxes. Region names and all
#'   numbers are invented for illustration.
#'
#' @param name Which example.
#' @return A `dataflow` object.
#' @export
#' @examples
#' flow_example("lab")
#' flow_example("trial")
#' flow_example("survey")
flow_example <- function(name = c("lab", "trial", "survey")) {
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
      flow_outcome("Analysis", "Analysed", n = c(141, 144)),
    survey = flow_start("Enumeration areas (EAs)\nin sampling frame",
                        n = 9200, stage = "Sampling frame") |>
      flow_split("Region", c("North", "Central", "South"),
                 n = c(3100, 2800, 3300)) |>
      flow_split("Stratum", c("Urban", "Rural"),
                 n = list(c(900, 2200), c(1100, 1700), c(1500, 1800))) |>
      flow_cluster("Clusters", c(40, 60, 30, 50, 50, 40),
                   summary = "{k} EAs\nselected",
                   method = "probability proportional to size") |>
      flow_select("Households", "25 per EA",
                  n = c(1000, 1500, 750, 1250, 1250, 1000),
                  method = "systematic random sampling") |>
      flow_apply("Pooled", "Households selected", n = 6750,
                 over = "Sampling frame", row_label = "") |>
      flow_exclude("Households not interviewed", n = 412,
                   reasons = c("Dwelling vacant or not found" = 186,
                               "No competent respondent at home" = 129,
                               "Refused" = 97)) |>
      flow_apply("Interviews", "Households interviewed", n = 6338) |>
      flow_select("Eligible women", "Women aged 15-49 identified", n = 8211,
                  note = "All women aged 15-49 in interviewed households") |>
      flow_exclude("Women not interviewed", n = 307,
                   reasons = c("Not at home" = 168, "Refused" = 89,
                               "Incapacitated" = 50)) |>
      flow_outcome("Analysis", "Women interviewed", n = 7904)
  )
  attr(x, "source") <- switch(name,
    lab = paste(
      "Hand-drawn laboratory design supplied by the package author (2026).",
      "Counts are the author's own and are drawn as given, not recomputed."
    ),
    trial = paste(
      "Invented two-arm trial following the layout of the CONSORT 2025 flow",
      "diagram (Hopewell et al., BMJ 2025;389:e081123,",
      "doi:10.1136/bmj-2024-081123). Numbers are made up",
      "for illustration and do not describe a real study."
    ),
    survey = paste(
      "Invented two-stage cluster household survey in the style of a",
      "Demographic and Health Survey (stratified selection of enumeration",
      "areas with probability proportional to size, then a fixed number of",
      "households per area). Region names and numbers are made up for",
      "illustration and do not describe a real survey."
    )
  )
  x
}
