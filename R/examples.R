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
#' * `"review"`: a made-up systematic review laid out like the PRISMA 2020
#'   flow diagram for new reviews that searched databases, registers and
#'   other sources: two starting streams, each with its own screening and
#'   exclusion boxes, joined in "Studies included in review". All numbers are
#'   invented for illustration.
#' * `"factorial"`: a made-up 2 x 2 factorial trial (drug or placebo, crossed
#'   with diet A or B, drawn as nested splits), an outcome box per cell, then
#'   the cells pooled over diet for an analysis per drug group. All numbers
#'   are invented for illustration.
#'
#' @param name Which example.
#' @return A `dataflow` object.
#' @export
#' @examples
#' flow_example("lab")
#' flow_example("trial")
#' flow_example("survey")
#' flow_example("review")
#' flow_example("factorial")
flow_example <- function(name = c("lab", "trial", "survey", "review",
                                  "factorial")) {
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
      flow_outcome("Analysis", "Women interviewed", n = 7904),
    review = flow_start(c("Records identified from\ndatabases and registers",
                          "Records identified from\nother sources"),
                        n = c(1954, 77),
                        note = c("Databases (n = 1,842), registers (n = 112)",
                                 "Websites (n = 46), citation searching (n = 31)"),
                        stage = "Identification") |>
      flow_exclude("Records removed before screening", n = 452,
                   reasons = c("Duplicate records" = 398,
                               "Marked ineligible by automation tools" = 54),
                   from = 1) |>
      flow_apply("Screening", "Records screened", n = 1502, from = 1) |>
      flow_exclude("Records excluded", n = 1310, from = "Records screened") |>
      flow_apply("Retrieval", "Reports sought for retrieval",
                 n = c(192, 77), row_label = "") |>
      flow_exclude("Reports not retrieved", n = c(7, 5)) |>
      flow_apply("Eligibility", "Reports assessed for eligibility",
                 n = c(185, 72), row_label = "") |>
      flow_exclude("Reports excluded", n = c(148, 66),
                   reasons = list(c("Wrong population" = 61,
                                    "Wrong intervention" = 47,
                                    "Wrong outcome" = 40),
                                  c("Wrong population" = 30,
                                    "No outcome data" = 36))) |>
      flow_pool("Included", "Studies included in review", n = 43,
                note = "Reports of included studies (n = 49)"),
    factorial = flow_start("Randomised", n = 200, stage = "Enrolment") |>
      flow_assign("Drug", c("Drug", "Placebo"), n = 100) |>
      flow_assign("Diet", c("Diet A", "Diet B"), n = 50) |>
      flow_exclude("Lost", n = c(2, 3, 1, 4), stage = "Follow-up") |>
      flow_outcome("Outcome", "Analysed", n = c(48, 47, 49, 46)) |>
      flow_pool("Pooled analysis", "Pooled over diet", n = c(95, 95),
                over = "Drug")
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
    ),
    review = paste(
      "Invented systematic review following the layout of the PRISMA 2020",
      "flow diagram for new systematic reviews which included searches of",
      "databases, registers and other sources (Page et al., BMJ",
      "2021;372:n71, doi:10.1136/bmj.n71). Numbers are made up for",
      "illustration and do not describe a real review."
    ),
    factorial = paste(
      "Invented 2 x 2 factorial trial (drug or placebo, crossed with diet A",
      "or B), drawn as nested assignments with an outcome per cell and the",
      "cells pooled over diet for an analysis per drug group. Numbers are",
      "made up for illustration and do not describe a real study."
    )
  )
  x
}
