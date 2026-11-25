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
#' * `"cv"`: a fold grid (see [flow_folds()]) for 5-fold cross-validation
#'   grouped by patient, with counts computed from an invented data frame
#'   whose `fold` column assigns whole patients to folds.
#' * `"forecast"`: a fold grid for time-series evaluation with an expanding
#'   window and a one-month gap, from invented monthly data for three
#'   stores (2016 to 2023).
#' * `"trial_data"`: the same kind of CONSORT-style trial as `"trial"`, but
#'   every count is computed from a small invented data frame of 412
#'   participants (built inside the function with a fixed seed), using
#'   formula `reasons`, `by =` and `where =`. Use [flow_data()] on it to see
#'   the rows behind any box.
#' * `"ml"`: a made-up development and validation flow for a clinical
#'   prediction model, in the spirit of TRIPOD+AI item 20a (numbers and
#'   outcome events in each analysis). Admissions from two hospitals form the
#'   development data, with exclusions and a split into training and test
#'   sets that keeps all admissions of a patient in one set; a third
#'   hospital is a separate stream for external validation. Every count and
#'   event is computed from invented data frames built inside the function
#'   with a fixed seed.
#'
#' @param name Which example.
#' @return A `studydesignflow` object; for `"cv"` and `"forecast"`, a
#'   `studydesignflow_grid`.
#' @export
#' @examples
#' flow_example("lab")
#' flow_example("trial")
#' flow_example("survey")
#' flow_example("review")
#' flow_example("factorial")
#' flow_example("trial_data")
#' flow_example("ml")
#' flow_example("cv")
#' flow_example("forecast")
flow_example <- function(name = c("lab", "trial", "survey", "review",
                                  "factorial", "trial_data", "ml", "cv",
                                  "forecast")) {
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
                over = "Drug"),
    cv = {
      cv_data <- example_cv_data()
      flow_folds(cv_data, fold = "fold", group = "patient_id")
    },
    forecast = {
      sales <- example_forecast_data()
      flow_folds(sales, type = "expanding", time = "month", initial = 60,
                 assess = 6, skip = 5, gap = 1, time_unit = "months")
    },
    ml = {
      ml <- example_ml_data()
      development <- ml$development
      validation <- ml$validation
      flow_start(c("Development data:
admissions to hospitals A and B",
                   "External validation data:
admissions to hospital C"),
                 data = list(development, validation),
                 events = ~ died_30d == 1, stage = "Source") |>
        flow_split("Hospital", by = "hospital", from = 1,
                   labels = c("Hospital A" = "A", "Hospital B" = "B")) |>
        flow_pool("Pooled", "Development data",
                  from = c("Hospital A", "Hospital B"), row_label = "") |>
        flow_exclude("Excluded", stage = "Eligibility",
                     reasons = list("Under 18" = ~ age < 18,
                                    "Outcome missing" =
                                      ~ is.na(died_30d))) |>
        flow_partition(labels = c("Training set" = "train",
                                  "Test set" = "test"),
                       by = "set", group = "patient_id",
                       from = "Development data") |>
        flow_outcome("Analysis", c("Model development", "Internal test",
                                   "External validation"))
    },
    trial_data = {
      trial_data <- example_trial_data()
      flow_start("Assessed for eligibility", data = trial_data,
                 stage = "Enrolment") |>
        flow_exclude("Excluded",
                     reasons = list("Under 18" = ~ age < 18,
                                    "Did not meet inclusion criteria" =
                                      ~ !eligible,
                                    "Declined to participate" =
                                      ~ consent == "no")) |>
        flow_apply("Randomisation", "Randomised", row_label = "") |>
        flow_assign("Allocation", by = "arm") |>
        flow_exclude("Lost to follow-up", stage = "Follow-up",
                     reasons = list("Moved away" = ~ status == "moved",
                                    "Withdrew consent" =
                                      ~ status == "withdrew")) |>
        flow_exclude("No outcome recorded",
                     where = ~ is.na(outcome_score)) |>
        flow_outcome("Analysis", "Analysed")
    }
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
    cv = paste(
      "Invented grouped 5-fold cross-validation: 300 made-up patients with",
      "one to three records each, assigned to folds by patient inside",
      "flow_example() with set.seed(20261125). The data do not describe a",
      "real study."
    ),
    forecast = paste(
      "Invented monthly sales of three stores, January 2016 to December",
      "2023, evaluated with an expanding window (five years of training,",
      "six-month forecast horizon, one-month gap, origins six months",
      "apart), in the rolling-origin vocabulary of the rsample package.",
      "Numbers are made up and built inside flow_example()."
    ),
    ml = paste(
      "Invented clinical prediction model study in the spirit of TRIPOD+AI",
      "(Collins et al., BMJ 2024;385:e078378, doi:10.1136/bmj-2023-078378),",
      "item 20a: numbers and outcome events per analysis. Admissions and",
      "outcomes are made up, built inside flow_example() with",
      "set.seed(20261124); they do not describe a real study."
    ),
    trial_data = paste(
      "Invented two-arm trial in the layout of the CONSORT 2025 flow diagram",
      "(Hopewell et al., BMJ 2025;389:e081123, doi:10.1136/bmj-2024-081123),",
      "with every count computed from an invented data frame of 412",
      "participants built inside flow_example() with set.seed(20261123).",
      "The data do not describe a real study."
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

# Invented participant data for flow_example("trial_data"): one row per
# person assessed. Built with a fixed seed; the caller's random number state
# is restored afterwards.
example_trial_data <- function() {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had_seed) old <- get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old, envir = genv) else
    rm(".Random.seed", envir = genv))
  set.seed(20261123)
  n <- 412
  age <- sample(16:79, n, replace = TRUE)
  eligible <- stats::runif(n) > 0.12
  consent <- ifelse(stats::runif(n) < 0.1, "no", "yes")
  in_trial <- age >= 18 & eligible & consent == "yes"
  k <- sum(in_trial)
  arm <- rep(NA_character_, n)
  arm[in_trial] <- sample(rep(c("Exercise programme", "Usual care"),
                              length.out = k))
  status <- rep(NA_character_, n)
  status[in_trial] <- sample(c("completed", "moved", "withdrew"), k,
                             replace = TRUE, prob = c(0.92, 0.04, 0.04))
  score <- rep(NA_real_, n)
  done <- which(status == "completed")
  score[done] <- round(stats::rnorm(length(done), 50, 10))
  score[sample(done, 6)] <- NA
  data.frame(
    id = sprintf("P%03d", seq_len(n)), age = age, eligible = eligible,
    consent = consent,
    arm = factor(arm, levels = c("Exercise programme", "Usual care")),
    status = status, outcome_score = score, stringsAsFactors = FALSE
  )
}

# Run `code` with a fixed seed, restoring the caller's random number state.
with_example_seed <- function(seed, code) {
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  if (had_seed) old <- get(".Random.seed", envir = genv, inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", old, envir = genv) else
    rm(".Random.seed", envir = genv))
  set.seed(seed)
  force(code)
}

# Invented admissions for flow_example("ml"): development data from two
# hospitals (patients may have several admissions; the `set` column assigns
# whole patients to training or test) and external validation data from a
# third hospital.
example_ml_data <- function() {
  with_example_seed(20261124, {
    admissions <- function(n_pat, hospital, prefix) {
      visits <- sample(1:3, n_pat, replace = TRUE, prob = c(0.6, 0.3, 0.1))
      pid <- rep(sprintf("%s%04d", prefix, seq_len(n_pat)), visits)
      n <- length(pid)
      age <- rep(sample(15:95, n_pat, replace = TRUE), visits)
      risk <- stats::plogis(-3.2 + 0.035 * (age - 60))
      died <- stats::rbinom(n, 1, risk)
      died[sample(n, round(0.03 * n))] <- NA
      data.frame(patient_id = pid, hospital = hospital, age = age,
                 died_30d = died, stringsAsFactors = FALSE)
    }
    development <- rbind(admissions(420, "A", "A"),
                         admissions(310, "B", "B"))
    pats <- unique(development$patient_id)
    test_pats <- sample(pats, round(0.25 * length(pats)))
    development$set <- ifelse(development$patient_id %in% test_pats,
                              "test", "train")
    validation <- admissions(260, "C", "C")
    list(development = development, validation = validation)
  })
}

# Invented records for flow_example("cv"): 300 patients with 1 to 3
# records, whole patients assigned to 5 folds.
example_cv_data <- function() {
  with_example_seed(20261125, {
    visits <- sample(1:3, 300, replace = TRUE)
    pid <- rep(sprintf("P%03d", 1:300), visits)
    fold_of <- sample(rep(1:5, length.out = 300))
    data.frame(patient_id = pid, fold = rep(fold_of, visits),
               stringsAsFactors = FALSE)
  })
}

# Invented monthly sales for flow_example("forecast"): three stores,
# January 2016 to December 2023.
example_forecast_data <- function() {
  with_example_seed(20261126, {
    month <- seq(as.Date("2016-01-01"), as.Date("2023-12-01"), by = "month")
    d <- expand.grid(month = month, store = c("North", "Central", "South"),
                     stringsAsFactors = FALSE)
    d$sales <- round(100 + 10 * sin(as.numeric(format(d$month, "%m")) / 2) +
                       stats::rnorm(nrow(d), 0, 5))
    d
  })
}

