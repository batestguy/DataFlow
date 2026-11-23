# Counts computed from data (milestone 4, part B) ---------------------------

people <- data.frame(
  id = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10),
  age = c(30, 30, 17, 45, 15, 60, 22, 70, 35, NA, 50, 50),
  consent = c("yes", "yes", "no", "yes", "no", "yes", "no", "yes", "yes",
              "yes", "yes", "yes"),
  site = c("North", "North", "South", "South", "North", "South", "North",
           "South", "South", "North", "North", "North"),
  arm = factor(c("B", "B", NA, "A", NA, "B", NA, "A", "B", "A", "A", "A"),
               levels = c("B", "A")),
  stringsAsFactors = FALSE
)
num <- function(v) as.numeric(gsub(",", "", v))

test_that("flow_start counts rows, or distinct values with `count`", {
  d <- flow_start("All", data = people)
  expect_equal(d$nodes$n, "12")
  expect_equal(d$data$names, "people")
  expect_equal(flow_data(d, 1), people)
  p <- flow_start("All", data = people, count = "id")
  expect_equal(p$nodes$n, as.character(length(unique(people$id))))
  expect_error(flow_start("All", n = 3, data = people), "not both")
  expect_error(flow_start("All", count = "id"), "give `data` too")
  expect_error(flow_start("All", data = people, count = "nope"),
               "not in the data frame `people`")
  expect_error(flow_start(c("a", "b"), data = people), "one data frame per")
  expect_error(flow_start("All", data = 1:3), "must be a data frame")
})

test_that("by = splits by the values present, with computed counts", {
  d <- flow_start("All", data = people) |> flow_split("Site", by = "site")
  kids <- d$nodes[d$nodes$stage == 2, ]
  expect_equal(kids$label, c("North", "South"))
  expect_equal(num(kids$n), as.numeric(table(people$site)))
  expect_equal(flow_data(d, "North"), people[people$site == "North", ])
  # nested: values present under each parent only
  dd <- d |> flow_split("Consent", by = "consent")
  third <- dd$nodes[dd$nodes$stage == 3, ]
  expect_equal(third$label, c("no", "yes", "no", "yes"))
  expect_equal(num(third$n), c(
    sum(people$site == "North" & people$consent == "no"),
    sum(people$site == "North" & people$consent == "yes"),
    sum(people$site == "South" & people$consent == "no"),
    sum(people$site == "South" & people$consent == "yes")))
})

test_that("by = uses factor level order and refuses missing values", {
  d <- flow_start("All", data = people) |>
    flow_exclude("No arm", where = ~ is.na(arm))
  a <- flow_assign(d, "Arm", by = "arm")
  expect_equal(a$nodes$label[2:3], c("B", "A"))
  expect_equal(num(a$nodes$n[2:3]), c(sum(people$arm == "B", na.rm = TRUE),
                                      sum(people$arm == "A", na.rm = TRUE)))
  expect_error(flow_assign(flow_start("All", data = people), "Arm",
                           by = "arm"),
               "3 missing values in the rows of box \"All\". Remove them")
  expect_error(flow_split(flow_start("All", data = people), "S", by = "zz"),
               "Column \"zz\" is not in the data frame `people`")
  expect_error(flow_split(flow_start("All", n = 1), "S", by = "site"),
               "needs a flow with data")
  expect_error(flow_split(flow_start("All", data = people), "S",
                          by = "site", n = 4), "not both")
})

test_that("labels with by: order, readable names, planned values", {
  base <- flow_start("All", data = people)
  d <- flow_split(base, "Site", c("South", "North"), by = "site")
  expect_equal(d$nodes$label[2:3], c("South", "North"))
  m <- flow_split(base, "Site", c("Northern" = "North", "Southern" = "South",
                                  "East (planned)" = "East"), by = "site")
  expect_equal(m$nodes$label[2:4], c("Northern", "Southern",
                                     "East (planned)"))
  expect_equal(num(m$nodes$n[2:4]), c(sum(people$site == "North"),
                                      sum(people$site == "South"), 0))
  expect_equal(nrow(flow_data(m, "East (planned)")), 0)
  # partly named: unnamed ones keep the value as text
  pn <- flow_split(base, "Site", c("Northern" = "North", "South"),
                   by = "site")
  expect_equal(pn$nodes$label[2:3], c("Northern", "South"))
  # a value present but not listed is never dropped silently
  expect_error(flow_split(base, "Site", "North", by = "site"),
               "misses \"South\"")
  expect_error(flow_split(base, "Site", c(a = "North", a = "South"),
                          by = "site"), "must differ")
})

test_that("summary with by: {k} distinct values, n of the parent", {
  d <- flow_start("All", data = people) |>
    flow_split("Site", by = "site") |>
    flow_cluster("Consent", by = "consent", summary = "{k} answers")
  s <- d$nodes[d$nodes$stage == 3, ]
  expect_equal(s$label, c("2 answers", "2 answers"))
  expect_equal(num(s$n), as.numeric(table(people$site)))
})

test_that("flow_select(where =) keeps matching rows; NA does not match", {
  d <- flow_start("All", data = people) |>
    flow_select("Adults", "Aged 18 or over", where = ~ age >= 18)
  expect_equal(num(d$nodes$n[2]), sum(people$age >= 18, na.rm = TRUE))
  expect_equal(flow_data(d, "Aged 18 or over"),
               people[!is.na(people$age) & people$age >= 18, ])
  # names outside the data come from the formula's environment
  cut <- 40
  e <- flow_start("All", data = people) |>
    flow_select("Old", "Older", where = ~ age > cut)
  expect_equal(num(e$nodes$n[2]), sum(people$age > 40, na.rm = TRUE))
  expect_error(flow_select(flow_start("All", data = people), "S", "s",
                           where = ~ age, n = 2), "not both")
  expect_error(flow_select(flow_start("All", data = people), "S", "s",
                           where = "age > 3"), "one-sided formula")
  expect_error(flow_select(flow_start("All", data = people), "S", "s",
                           where = ~ age + 1), "TRUE or FALSE")
  expect_error(flow_select(flow_start("All", n = 2), "S", "s",
                           where = ~ age > 1), "needs a flow with data")
  # without where, a selection has no rows: counts stay as typed
  s <- flow_select(flow_start("All", data = people), "S", "25 each")
  expect_true(is.na(s$nodes$n[2]))
})

test_that("exclusion reasons: first match wins; the rest continue", {
  d <- flow_start("All", data = people) |>
    flow_exclude("Excluded", reasons = list("Under 18" = ~ age < 18,
                                            "Declined" = ~ consent == "no"))
  under <- !is.na(people$age) & people$age < 18
  declined <- people$consent == "no"
  expect_equal(num(d$side$n), sum(under | declined))
  # the 17- and 15-year-olds also declined: counted under "Under 18" only
  expect_equal(num(d$reasons$n), c(sum(under), sum(declined & !under)))
  expect_equal(sum(num(d$reasons$n)), num(d$side$n))
  expect_equal(nrow(flow_data(d, "Excluded", excluded = TRUE)),
               sum(under | declined))
  # the flow continues with the remaining rows, but the box keeps all rows
  nxt <- flow_apply(d, "Included", "Included")
  expect_equal(flow_data(nxt, "Included"), people[!(under | declined), ])
  expect_equal(nrow(flow_data(nxt, "All")), nrow(people))
  # where: a box without reasons
  w <- flow_exclude(nxt, "No age", where = ~ is.na(age)) |>
    flow_outcome("End", "Analysed")
  expect_equal(num(w$side$n[2]), 1)
  expect_equal(num(w$nodes$n[3]), sum(!(under | declined)) - 1)
})

test_that("exclusions from data: errors and typed exclusions", {
  base <- flow_start("All", data = people)
  expect_error(flow_exclude(base, "X", n = 3,
                            reasons = list(a = ~ age < 18)), "not both")
  expect_error(flow_exclude(base, "X", reasons = list(a = ~ age < 18),
                            where = ~ age > 3), "not both")
  expect_error(flow_exclude(base, "X", reasons = list(~ age < 18)),
               "unique name")
  expect_error(flow_exclude(flow_start("A", n = 3), "X",
                            reasons = list(a = ~ age < 18)),
               "needs a flow with data")
  # a typed exclusion does not say which rows left: counts below go blank
  t <- flow_exclude(base, "Lost", n = 2) |> flow_apply("Next", "Next")
  expect_true(is.na(t$nodes$n[2]))
  expect_error(flow_data(t, "Next"), "no rows")
})

test_that("boxes made without data have no rows; counts below stay blank", {
  d <- flow_start("All", data = people) |>
    flow_split("Arm", c("X", "Y"), n = 6) |>
    flow_apply("Treated", "Treated") |>
    flow_outcome("Out", "600 trials", n = 600)
  expect_equal(d$nodes$n, c("12", "6", "6", NA, NA, "600", "600"))
  expect_error(flow_split(d, "S", by = "site"), "made without data")
  expect_error(flow_data(d, "X"), "no rows behind it")
  # a typed n always wins over the computed one
  e <- flow_start("All", data = people) |> flow_apply("T", "Trials", n = 600)
  expect_equal(e$nodes$n[2], "600")
  expect_equal(nrow(flow_data(e, "Trials")), nrow(people))
})

test_that("several data frames: one per stream, pooled across them", {
  cases <- people[1:5, ]
  controls <- data.frame(id = 101:104, age = c(40, 41, 12, 50),
                         consent = "yes", site = "North", arm = NA)
  d <- flow_start(c("Cases", "Controls"), data = list(cases, controls)) |>
    flow_exclude("Under 18", where = ~ age < 18) |>
    flow_pool("Analysis", "All analysed")
  expect_equal(d$data$names, c("cases", "controls"))
  expect_equal(num(d$nodes$n[1:2]), c(5, 4))
  expect_equal(num(d$side$n), c(sum(cases$age < 18), 1))
  expect_equal(num(d$nodes$n[3]), sum(cases$age >= 18) + 3)
  out <- flow_data(d, "All analysed")
  expect_named(out, c("cases", "controls"))
  expect_equal(out$controls, controls[controls$age >= 18, ])
  # a named list names the sources
  n2 <- flow_start(c("a", "b"), data = list(A = cases, B = controls))
  expect_equal(n2$data$names, c("A", "B"))
  expect_match(flow_describe(d),
               "Counts were computed from the data frames `cases` (5 rows) and `controls` (4 rows).",
               fixed = TRUE)
  # count = column across streams
  p <- flow_start(c("a", "b"), data = list(people, people), count = "id")
  expect_equal(num(p$nodes$n), rep(length(unique(people$id)), 2))
})

test_that("flow_data finds boxes by id or label and refuses bad input", {
  d <- flow_start("All", data = people) |> flow_split("Site", by = "site") |>
    flow_apply("T", "Treated")
  expect_equal(flow_data(d, 2), flow_data(d, "North"))
  expect_error(flow_data(d, "Treated"), "Several boxes")
  expect_error(flow_data(d, "Nope"), "No box labelled")
  expect_error(flow_data(d, 99), "No box with id")
  expect_error(flow_data(d, c(1, 2)), "one box id or label")
  expect_error(flow_data(flow_example("lab"), 1), "no data")
  # every count equals the number of rows flow_data() returns
  for (id in d$nodes$id) {
    expect_equal(num(d$nodes$n[d$nodes$id == id]), nrow(flow_data(d, id)))
  }
})

test_that("printing a flow with data does not dump the data", {
  d <- flow_start("All", data = people)
  out <- capture.output(print(d))
  expect_equal(length(out), 3)
  expect_match(out[2], "counts from data: people (12 rows)", fixed = TRUE)
})

test_that("the trial_data example: counts trace to its data frame", {
  d <- flow_example("trial_data")
  td <- d$data$sources[[1]]
  expect_equal(d$data$names, "trial_data")
  expect_match(attr(d, "source"), "set.seed\\(20261123\\)")
  expect_equal(num(d$nodes$n[1]), nrow(td))
  ok <- td$age >= 18 & td$eligible & td$consent == "yes"
  expect_equal(num(d$nodes$n[d$nodes$label == "Randomised"]), sum(ok))
  expect_equal(num(d$side$n[1]), sum(!ok))
  expect_equal(sum(num(d$reasons$n[d$reasons$side == 1])), sum(!ok))
  for (arm in c("Exercise programme", "Usual care")) {
    rows <- ok & td$arm %in% arm & td$status == "completed" &
      !is.na(td$outcome_score)
    expect_equal(num(d$nodes$n[d$nodes$label == "Analysed" &
                                 d$nodes$id == d$nodes$id[d$nodes$label == arm] + 2]),
                 sum(rows))
  }
  expect_match(flow_describe(d),
               "Counts were computed from the data frame `trial_data` (412 rows).",
               fixed = TRUE)
  # the example does not change the caller's random numbers
  set.seed(1)
  a <- stats::runif(1)
  set.seed(1)
  invisible(flow_example("trial_data"))
  expect_equal(stats::runif(1), a)
})

test_that("the trial_data diagram looks right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("trial data design",
                              autoplot(flow_example("trial_data")))
})

test_that("flows without data are unchanged from main (layouts as data)", {
  skip_on_cran()
  # fonts differ between machines
  skip_on_ci()
  ref <- readRDS(test_path("fixtures", "typed_layouts_main.rds"))
  for (nm in names(ref)) {
    lay <- unclass(flow_layout(flow_example(nm)))
    expect_identical(lay, ref[[nm]], label = nm)
  }
})

# Editing ---------------------------------------------------------------------

test_that("flow_edit changes labels, counts and notes, rows untouched", {
  d <- flow_start("All", data = people) |>
    flow_split("Site", c("N" = "North", "S" = "South"), by = "site")
  before <- d$data
  e <- d |>
    flow_edit("N", label = "Northern sites", note = "two districts") |>
    flow_edit(3, n = 99)
  expect_identical(e$data, before)
  expect_equal(e$nodes$label[2], "Northern sites")
  expect_equal(e$nodes$note[2], "two districts")
  expect_equal(e$nodes$n[3], "99")
  expect_equal(flow_nodes(e)$n_edited, c(FALSE, FALSE, TRUE))
  expect_equal(flow_data(e, 3), people[people$site == "South", ])
  expect_match(flow_describe(e),
               "Counts were computed from the data frame `people` (12 rows), except 1 typed by hand: S (n = 99).",
               fixed = TRUE)
  # NA clears
  expect_true(is.na(flow_edit(e, 2, note = NA)$nodes$note[2]))
  # one value per box
  two <- flow_edit(d, c(2, 3), label = c("a", "b"))
  expect_equal(two$nodes$label[2:3], c("a", "b"))
})

test_that("flow_edit: typed boxes in a data flow are listed as typed", {
  d <- flow_start("All", data = people) |>
    flow_split("Arm", c("X", "Y"), n = 6)
  expect_match(flow_describe(d),
               "except 2 typed by hand: X (n = 6) and Y (n = 6).",
               fixed = TRUE)
})

test_that("flow_edit works on typed flows, exclusion boxes and row labels", {
  t <- flow_example("trial")
  e <- t |>
    flow_edit("Randomised", n = 301) |>
    flow_edit("Excluded", n = 111, label = "Not included", excluded = TRUE) |>
    flow_edit(stage = "Allocation", row_label = "Arms")
  expect_equal(e$nodes$n[e$nodes$label == "Randomised"], "301")
  expect_equal(e$side$label[1], "Not included")
  expect_equal(e$side$n[1], "111")
  expect_equal(e$stages$row_label[e$stages$name == "Allocation"], "Arms")
  expect_equal(sum(flow_nodes(e)$n_edited), 1)
  expect_equal(flow_layout(e)$row_labels$label,
               c("Enrolment", "Arms", "Follow-up", "Analysis"))
  # typed flows get no data sentence
  expect_false(grepl("Counts were computed", flow_describe(e)))
  expect_error(flow_edit(t, "Nope", n = 1), "No box labelled \"Nope\"")
  expect_error(flow_edit(t, 99, n = 1), "No box with id 99")
  expect_error(flow_edit(t, 1, n = 1, excluded = TRUE, label = ""),
               "not empty")
  expect_error(flow_edit(t, stage = "Nope", row_label = "x"), "no stage")
  expect_error(flow_edit(t, 1, stage = "Allocation", row_label = "x"),
               "not both")
  expect_error(flow_edit(t), "Give `box`")
  expect_error(flow_edit(t, c(1, 2), n = c(1, 2, 3)), "2 boxes to fill")
})

# Drawing a data frame's structure -------------------------------------------

test_that("flow_from_data rebuilds the lab design's nested structure", {
  lab_data <- data.frame(
    `Experiment ID` = rep(c("Ex. 1", "Ex. 2"), each = 50),
    `Culture type` = rep(c("Type A", "Type B"), each = 50),
    Batch = rep(rep(c("Batch 1", "Batch 2"), each = 25), 2),
    `Cell culture composition` = rep(c("A", "B", "C"), length.out = 100),
    check.names = FALSE
  )
  levels <- c("Experiment ID", "Culture type", "Batch",
              "Cell culture composition")
  d <- flow_from_data(lab_data, levels, label = "Cultures")
  lab <- flow_example("lab")
  rows <- 1:5
  got <- d$nodes[d$nodes$stage %in% rows, ]
  want <- lab$nodes[lab$nodes$stage %in% rows, ]
  expect_equal(got$label, want$label)
  expect_equal(got$anchor, want$anchor)
  expect_equal(d$stages$name[2:5], lab$stages$name[2:5])
  # counts match the typed lab design wherever it gives one (rows 1 to 4);
  # its composition boxes carry no count, the data give each one
  typed <- !is.na(want$n)
  expect_equal(got$n[typed], want$n[typed])
  comp <- got[got$stage == 5, ]
  expect_equal(sum(num(comp$n)), 100)
  expect_equal(d$data$names, "lab_data")
  # it is an ordinary flow: the lab's remaining steps can be piped on
  full <- d |>
    flow_apply("Condition", "9 conditions each", over = "Experiment ID") |>
    flow_apply("Task", "2 tasks each\n(per condition)", n = NA_character_) |>
    flow_outcome("Number of trials", "600 each", n = NA_character_)
  expect_equal(nrow(full$nodes), nrow(lab$nodes))
  expect_equal(num(full$nodes$n[full$nodes$label == "9 conditions each"]),
               c(50, 50))
  # readable names and planned values through `labels`
  m <- flow_from_data(lab_data, "Batch", labels = list(
    Batch = c("First" = "Batch 1", "Second" = "Batch 2", "Third" = "Batch 3")
  ), row_labels = "Batches")
  expect_equal(m$nodes$label[2:4], c("First", "Second", "Third"))
  expect_equal(m$nodes$n[2:4], c("50", "50", "0"))
  expect_equal(m$stages$row_label[2], "Batches")
  expect_error(flow_from_data(lab_data, "Nope"), "Not columns")
  expect_error(flow_from_data(lab_data, "Batch", row_labels = c("a", "b")),
               "one per column")
  expect_error(flow_from_data(lab_data, "Batch", labels = list(x = "a")),
               "named by columns")
  expect_error(flow_from_data(1:3, "a"), "must be a data frame")
})
