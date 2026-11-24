# Machine-learning steps: partitions and outcome events (milestone 5) -------

num <- function(v) as.numeric(gsub(",", "", v))

visits <- data.frame(
  pid = c(1, 1, 2, 3, 3, 4, 5, 6, 7, 7),
  date = as.Date("2020-01-01") + c(0, 30, 60, 90, 95, 400, 420, 500, 510, 515),
  set = c("train", "train", "train", "train", "train", "test", "test",
          "test", "test", "test"),
  died = c(0, 1, 0, 0, 0, 1, 0, 1, NA, 0),
  stringsAsFactors = FALSE
)

test_that("typed partitions are splits of their own type", {
  d <- flow_start("Patients", n = 1000) |>
    flow_partition(labels = c("Training", "Test"), n = c(800, 200))
  expect_equal(d$stages$type[2], "partition")
  expect_equal(d$stages$name[2], "Data split")
  expect_equal(d$nodes$n, c("1,000", "800", "200"))
  expect_match(flow_describe(d),
               "Data split: Patients is partitioned into Training (n = 800) and Test (n = 200).",
               fixed = TRUE)
  g <- flow_start("Patients", n = 1000) |>
    flow_partition(labels = c("Training", "Test"), group = "patient",
                   time = "admission date")
  expect_match(flow_describe(g),
               "all records of each patient are in one set (not checked against data)",
               fixed = TRUE)
  expect_match(flow_describe(g),
               "older data for training, newer for testing) (not checked against data)",
               fixed = TRUE)
  expect_error(flow_partition(flow_start("a"), labels = "b", group = 1),
               "single piece of text")
})

test_that("data partitions: counts, grouped check, time order and ranges", {
  d <- flow_start("Visits", data = visits) |>
    flow_partition(labels = c("Training" = "train", "Test" = "test"),
                   by = "set", group = "pid", time = "date")
  expect_equal(num(d$nodes$n[2:3]), as.numeric(table(visits$set)[c("train", "test")]))
  expect_equal(d$nodes$note[2:3], c("2020-01-01 to 2020-04-05",
                                    "2021-02-04 to 2021-05-30"))
  txt <- flow_describe(d)
  expect_match(txt, "all records of each pid are in one set;", fixed = TRUE)
  expect_false(grepl("not checked", txt))
  # a given note wins over the date range
  n <- flow_start("Visits", data = visits) |>
    flow_partition(labels = c("train", "test"), by = "set", time = "date",
                   note = "my note")
  expect_equal(n$nodes$note[2:3], c("my note", "my note"))
  # patient 3 in both sets
  leak <- visits
  leak$set[5] <- "test"
  expect_error(flow_start("Visits", data = leak) |>
                 flow_partition(by = "set", group = "pid"),
               "1 value of \"pid\" (e.g. 3) is in more than one set across the boxes of stage \"Data split\": test and train share 1",
               fixed = TRUE)
  # sets out of time order (labels give the order)
  expect_error(flow_start("Visits", data = visits) |>
                 flow_partition(labels = c("test", "train"), by = "set",
                                time = "date"),
               "test runs to 2021-05-30 but train starts on 2020-01-01",
               fixed = TRUE)
  # equal times in two sets are an overlap
  tie <- visits
  tie$date[6] <- tie$date[5]
  expect_error(flow_start("Visits", data = tie) |>
                 flow_partition(labels = c("train", "test"), by = "set",
                                time = "date"), "not in time order")
  # without `by`, nothing is checked and the description says so
  u <- flow_start("Visits", data = visits) |>
    flow_partition(labels = c("A", "B"), group = "pid")
  expect_match(flow_describe(u), "(not checked against data)", fixed = TRUE)
})

test_that("typed events are drawn with the count and described", {
  d <- flow_start("Patients", n = 1000, events = 80) |>
    flow_partition(labels = c("Training", "Test"), n = c(800, 200),
                   events = c(64, 16)) |>
    flow_apply("Model", "Fitted", events = 64, from = "Training") |>
    flow_outcome("Result", "Checked", from = "Test", events = 16)
  expect_equal(unname(d$events[c("1", "2", "3", "4", "5")]),
               c("80", "64", "16", "64", "16"))
  lay <- flow_layout(d)
  expect_equal(lay$boxes$text[1:2], c("Patients\n(n = 1,000; events = 80)",
                                      "Training\n(n = 800; events = 64)"))
  expect_equal(lay$boxes$text[4], "Fitted\n(events = 64)")
  expect_match(flow_describe(d), "Patients (n = 1,000; events = 80)",
               fixed = TRUE)
  expect_equal(flow_nodes(d)$events, c("80", "64", "16", "64", "16"))
  # {events} in the format places them
  f <- flow_layout(d, count_format = "{label}\n{n} ({events} events)")
  expect_equal(f$boxes$text[2], "Training\n800 (64 events)")
  # pools and splits take events too
  p <- flow_start(c("a", "b"), n = c(5, 6), events = c(1, 2)) |>
    flow_pool("P", "All", n = 11, events = 3) |>
    flow_split("S", c("x", "y"), n = c(4, 7), events = c(1, 2))
  expect_equal(unname(p$events), c("1", "2", "3", "1", "2"))
  expect_error(flow_start("a", events = ~ died == 1), "give `data` too")
  expect_error(flow_start("a", n = 2) |> flow_split("S", c("x", "y"),
                                                   events = 1:3),
               "2 boxes to fill")
})

test_that("events from data: every box with rows counts its events", {
  d <- flow_start("Visits", data = visits, events = ~ died == 1) |>
    flow_exclude("Missing outcome", where = ~ is.na(died)) |>
    flow_partition(labels = c("train", "test"), by = "set") |>
    flow_outcome("Analysis", "Analysed")
  ev <- function(rows) sum(visits$died[rows] == 1, na.rm = TRUE)
  keep <- !is.na(visits$died)
  expect_equal(num(flow_nodes(d)$events),
               c(ev(TRUE), ev(keep & visits$set == "train"),
                 ev(keep & visits$set == "test"),
                 ev(keep & visits$set == "train"),
                 ev(keep & visits$set == "test")))
  # counted in `count` units: patients with an event
  p <- flow_start("Visits", data = visits, count = "pid",
                  events = ~ died == 1)
  expect_equal(p$events[["1"]],
               as.character(length(unique(visits$pid[visits$died %in% 1]))))
  # boxes without rows have no events
  t <- flow_start("Visits", data = visits, events = ~ died == 1) |>
    flow_split("S", c("x", "y"))
  expect_true(all(is.na(flow_nodes(t)$events[2:3])))
  # editing events marks them as typed; rows untouched
  expect_warning(e <- flow_edit(d, "Analysed", events = 9), "n = 5")
  expect_identical(e$data, d$data)
  expect_match(flow_describe(e), "typed by hand: Analysed (events = 9) and Analysed (events = 9)",
               fixed = TRUE)
  expect_error(flow_edit(d, 1, events = 2, excluded = TRUE), "do not show")
  # typed events in a data flow are listed as typed too
  ty <- flow_start("Visits", data = visits) |>
    flow_partition(labels = c("train", "test"), by = "set", events = c(1, 2))
  expect_match(flow_describe(ty),
               "typed by hand: train (events = 1) and test (events = 2)",
               fixed = TRUE)
})

test_that("flows without events are drawn as before", {
  lay <- flow_layout(flow_example("trial"))
  expect_false(any(grepl("events", lay$boxes$text)))
  expect_true(all(is.na(flow_nodes(flow_example("trial"))$events)))
})

test_that("the ml example: counts and events trace to its data", {
  d <- flow_example("ml")
  expect_match(attr(d, "source"), "TRIPOD\\+AI")
  expect_equal(d$data$names, c("development", "validation"))
  dev <- d$data$sources[[1]]
  val <- d$data$sources[[2]]
  ok <- function(z) z$age >= 18 & !is.na(z$died_30d)
  nd <- flow_nodes(d)
  get <- function(lbl, col = "n") num(nd[[col]][nd$label == lbl])
  expect_equal(get("Development data"), nrow(dev))
  expect_equal(get("Training set"), sum(ok(dev) & dev$set == "train"))
  expect_equal(get("Test set"), sum(ok(dev) & dev$set == "test"))
  expect_equal(get("External validation"), sum(ok(val)))
  expect_equal(get("Test set", "events"),
               sum(ok(dev) & dev$set == "test" & dev$died_30d == 1))
  expect_equal(get("External validation", "events"),
               sum(ok(val) & val$died_30d == 1))
  # grouped split: no patient in both sets
  expect_length(intersect(dev$patient_id[dev$set == "train"],
                          dev$patient_id[dev$set == "test"]), 0)
  expect_match(flow_describe(d), "all records of each patient_id are in one set.",
               fixed = TRUE)
  expect_match(flow_describe(d), "External validation (n = 348; events = 18)",
               fixed = TRUE)
  # the example leaves the caller's random numbers alone
  set.seed(1)
  a <- stats::runif(1)
  set.seed(1)
  invisible(flow_example("ml"))
  expect_equal(stats::runif(1), a)
})

test_that("the ml diagram looks right", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("ml design", autoplot(flow_example("ml")))
})

test_that("the group check runs across all boxes of the stage", {
  # patient 1 is in Train under arm x and in Test under arm y
  dd <- data.frame(arm = rep(c("x", "y"), each = 4),
                   g = c(1, 1, 2, 2, 3, 3, 1, 1),
                   s = c("a", "a", "b", "b", "a", "a", "b", "b"))
  arms <- flow_start("D", data = dd) |> flow_split("Arm", by = "arm")
  expect_error(flow_partition(arms, labels = c(Train = "a", Test = "b"),
                              by = "s", group = "g"),
               "1 value of \"g\" (e.g. 1) is in more than one set",
               fixed = TRUE)
  dd$g[7:8] <- 4
  ok <- flow_start("D", data = dd) |> flow_split("Arm", by = "arm") |>
    flow_partition(labels = c(Train = "a", Test = "b"), by = "s", group = "g")
  expect_match(flow_describe(ok), "all records of each g are in one set",
               fixed = TRUE)
})

test_that("the time check runs across all boxes and refuses text", {
  dd <- data.frame(arm = rep(c("x", "y"), each = 2),
                   t = c(1, 2, 3, 4), s = c("a", "b", "a", "b"))
  # in order within each arm, but arm y trains on times after arm x's test
  expect_error(flow_start("D", data = dd) |> flow_split("Arm", by = "arm") |>
                 flow_partition(labels = c(Train = "a", Test = "b"),
                                by = "s", time = "t"),
               "Train runs to 3 but Test starts on 2", fixed = TRUE)
  # as text "10" sorts before "9", so a wrong order would pass
  tx <- data.frame(t = c("10", "10", "9", "9"), s = c("a", "a", "b", "b"))
  part <- function(d) {
    flow_start("D", data = d) |>
      flow_partition(labels = c(Train = "a", Test = "b"), by = "s",
                     time = "t")
  }
  expect_error(part(tx), "must hold dates, date-times or numbers")
  expect_error(part(transform(tx, t = factor(t))), "it holds factor values")
  expect_error(part(transform(tx, t = as.numeric(t))),
               "Train runs to 10 but Test starts on 9", fixed = TRUE)
  # years are not written with thousands separators
  yr <- part(data.frame(t = c(2001, 2002), s = c("a", "b")))
  expect_equal(yr$nodes$note[2:3], c("2001 to 2001", "2002 to 2002"))
})

test_that("missing groups are warned about and described", {
  dd <- data.frame(g = c(NA, NA, 2, 3), s = c("a", "b", "a", "b"))
  expect_warning(
    p <- flow_start("D", data = dd) |>
      flow_partition(by = "s", group = "g"),
    "2 rows have a missing \"g\"; they cannot be checked", fixed = TRUE)
  expect_match(flow_describe(p), "(2 rows with missing g not checked)",
               fixed = TRUE)
})

test_that("missing outcomes are neither events nor non-events, and said so", {
  # visits has one missing outcome, in the test set
  d <- flow_start("Visits", data = visits, events = ~ died == 1) |>
    flow_partition(labels = c("train", "test"), by = "set")
  fmt <- "{label}: {n}, {events} with and {non_events} without"
  expect_equal(flow_layout(d, count_format = fmt)$boxes$text,
               c("Visits: 10, 3 with and 6 without",
                 "train: 5, 1 with and 4 without",
                 "test: 5, 2 with and 2 without"))
  expect_match(flow_describe(d),
               "1 row has a missing outcome: counted in n, but neither as events nor as rows without the event; 1 of them is left in the end boxes.",
               fixed = TRUE)
  expect_warning(autoplot(d), "1 row in the end boxes has a missing outcome",
                 fixed = TRUE)
  # excluded: no warning, and the description says none is left
  ex <- flow_start("Visits", data = visits, events = ~ died == 1) |>
    flow_exclude("Missing outcome", where = ~ is.na(died))
  expect_no_warning(autoplot(ex))
  expect_match(flow_describe(ex), "none of them is left in the end boxes",
               fixed = TRUE)
  # typed: n minus events
  ty <- flow_start("P", n = 100, events = 8)
  expect_equal(flow_layout(ty, count_format = fmt)$boxes$text,
               "P: 100, 8 with and 92 without")
  # an edited count too: the computed 4 would not go with a typed n
  en <- flow_edit(d, "train", n = 3)
  expect_equal(flow_layout(en, count_format = fmt)$boxes$text[2],
               "train: 3, 1 with and  without")
  expect_equal(flow_layout(flow_edit(d, "train", note = "x"),
                           count_format = fmt)$boxes$text[2],
               "train: 5, 1 with and 4 without")
  # an edited number of events leaves the rows without it unknown
  ed <- flow_edit(d, "train", events = 3)
  expect_equal(flow_layout(ed, count_format = fmt)$boxes$text[2],
               "train: 5, 3 with and  without")
})

test_that("typed events that cannot be right warn; a forgotten ~ is explained", {
  expect_warning(flow_start("P", n = 10, events = 11),
                 "has 11 events but n = 10", fixed = TRUE)
  expect_warning(flow_start("P", n = 10, events = -1),
                 "between 0 and n", fixed = TRUE)
  expect_warning(flow_edit(flow_start("P", n = 10, events = 1), 1,
                           events = 12), "12 events")
  expect_error(flow_start("V", data = visits, events = no_such_col == 1),
               "note the ~", fixed = TRUE)
})
