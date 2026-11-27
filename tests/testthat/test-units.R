# Units: what each number counts (part 7b)

f01 <- function(units = TRUE) {
  u <- function(v) if (units) v
  flow_start("Deliveries of paste") |>
    flow_select("Deliveries", "Sampled", n = 10,
                method = "simple random sampling", unit = u("deliveries")) |>
    flow_select("Casks", "3 per delivery", n = 30,
                method = "simple random sampling", unit = u("casks")) |>
    flow_apply("Analyses", "2 per cask sample", n = 60, unit = u("analyses")) |>
    flow_outcome("Measured", "Paste strength", n = 60)
}

survey <- function() {
  flow_start("Villages sampled", n = 40, unit = "villages") |>
    flow_select("Households", "20 per village", n = 800, unit = "households") |>
    flow_exclude("Refused", n = 12,
                 reasons = c("Not at home" = 7, "Declined" = 5)) |>
    flow_apply("Interviewed", "Households interviewed", n = 788) |>
    flow_select("People", "All members", n = 3200, unit = "people")
}

test_that("boxes and exclusion boxes show counts with their unit", {
  lay <- flow_layout(survey())
  expect_equal(lay$boxes$text, c("Villages sampled\n(40 villages)",
                                 "20 per village\n(800 households)",
                                 "Households interviewed\n(788 households)",
                                 "All members\n(3,200 people)"))
  expect_equal(lay$side$text, paste0("Refused (12 households)\n",
                                     "- Not at home (7 households)\n",
                                     "- Declined (5 households)"))
})

test_that("without units nothing changes", {
  d <- flow_start("P", n = 10) |>
    flow_exclude("Out", n = 2, reasons = c("A" = 1, "B" = 1)) |>
    flow_split("Arm", c("X", "Y"), n = 4)
  expect_null(d$units)
  expect_null(d$side_units)
  expect_false("unit" %in% names(flow_nodes(d)))
  lay <- flow_layout(d)
  expect_equal(lay$boxes$text[2], "X\n(n = 4)")
  expect_equal(lay$side$text, "Out (n = 2)\n- A (n = 1)\n- B (n = 1)")
  # a format with {unit} on a box without one: the unit is empty
  lay <- flow_layout(d, count_format = "{label}: {n} {unit}")
  expect_equal(lay$boxes$text[1], "P: 10 ")
})

test_that("user formats: 'n = {n}' takes the unit, and {unit} works", {
  d <- flow_start("Cultures", n = 100, unit = "cultures")
  expect_equal(flow_layout(d, count_format = "{label} [n = {n}]")$boxes$text,
               "Cultures [100 cultures]")
  expect_equal(flow_layout(d, count_format = "{label}: {n} {unit}")$boxes$text,
               "Cultures: 100 cultures")
  expect_equal(flow_layout(d, count_format = "{label}: {n}")$boxes$text,
               "Cultures: 100")
  e <- d |> flow_exclude("Lost", n = 3, unit = "plates")
  expect_equal(flow_layout(e, side_format = "{label}: n = {n}")$side$text,
               "Lost: 3 plates")
  expect_equal(flow_layout(e, side_format = "{label} - {unit}")$side$text,
               "Lost - plates")
})

test_that("events read after the unit", {
  d <- flow_start("Patients", n = 412, events = 37, unit = "patients")
  expect_equal(flow_layout(d)$boxes$text,
               "Patients\n(412 patients; events = 37)")
  # an own {events} placeholder
  expect_equal(flow_layout(d, count_format = "{label} ({n} {unit}, {events} died)")$boxes$text,
               "Patients (412 patients, 37 died)")
})

test_that("copies say 'the same 30 casks as above'", {
  d <- flow_start("Casks", n = 30, unit = "casks") |>
    flow_copy("Tasters", c("Taster 1", "Taster 2"))
  expect_equal(flow_layout(d)$boxes$note[2], "the same 30 casks as above")
})

test_that("units are inherited down splits, applies and outcomes", {
  d <- flow_start("Cultures", n = 100, unit = "cultures") |>
    flow_split("Experiment", c("Ex. 1", "Ex. 2"), n = 50) |>
    flow_apply("Condition", "9 conditions", over = "Experiment") |>
    flow_outcome("Trials", "600 trials", n = 600, unit = "trials")
  u <- flow_nodes(d)$unit
  # a typed count keeps one outcome box per branch
  expect_equal(u, c(rep("cultures", 5), "trials", "trials"))
  # "" stops the unit from above
  e <- flow_start("A", n = 4, unit = "people") |>
    flow_split("S", c("x", "y"), n = 2, unit = c("", "people"))
  expect_equal(flow_nodes(e)$unit, c("people", NA, "people"))
})

test_that("pools take the sources' unit when they agree, else none", {
  agree <- flow_start(c("A", "B"), n = c(5, 6), unit = "records") |>
    flow_pool("Pool", "All", n = 11)
  expect_equal(flow_nodes(agree)$unit[3], "records")
  differ <- flow_start(c("A", "B"), n = c(5, 6), unit = c("records", "reports")) |>
    flow_pool("Pool", "All", n = 11)
  expect_true(is.na(flow_nodes(differ)$unit[3]))
  # an outcome measured in every branch: the branches agree
  m <- flow_start("P", n = 40, unit = "patients") |>
    flow_split("Arm", c("Drug", "Placebo"), n = 20) |>
    flow_outcome("Measured", "Pain")
  expect_true(any(m$edges$kind == "measure"))
  expect_equal(tail(flow_nodes(m)$unit, 1), "patients")
})

test_that("exclusion boxes take the parent's unit unless given their own", {
  d <- flow_start("H", n = 10, unit = "households") |>
    flow_exclude("Out", n = 2)
  expect_equal(unname(side_units(d)), "households")
  e <- flow_start("H", n = 10, unit = "households") |>
    flow_exclude("Out", n = 5, unit = "people")
  expect_equal(unname(side_units(e)), "people")
  expect_equal(flow_layout(e)$side$text, "Out (5 people)")
})

test_that("flow_edit() changes units, and the change carries down", {
  d <- flow_start("A", n = 10) |>
    flow_split("S", c("x", "y"), n = 5) |>
    flow_apply("T", "t")
  expect_null(d$units)
  e <- flow_edit(d, 1, unit = "sites")
  expect_equal(flow_nodes(e)$unit, rep("sites", 5))
  e <- flow_edit(e, "x", unit = "plots")
  expect_equal(flow_nodes(e)$unit, c("sites", "plots", "sites", "plots", "sites"))
  # NA removes a box's own unit
  e <- flow_edit(e, "x", unit = NA)
  expect_equal(flow_nodes(e)$unit, rep("sites", 5))
  expect_null(flow_edit(e, 1, unit = NA)$units)
  # exclusion boxes
  x <- flow_start("A", n = 10) |> flow_exclude("Out", n = 1)
  x <- flow_edit(x, 1, unit = "plates", excluded = TRUE)
  expect_equal(unname(x$side_units), "plates")
  # rows and structure untouched
  expect_identical(flow_edit(d, 1, unit = "sites")[c("nodes", "edges")],
                   d[c("nodes", "edges")])
})

test_that("unit values are recycled like n, with the same errors", {
  d <- flow_start("A", n = 4) |>
    flow_split("S", c("x", "y"), n = 2, unit = c("plots", "pots"))
  expect_equal(unname(d$units), c("plots", "pots"))
  d2 <- flow_start("A", n = 4) |>
    flow_split("S1", c("x", "y")) |>
    flow_split("S2", c("p", "q", "r"), unit = c("u1", "u2", "u3"))
  expect_equal(unname(d2$units), rep(c("u1", "u2", "u3"), 2))
  expect_error(flow_start("A") |> flow_split("S", c("x", "y"), unit = c("a", "b", "c")),
               "`unit` has 3 values")
  expect_error(flow_start("A", unit = 3), "`unit` must be text")
  expect_error(flow_start("A") |> flow_apply("T", "t", unit = c("a", "b")),
               "`unit` has 2 values")
  expect_error(flow_start("A") |> flow_exclude("Out", unit = c("a", "b")),
               "`unit` has 2 values")
  # every verb takes unit
  x <- flow_start("A", n = 100, unit = "people") |>
    flow_assign("Arm", c("D", "P"), n = 50, unit = "people") |>
    flow_cluster("Clusters", 4, unit = "clinics") |>
    flow_select("Sel", "s", unit = "visits") |>
    flow_copy("Copy", c("c1", "c2"), unit = "visits") |>
    flow_partition(labels = c("Train", "Test"), unit = "images")
  expect_true(all(c("people", "clinics", "visits", "images") %in% x$units))
})

test_that("validation checks the stored units", {
  d <- flow_start("A", n = 1, unit = "x")
  bad <- d
  bad$units <- c("99" = "x")
  expect_error(validate_studydesignflow(bad), "units of boxes")
  bad$units <- c("1" = 3)
  expect_error(validate_studydesignflow(bad), "units of boxes")
})

test_that("descriptions use units and say where they change", {
  txt <- as.character(flow_describe(f01()))
  expect_match(txt, "Sampled (10 deliveries)", fixed = TRUE)
  expect_match(txt, "3 per delivery (30 casks), from Sampled; counted in casks",
               fixed = TRUE)
  expect_false(grepl("counted in deliveries", txt))
  s <- as.character(flow_describe(survey()))
  expect_match(s, "Refused (12 households) after 20 per village: Not at home (7 households) and Declined (5 households)",
               fixed = TRUE)
  e <- as.character(flow_describe(
    flow_start("C", n = 100, unit = "cultures") |>
      flow_split("Ex", c("E1", "E2"), n = 50)))
  expect_match(e, "E1 (50 cultures) and E2 (50 cultures)", fixed = TRUE)
})

test_that("flow_check() does not compare across a change of unit", {
  ok <- flow_check(f01(TRUE))
  expect_equal(nrow(ok), 0)
  expect_equal(attr(ok, "compared"), 1L)
  old <- flow_check(f01(FALSE))
  expect_equal(old$level, c("note", "note"))
  expect_equal(attr(old, "compared"), 3L)
  sv <- flow_check(survey())
  expect_equal(nrow(sv), 0)
  # within one unit, problems are still found
  bad <- flow_start("H", n = 100, unit = "households") |>
    flow_split("S", c("a", "b"), n = c(60, 60))
  expect_equal(flow_check(bad)$level, "problem")
  # an exclusion in another unit is not subtracted
  ex <- flow_start("H", n = 10, unit = "households") |>
    flow_exclude("Out", n = 30, unit = "people") |>
    flow_apply("Next", "n", n = 10)
  expect_equal(nrow(flow_check(ex)), 0)
})

people_data <- function() {
  data.frame(person = 1:12,
             household = rep(c("h1", "h2", "h3", "h4", "h5"), c(3, 2, 3, 2, 2)),
             village = rep(c("v1", "v2"), c(5, 7)),
             age = c(30, 5, 40, 22, 8, 61, 15, 33, 70, 12, 45, 3))
}

test_that("count = counts distinct values of a column at that step", {
  pp <- people_data()
  d <- flow_start("People listed", data = pp) |>
    flow_split("Village", by = "village", count = "household",
               unit = "households") |>
    flow_select("Adults", "Aged 18 or over", where = ~ age >= 18,
                count = "person", unit = "people")
  nd <- flow_nodes(d)
  expect_equal(nd$n[1], "12")
  expect_equal(nd$n[2:3], c(as.character(length(unique(pp$household[pp$village == "v1"]))),
                            as.character(length(unique(pp$household[pp$village == "v2"])))))
  ad <- pp[pp$age >= 18, ]
  expect_equal(nd$n[4:5], as.character(c(sum(ad$village == "v1"), sum(ad$village == "v2"))))
  # inherited below: an apply under the village boxes counts households
  e <- flow_start("People listed", data = pp) |>
    flow_split("Village", by = "village", count = "household") |>
    flow_apply("Visited", "Visited")
  expect_equal(flow_nodes(e)$n[4:5], c("2", "3"))
  # exclusions count as their parent box
  # households with both children and adults: the warning says so
  w <- testthat::capture_warnings(x <- flow_start("People listed", data = pp) |>
    flow_split("Village", by = "village", count = "household") |>
    flow_exclude("Children only", where = ~ age < 18))
  expect_length(w, 2)
  expect_match(w[1], "household")
  expect_equal(x$side$n, as.character(c(
    length(unique(pp$household[pp$village == "v1" & pp$age < 18])),
    length(unique(pp$household[pp$village == "v2" & pp$age < 18])))))
  # computed counts stay trusted by flow_check() and are not typed
  expect_equal(nrow(typed_counts(d)), 0)
  expect_equal(nrow(flow_check(d)), 0)
  expect_match(as.character(flow_describe(d)),
               "from the Village row on, of distinct values of `household`",
               fixed = TRUE)
  # no unit set automatically
  expect_null(e$units)
})

test_that("count = errors clearly", {
  pp <- people_data()
  expect_error(flow_start("P", data = pp) |>
                 flow_split("V", by = "village", count = "hh"),
               "`count` column \"hh\" is not in the data frame `pp`")
  expect_error(flow_start("P", n = 3) |> flow_apply("A", "a", count = "x"),
               "needs a flow with data")
})

test_that("unit designs lay out without overlaps", {
  for (d in list(f01(), survey(),
                 flow_start("Cultures", n = 100, unit = "cultures") |>
                   flow_split("Ex", c("Ex. 1", "Ex. 2"), n = 50) |>
                   flow_exclude("Contaminated", n = 2,
                                reasons = c("Mould" = 1, "Bacteria" = 1)) |>
                   flow_split("Plates", c("P1", "P2", "P3"), n = 16,
                              unit = "plates") |>
                   flow_outcome("Read", "Colonies", unit = "readings"))) {
    for (con in c("elbow", "straight")) {
      lay <- flow_layout(d, connector = con)
      expect_equal(boxes_overlap(all_boxes(lay)), 0)
      expect_equal(crossings(lay), 0)
    }
  }
})

test_that("review fixes: joins, copy notes and row-label edits respect units", {
  # branches counting different units are not joined into one box
  df <- data.frame(id = 1:40, arm = rep(c("A", "B"), each = 20))
  d <- flow_start("All", data = df) |>
    flow_split("Arm", by = "arm", unit = c("flasks", "plates")) |>
    flow_outcome("Measured", "Strength")
  expect_length(d$leaves, 2)
  base <- flow_start("A", n = 100) |>
    flow_split("Arm", c("X", "Y"), n = 50, unit = c("flasks", "plates"))
  expect_error(flow_outcome(base, "End", "S", join = TRUE), "different unit")
  same <- flow_start("A", n = 100, unit = "flasks") |>
    flow_split("Arm", c("X", "Y")) |>
    flow_outcome("End", "S")
  expect_length(same$leaves, 1)
  # a copy counted in another unit is not "the same ... as above"
  cp <- flow_start("A", n = 100, unit = "casks") |>
    flow_copy("Raters", c("r1", "r2"), unit = "ratings")
  txt <- flow_layout(cp)$boxes$note
  expect_false(any(grepl("the same", txt, fixed = TRUE)))
  cp2 <- flow_start("A", n = 100, unit = "casks") |>
    flow_copy("Raters", c("r1", "r2"))
  expect_true(any(grepl("the same 100 casks", flow_layout(cp2)$boxes$note,
                        fixed = TRUE)))
  # row-label edits refuse box fields instead of ignoring them
  expect_error(flow_edit(base, stage = "Arm", row_label = "Group", unit = "z"),
               "`unit` need `box`")
})
