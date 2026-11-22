# Irregular designs that once produced wrong pictures (from review of
# milestone 1). Each should lay out with no box overlaps and no line passing
# through a box it does not belong to.
irregular_designs <- function() {
  list(
    skip_arrow = flow_start("All") |>
      flow_split("Arm", c("Ctl", "Trt")) |>
      flow_split("Sub", c("alpha", "beta", "gamma", "delta", "epsilon"),
                 from = "Trt") |>
      flow_split("Sub2", "z", from = "Ctl"),
    uneven_bracket = flow_start("All") |>
      flow_split("Arm", c("A1", "A2")) |>
      flow_split("Site", c("s1", "s2")) |>
      flow_split("Pt", c("p1", "p2", "p3"), from = "s1") |>
      flow_apply("Cond", "merged label", over = "Arm"),
    uneven_branches = flow_start("Patients", n = 412) |>
      flow_split("Site", c("Hospital A", "Hospital B", "Clinic C")) |>
      flow_split("Ward", list(c("W1", "W2", "W3"), "General",
                              c("Day", "Night"))) |>
      flow_apply("Protocol", "Standard care", over = "Site") |>
      flow_split("Follow-up", c("6 months", "12 months"),
                 from = "Standard care") |>
      flow_outcome("Outcome", "Readmission")
  )
}

# Segments that pass through the inside of a box (flow boxes and exclusion
# boxes alike) other than at their own start or end boxes.
crossings <- function(lay) {
  b <- all_boxes(lay)
  sg <- lay$segments[, c("x", "y", "xend", "yend")]
  # bracket paths, as segments between consecutive points
  for (g in split(lay$brackets, lay$brackets$group)) {
    k <- nrow(g)
    sg <- rbind(sg, data.frame(x = g$x[-k], y = g$y[-k],
                               xend = g$x[-1], yend = g$y[-1]))
  }
  eps <- 1e-6
  hits <- 0
  for (i in seq_len(nrow(sg))) {
    s <- sg[i, ]
    xl <- min(s$x, s$xend); xr <- max(s$x, s$xend)
    yl <- min(s$y, s$yend); yh <- max(s$y, s$yend)
    inside <- b$xmin + eps < xr & xl < b$xmax - eps &
      b$ymin + eps < yh & yl < b$ymax - eps
    hits <- hits + sum(inside)
  }
  hits
}

# Number of pairs of rectangles that overlap.
boxes_overlap <- function(b) {
  hits <- 0
  for (i in seq_len(nrow(b) - 1)) for (j in (i + 1):nrow(b)) {
    if (b$xmin[i] < b$xmax[j] && b$xmin[j] < b$xmax[i] &&
        b$ymin[i] < b$ymax[j] && b$ymin[j] < b$ymax[i]) hits <- hits + 1
  }
  hits
}

# Flow boxes and exclusion boxes as one set of rectangles.
all_boxes <- function(lay) {
  cols <- c("xmin", "xmax", "ymin", "ymax")
  rbind(lay$boxes[, cols], if (!is.null(lay$side)) lay$side[, cols])
}

# Trial-shaped designs with exclusion boxes in awkward places.
trial_designs <- function() {
  list(
    consort = flow_example("trial"),
    one_arm_only = flow_start("Screened", n = 200) |>
      flow_assign("Arm", c("Drug", "Placebo"), n = 100) |>
      flow_exclude("Withdrew", n = 7, reasons = "Adverse event",
                   from = "Drug") |>
      flow_outcome("Analysis", "Analysed"),
    split_below_exclusion = flow_start("Assessed", n = 500) |>
      flow_exclude("Not eligible", n = 100) |>
      flow_assign("Arm", c("A", "B", "C")) |>
      flow_outcome("End", "Analysed"),
    stacked_exclusions = flow_start("Invited", n = 1000) |>
      flow_exclude("No reply", n = 400) |>
      flow_exclude("Declined", n = 150,
                   reasons = c("No time" = 90, "Not interested" = 60)) |>
      flow_split("Consented", "Consented", n = 450),
    exclusion_at_end = flow_start("Randomised", n = 60) |>
      flow_assign("Arm", c("A", "B"), n = 30) |>
      flow_exclude("Excluded from analysis", n = c(2, 1)),
    exclusion_then_bracket = flow_start("Sites") |>
      flow_split("Site", c("North", "South")) |>
      flow_split("Ward", c("W1", "W2")) |>
      flow_exclude("Closed", n = 1) |>
      flow_apply("Pooled", "All wards", over = "Site"),
    wide_side_box = flow_start("Assessed", n = 412) |>
      flow_assign("Arm", c("A", "B", "C", "D"), n = 100) |>
      flow_exclude("Lost to follow-up", n = 5,
                   reasons = c("A long reason that makes the box wide" = 5)) |>
      flow_outcome("Analysis", "Analysed"),
    # found by random testing: the line down to an exclusion box two rows
    # below its parent ran through a neighbour's exclusion box
    stub_through_neighbour = flow_start("All") |>
      flow_split("Pair", c("Long label here", "Two\nlines")) |>
      flow_exclude("Lost to follow-up", n = 3, from = "Long label here") |>
      flow_exclude("Excluded", n = 3, from = "Two\nlines"),
    # found by random testing: children that skip a full row had their lines
    # drawn through their parent's own exclusion box
    skip_past_own_exclusion = flow_start("Top") |>
      flow_split("Side", c("Left", "Right")) |>
      flow_split("Sub", c("a", "b"), from = "Left") |>
      flow_exclude("Lost to follow-up", n = 3,
                   reasons = c("Reason one" = 1,
                               "Another longer reason" = 2)) |>
      flow_split("Next", c("Ccc", "A"))
  )
}
