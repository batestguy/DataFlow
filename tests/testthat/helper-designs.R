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

# Segments that pass through the inside of a box other than at their own
# start or end boxes.
crossings <- function(lay) {
  b <- lay$boxes
  sg <- lay$segments
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
