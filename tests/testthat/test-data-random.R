# Random data-mode check (from the milestone 4 review): whatever steps are
# taken, the rows and counts stay consistent.
#  * the boxes made by a `by =` split hold exactly the parent's remaining
#    rows, and their counts add up to it;
#  * rows of a box = rows still in the flow + rows of its exclusion boxes;
#  * every count drawn for a box with rows equals its number of rows.

random_data <- function(N, factor_g = TRUE) {
  g <- sample(c("lo", "mid", "hi"), N, TRUE)
  data.frame(
    a = sample(c("u", "v", "w"), N, TRUE),
    g = if (factor_g) factor(g, levels = c("lo", "mid", "hi", "none")) else g,
    k = sample(1:4, N, TRUE),
    x = round(stats::runif(N, 0, 10), sample(0:3, 1)),
    stringsAsFactors = FALSE
  )
}

random_steps <- function(d, steps) {
  for (step in seq_len(steps)) {
    op <- sample(c("split", "excl", "reasons", "sel", "apply"), 1)
    col <- sample(c("a", "g", "k", "x"), 1)
    cond <- sample(c("x < 3", "k > 2", "a == 'u'", "x >= 5 & k == 1"), 1)
    f <- stats::as.formula(paste("~", cond))
    d2 <- tryCatch(switch(op,
      split = flow_split(d, paste0("S", step), by = col),
      excl = flow_exclude(d, paste0("E", step), where = f),
      reasons = flow_exclude(d, paste0("R", step),
                             reasons = list("first" = f, "second" = ~ k == 1)),
      sel = flow_select(d, paste0("T", step), "sel", where = f),
      apply = flow_apply(d, paste0("A", step), "next")
    ), error = function(e) NULL)
    if (!is.null(d2)) d <- d2
  }
  d
}

check_invariants <- function(d) {
  nd <- d$nodes
  rows <- d$data$rows
  len <- function(r) length(r)
  for (p in unique(d$edges$from)) {
    kids <- d$edges$to[d$edges$from == p]
    st <- unique(nd$stage[nd$id %in% kids])
    if (length(st) != 1 || d$stages$type[d$stages$id == st] != "split" ||
        d$stages$display[d$stages$id == st] != "all") next
    left <- rows$left[[as.character(p)]]
    kid_rows <- unlist(lapply(kids, function(k) rows$box[[as.character(k)]]))
    expect_setequal(kid_rows, left)
    expect_equal(length(kid_rows), length(left))
    expect_equal(sum(as.numeric(nd$n[nd$id %in% kids])), length(left))
  }
  for (p in nd$id) {
    key <- as.character(p)
    if (is.null(rows$left[[key]])) next
    sides <- d$side$id[d$side$parent == p]
    gone <- unlist(lapply(sides, function(s) rows$side[[as.character(s)]]))
    expect_equal(sort(c(rows$left[[key]], gone)), sort(rows$box[[key]]))
    expect_equal(as.numeric(nd$n[nd$id == p]), len(rows$box[[key]]))
  }
  for (s in d$side$id) {
    r <- rows$side[[as.character(s)]]
    if (!is.null(r)) {
      expect_equal(as.numeric(d$side$n[d$side$id == s]), len(r))
    }
  }
}

test_that("random data flows keep rows and counts consistent", {
  set.seed(20261124)
  for (it in 1:40) {
    d <- flow_start("All", data = random_data(sample(30:90, 1)))
    d <- random_steps(d, 4)
    check_invariants(d)
  }
})

test_that("random flows from two data frames (factor and text) stay consistent", {
  set.seed(20261125)
  for (it in 1:20) {
    d <- flow_start(c("One", "Two"),
                    data = list(random_data(sample(20:50, 1), TRUE),
                                random_data(sample(20:50, 1), FALSE))) |>
      flow_pool("P", "Pooled")
    d <- random_steps(d, 3)
    check_invariants(d)
  }
})
