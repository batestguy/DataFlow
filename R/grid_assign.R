# Fold grids from the user's own split assignments ----------------------------
#
# `data` has one row per record per split: column `split` says which split,
# column `role` the record's role in it. This documents the split actually
# made, so the group and time checks are real checks of it.

grid_from_assignments <- function(data, dname, split, role, roles, time,
                                  group, settings) {
  for (col in c(split, role, time, group)) {
    check_string(col, "column")
    if (!col %in% names(data)) {
      stop("Column \"", col, "\" is not in `data`.", call. = FALSE)
    }
  }
  sp <- data[[split]]
  rv <- data[[role]]
  if (anyNA(sp) || anyNA(rv)) {
    stop("Columns \"", split, "\" and \"", role, "\" must have no missing ",
         "values.", call. = FALSE)
  }
  assignable <- c("Training", "Validation", "Test", "Gap")
  if (is.null(roles)) {
    roles <- stats::setNames(assignable, assignable)
  } else if (!is.character(roles) || is.null(names(roles)) ||
             !all(names(roles) %in% assignable) || anyNA(roles) ||
             anyDuplicated(roles)) {
    stop("`roles` must be text named by ",
         paste0("\"", assignable, "\"", collapse = ", "),
         ", e.g. c(Training = \"analysis\", Test = \"assessment\").",
         call. = FALSE)
  }
  rc <- as.character(rv)
  mapped <- names(roles)[match(rc, roles)]
  if (anyNA(mapped)) {
    miss <- unique(rc[is.na(mapped)])
    stop("Values of \"", role, "\" not mapped by `roles`: ",
         paste0("\"", miss, "\"", collapse = ", "), ". Map every value to ",
         "Training, Validation, Test or Gap.", call. = FALSE)
  }
  split_ids <- if (is.factor(sp)) levels(sp)[levels(sp) %in% as.character(sp)]
    else unique(as.character(sort(sp)))
  spc <- as.character(sp)

  # checks against the split as made ---------------------------------------
  na_group <- 0
  if (!is.null(group)) {
    gv <- data[[group]]
    na_group <- warn_missing_groups(gv, group)
    leaks <- character()
    for (s in split_ids) {
      in_s <- spc == s & !is.na(gv) & mapped != "Gap"
      per_group <- tapply(mapped[in_s], as.character(gv[in_s]),
                          function(z) length(unique(z)))
      bad <- names(per_group)[per_group > 1]
      if (length(bad) > 0) {
        leaks <- c(leaks, paste0("split ", s, ": ", length(bad), " (e.g. ",
                                 bad[1], ")"))
      }
    }
    if (length(leaks) > 0) {
      stop("Some values of \"", group, "\" have more than one role within ",
           "a split: ", paste(leaks, collapse = "; "), ". A grouped split ",
           "keeps all rows of each ", group, " in one role per split.",
           call. = FALSE)
    }
  }
  times <- NULL
  if (!is.null(time)) {
    tv <- check_time_values(data[[time]], time)
    if (anyNA(tv)) {
      stop("Column \"", time, "\" has missing values; remove them first.",
           call. = FALSE)
    }
    order_roles <- c("Training", "Validation", "Test")
    for (s in split_ids) {
      in_s <- spc == s
      present <- order_roles[order_roles %in% mapped[in_s]]
      for (a in seq_along(present)[-1]) {
        r1 <- present[a - 1]
        r2 <- present[a]
        t1 <- max(tv[in_s & mapped == r1])
        t2 <- min(tv[in_s & mapped == r2])
        if (!(t1 < t2)) {
          stop("In split ", s, ", ", r1, " rows run to ", format_time(t1),
               " but ", r2, " rows start on ", format_time(t2), ". Each ",
               "role must end before the next one starts.", call. = FALSE)
        }
      }
    }
    times <- sort(unique(tv))
  }

  # blocks --------------------------------------------------------------------
  out <- list()
  rows <- list()
  add <- function(s, r, a, b, idx) {
    out[[length(out) + 1]] <<- data.frame(split = match(s, split_ids),
                                          role = r, start = a, end = b,
                                          stringsAsFactors = FALSE)
    rows[[length(rows) + 1]] <<- idx
  }
  if (!is.null(time)) {
    pos <- match(tv, times)
    P <- length(times)
    for (s in split_ids) {
      in_s <- which(spc == s)
      at <- rep(NA_character_, P)
      for (p in unique(pos[in_s])) {
        r <- unique(mapped[in_s][pos[in_s] == p])
        if (length(r) > 1) {
          stop("In split ", s, ", the time value ", format_time(times[p]),
               " has rows with roles ", and_list(r), "; on a time axis each ",
               "time value needs one role per split.", call. = FALSE)
        }
        at[p] <- r
      }
      at[is.na(at)] <- "Not used"
      # unused time between training and test is the gap
      tr_end <- suppressWarnings(max(which(at == "Training")))
      te_start <- suppressWarnings(min(which(at %in% c("Validation", "Test"))))
      if (is.finite(tr_end) && is.finite(te_start) && te_start > tr_end + 1) {
        mid <- (tr_end + 1):(te_start - 1)
        at[mid][at[mid] == "Not used"] <- "Gap"
      }
      runs <- rle(at)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1
      for (j in seq_along(runs$values)) {
        idx <- in_s[pos[in_s] >= starts[j] & pos[in_s] <= ends[j]]
        add(s, runs$values[j], starts[j], ends[j], idx)
      }
    }
    axis <- "time"
  } else {
    P <- 0
    for (s in split_ids) {
      in_s <- which(spc == s)
      start <- 1
      for (r in assignable) {
        idx <- in_s[mapped[in_s] == r]
        if (length(idx) == 0) next
        add(s, r, start, start + length(idx) - 1, idx)
        start <- start + length(idx)
      }
      P <- max(P, start - 1)
    }
    axis <- "rows"
  }
  b <- do.call(rbind, out)
  blocks <- data.frame(id = seq_len(nrow(b)), b,
                       n = format_n(lengths(rows)), label = NA_character_,
                       stringsAsFactors = FALSE)
  attr(blocks, "rows") <- rows
  settings$group_checked <- !is.null(group)
  settings$time_checked <- !is.null(time)
  settings$na_group <- na_group
  settings$split_col <- split
  settings$role_col <- role
  g <- new_grid("assigned", axis, blocks, settings, as.integer(P),
                times = times, data = list(name = dname, df = data))
  g$splits$label <- paste("Split", split_ids)
  g
}
