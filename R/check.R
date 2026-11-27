# Number check: reports, never changes the flow --------------------------------

#' Check whether the numbers in a flow add up
#'
#' Compares the counts of neighbouring boxes and reports what does not fit.
#' Nothing is changed and drawing is never blocked: counts are drawn as typed
#' whether or not you run this.
#'
#' A **problem** is a pair of numbers that cannot both be right if every box
#' counts the same kind of unit:
#'
#' * the boxes of a split, assignment or data split do not add up to what is
#'   left in the box above (its count minus its exclusion boxes);
#' * the reasons in an exclusion box do not add up to its count, or more are
#'   excluded than there were;
#' * after an exclusion, the next box is not what is left, or a pool or a
#'   selection has more than what is left;
#' * a pool has fewer than its sources together;
#' * a copy made by [flow_copy()] has more than what is left in the box it
#'   copies, or a pool of copies has more units than were copied (each unit
#'   is counted once) or fewer than one of the copies;
#' * there are more outcome events than units, or the events of a split do not
#'   add up.
#'
#' A **note** is something that may be right and is worth a look: every branch
#' of a split has the same count as the box above (fine if each branch gets
#' all of them, as when two people label the same records), a later box counts
#' more than the one above (fine if the unit changes, as from households to
#' people), or fewer with no exclusion drawn, including a copy with fewer than
#' the box it copies.
#'
#' An outcome drawn once for several branches ([flow_outcome()]) is a
#' measurement, not a pool: its count may be of all the branches together or
#' that of each branch, and any other count gives a note, never a problem.
#'
#' Sampling steps ([flow_cluster()], [flow_select()]) and summary boxes are not
#' compared, except that a selection larger than its source is reported. Missing
#' counts and counts that are not plain numbers cannot be compared; the number
#' of comparisons that could not be made is printed. Counts computed from data
#' are taken as right; a comparison is made when at least one count in it was
#' typed or edited. Fold grids are not checked.
#'
#' A clean result means only that the numbers fit each other. It does not mean
#' that they are the right numbers.
#'
#' @param x A `studydesignflow` object.
#' @return A data frame (class `studydesignflow_check`) with one row per
#'   finding: `level` (`"problem"` or `"note"`), `stage`, `box` (the box the
#'   numbers belong to) and `message`. Notes of the same sort in one row of
#'   the diagram are given once. Printing it gives a short report; a selection
#'   of its rows or columns is a plain data frame.
#' @export
#' @examples
#' flow_start("Assessed", n = 500) |>
#'   flow_exclude("Excluded", n = 100,
#'                reasons = c("Not eligible" = 60, "Declined" = 30)) |>
#'   flow_split("Randomised", "Randomised", n = 400) |>
#'   flow_assign("Allocation", c("Drug", "Placebo"), n = c(200, 190)) |>
#'   flow_check()
flow_check <- function(x) {
  if (inherits(x, "studydesignflow_grid")) {
    stop("`flow_check()` checks flow diagrams; fold grids are not checked.",
         call. = FALSE)
  }
  validate_studydesignflow(x)
  st <- x$stages
  nd <- x$nodes
  ed <- x$edges
  sd <- x$side %||% empty_side()
  rs <- x$reasons %||% empty_reasons()
  flat <- function(s) gsub("[[:space:]]*\n[[:space:]]*", " ", s)
  nice <- unique_names(x, flat)
  quoted <- function(id) paste0("\"", nice[as.character(id)], "\"")
  n_of <- stats::setNames(parse_count(nd$n), nd$id)
  side_n <- stats::setNames(parse_count(sd$n), sd$id)
  ev_of <- stats::setNames(rep(NA_real_, nrow(nd)), nd$id)
  if (!is.null(x$events)) ev_of[names(x$events)] <- parse_count(x$events)
  N <- function(id) unname(n_of[as.character(id)])
  E <- function(id) unname(ev_of[as.character(id)])
  row_of <- function(stage) match(stage, st$id)

  found <- list()
  made <- 0L
  unknown <- 0L
  # `kind` groups notes of one sort within a stage; problems are never grouped
  report <- function(level, stage, box, ..., kind = NA_character_) {
    found[[length(found) + 1]] <<- data.frame(
      level = level, stage = stage, box = box, message = paste0(...),
      kind = kind, stringsAsFactors = FALSE
    )
  }
  # counts computed from data and not edited since: taken as right
  computed_box <- function(id) {
    if (!has_data(x) || id %in% edited_ids(x, "nodes")) return(FALSE)
    r <- box_rows(x, id)
    !is.null(r) && identical(nd$n[nd$id == id], format_n(count_rows(x, r)))
  }
  computed_side <- function(id) {
    if (!has_data(x) || id %in% edited_ids(x, "side")) return(FALSE)
    r <- x$data$rows$side[[as.character(id)]]
    !is.null(r) && identical(sd$n[sd$id == id], format_n(count_rows(x, r)))
  }
  # outcome events that were typed or edited, not computed from data
  typed_event <- function(ids) {
    if (!has_data(x)) return(TRUE)
    any(ids %in% c(x$events_typed, edited_ids(x, "events"))) ||
      !all(vapply(ids, computed_event, NA))
  }
  computed_event <- function(id) {
    r <- box_rows(x, id)
    !is.null(r) && !is.null(x$data$events) &&
      identical(unname(x$events[as.character(id)]),
                format_n(count_events(x, r)[["events"]]))
  }
  all_computed <- function(boxes, sides = integer()) {
    has_data(x) && all(vapply(boxes, computed_box, NA)) &&
      all(vapply(sides, computed_side, NA))
  }
  # what is left in box `id` for a stage on row `r`: its count minus the
  # exclusion boxes that hang off it above that row
  sides_of <- function(id, r) sd$id[sd$parent == id & row_of(sd$stage) < r]
  left_in <- function(id, r) {
    N(id) - sum(side_n[as.character(sides_of(id, r))])
  }
  left_text <- function(id, r) {
    s <- unlist(lapply(id, sides_of, r = r))
    if (length(s) == 0) return(show_count(sum(N(id))))
    paste0(show_count(sum(vapply(id, left_in, 1, r = r))), " (",
           show_count(sum(N(id))), " minus ",
           show_count(sum(side_n[as.character(s)])), " excluded)")
  }

  # exclusion boxes: reasons, and more excluded than there were
  for (i in seq_len(nrow(sd))) {
    id <- sd$id[i]
    stage <- st$name[st$id == sd$stage[i]]
    lab <- flat(sd$label[i])
    r <- rs[rs$side == id, ]
    if (nrow(r) > 0 && !(computed_side(id) &&
                         id %in% x$data$computed_reasons)) {
      rn <- parse_count(r$n)
      total <- sum(rn, na.rm = TRUE)
      if (anyNA(rn) || is.na(side_n[[as.character(id)]])) {
        if (!is.na(side_n[[as.character(id)]]) &&
            total > side_n[[as.character(id)]]) {
          made <- made + 1L
          report("problem", stage, lab, "The reasons in \"", lab, "\" under ",
                 quoted(sd$parent[i]), " already add up to ",
                 show_count(total), ", more than its ",
                 show_count(side_n[[as.character(id)]]), ".")
        } else {
          unknown <- unknown + 1L
        }
      } else {
        made <- made + 1L
        if (differs(total, side_n[[as.character(id)]])) {
          report("problem", stage, lab, "The reasons in \"", lab, "\" under ",
                 quoted(sd$parent[i]), " add up to ", show_count(total),
                 ", not ", show_count(side_n[[as.character(id)]]), ".",
                 if (total > side_n[[as.character(id)]]) {
                   paste0(" (If some units have more than one reason, the ",
                          "labels should say so.)")
                 })
        }
      }
    }
    r_next <- row_of(sd$stage[i]) + 1
    if (!all_computed(sd$parent[i], sides_of(sd$parent[i], r_next))) {
      lft <- left_in(sd$parent[i], r_next)
      if (is.na(lft)) {
        unknown <- unknown + 1L
      } else {
        made <- made + 1L
        if (lft < -1e-8) {
          report("problem", stage, lab, "More are excluded than there are in ",
                 quoted(sd$parent[i]), " (", show_count(N(sd$parent[i])),
                 ").")
        }
      }
    }
  }

  # outcome events within one box
  for (id in nd$id) {
    if (is.na(E(id)) || is.na(N(id))) next
    if (computed_box(id) && !typed_event(id)) next
    made <- made + 1L
    if (E(id) > N(id)) {
      report("problem", st$name[st$id == nd$stage[nd$id == id]],
             nice[[as.character(id)]], quoted(id), " has ",
             show_count(E(id)), " events but only ", show_count(N(id)),
             " units.")
    }
  }

  for (s in seq_len(nrow(st))[-1]) {
    type <- st$type[s]
    stage <- st$name[s]
    ids <- nd$id[nd$stage == st$id[s]]
    if (type == "exclude" || type == "cluster" || st$display[s] == "summary") {
      next
    }
    srcs <- lapply(ids, function(i) ed$from[ed$to == i])

    if (type == "copy") {
      for (i in seq_along(ids)) {
        id <- ids[i]
        p <- srcs[[i]]
        sides <- sides_of(p, s)
        if (all_computed(c(p, id), sides)) next
        lft <- left_in(p, s)
        if (is.na(lft) || is.na(N(id))) {
          unknown <- unknown + 1L
          next
        }
        made <- made + 1L
        if (differs(N(id), lft) && N(id) > lft) {
          report("problem", stage, nice[[as.character(id)]], quoted(id),
                 " has ", show_count(N(id)), ", more than the ",
                 left_text(p, s), " it is a copy of.")
        } else if (differs(N(id), lft)) {
          report("note", stage, nice[[as.character(id)]], quoted(id),
                 " has ", show_count(N(id)), ", fewer than the ",
                 left_text(p, s), " it is a copy of. Fine if not every unit ",
                 "reached it; the label should say so.", kind = "copy_fewer")
        }
      }
      next
    }

    if (type %in% c("split", "assign", "partition")) {
      for (p in unique(unlist(srcs))) {
        kids <- ids[vapply(srcs, function(z) p %in% z, NA)]
        sides <- sides_of(p, s)
        lft <- left_in(p, s)
        kn <- N(kids)
        copies <- length(kids) > 1 && !anyNA(c(kn, lft)) &&
          !any(differs(kn, lft))
        trusted <- all_computed(c(p, kids), sides)
        if (trusted) {
          # counts computed from data: nothing to compare
        } else if (is.na(lft) || all(is.na(kn))) {
          unknown <- unknown + 1L
        } else if (anyNA(kn)) {
          if (sum(kn, na.rm = TRUE) > lft) {
            made <- made + 1L
            report("problem", stage, nice[[as.character(p)]], "The boxes under ",
                   quoted(p), " that have a count already add up to ",
                   show_count(sum(kn, na.rm = TRUE)), ", more than the ",
                   left_text(p, s), " there.")
          } else {
            unknown <- unknown + 1L
          }
        } else {
          made <- made + 1L
          if (copies && lft > 0) {
            report("note", stage, nice[[as.character(p)]], "Each of the ",
                   length(kids), " boxes under ", quoted(p),
                   " has the same count as what is there (",
                   show_count(lft), "). Fine if every branch gets all of ",
                   "them; flow_copy() draws that.", kind = "copies")
          } else if (length(kids) == 1 && kn > N(p)) {
            report("note", stage, nice[[as.character(kids)]], quoted(kids),
                   " has ", show_count(kn), ", more than the ",
                   show_count(lft), " above it. Fine if the unit changes; ",
                   "the label should say so.", kind = "more")
          } else if (length(kids) == 1 && length(sides) == 0 && kn < lft) {
            report("note", stage, nice[[as.character(kids)]], quoted(kids),
                   " has ", show_count(kn), ", fewer than the ",
                   show_count(lft), " above it, and no exclusion is drawn.",
                   kind = "fewer")
          } else if (differs(sum(kn), lft)) {
            report("problem", stage, nice[[as.character(p)]],
                   if (length(kids) == 1) {
                     paste0(quoted(kids), " has ", show_count(kn))
                   } else {
                     paste0("The ", length(kids), " boxes under ", quoted(p),
                            " add up to ", show_count(sum(kn)))
                   },
                   ", not ", left_text(p, s), ".",
                   if (length(kids) > 1 && sum(kn) > N(p)) {
                     paste0(" (If some units are in more than one box, or ",
                            "the boxes count another kind of unit, the labels ",
                            "should say so.)")
                   })
          }
        }
        # events of a split, when nothing was excluded in between
        ke <- E(kids)
        if (!copies && length(sides) == 0 && !is.na(E(p)) && !anyNA(ke) &&
            typed_event(c(p, kids))) {
          made <- made + 1L
          if (differs(sum(ke), E(p))) {
            report("problem", stage, nice[[as.character(p)]], "The events under ",
                   quoted(p), " add up to ", show_count(sum(ke)), ", not ",
                   show_count(E(p)), ".",
                   if (sum(ke) > E(p)) {
                     paste0(" (If some units are in more than one box, the ",
                            "labels should say so.)")
                   })
          }
        }
      }
      next
    }

    for (i in seq_along(ids)) {
      id <- ids[i]
      src <- srcs[[i]]
      box <- nice[[as.character(id)]]
      sides <- unlist(lapply(src, sides_of, r = s))
      if (all_computed(c(id, src), sides)) next
      lft <- vapply(src, left_in, 1, r = s)
      total <- sum(lft)
      if (is.na(N(id)) || is.na(total)) {
        unknown <- unknown + 1L
        next
      }
      if (any(ed$kind[ed$to == id] == "measure")) {
        # an outcome measured in every branch is not a pool: its count may
        # be of all the branches together or of each branch
        made <- made + 1L
        if (!differs(N(id), total) || !any(differs(lft, N(id)))) next
        if (N(id) > total) {
          report("note", stage, box, quoted(id), " has ", show_count(N(id)),
                 ", more than the ", show_count(total), " in the branches it ",
                 "is measured in. Fine if the unit changes; the label should ",
                 "say so.", kind = "more")
        } else {
          report("note", stage, box, quoted(id), " has ", show_count(N(id)),
                 ", neither the ", show_count(total), " in the branches it is ",
                 "measured in together nor the count of each branch. Fine if ",
                 "not every unit was measured; the label should say so.",
                 kind = "measure")
        }
        next
      }
      cp <- if (type == "pool") pool_units(x, src, s) else NULL
      if (!is.null(cp)) {
        # some sources are copies of the same units: each unit once
        if (anyNA(c(cp$upper, cp$lower))) {
          unknown <- unknown + 1L
          next
        }
        made <- made + 1L
        if (N(id) > cp$upper + 1e-8) {
          report("note", stage, box, quoted(id), " has ", show_count(N(id)),
                 ", more than the ", show_count(cp$upper), " different units ",
                 "in the boxes joined in it, as some are copies of the same ",
                 "units. Fine if the unit changes (for example to ",
                 "person-periods); the label should say so.",
                 kind = "pool_more")
        } else if (N(id) < cp$lower - 1e-8) {
          report("problem", stage, box, quoted(id), " has ",
                 show_count(N(id)), ", but the boxes joined in it hold at ",
                 "least ", show_count(cp$lower), " different units.")
        }
        next
      }
      made <- made + 1L
      if (!differs(N(id), total)) next
      if (type == "pool") {
        if (!any(differs(lft, N(id)))) {
          report("note", stage, box, quoted(id), " has the same count (",
                 show_count(N(id)), ") as each of the boxes joined in it. ",
                 "Fine if they are the same units; a pool of different ones ",
                 "would have ", show_count(total), ".", kind = "pool_same")
        } else if (N(id) < total) {
          report("problem", stage, box, quoted(id), " has ",
                 show_count(N(id)), ", but the boxes joined in it add up to ",
                 left_text(src, s), ".")
        } else if (length(sides) > 0 && N(id) <= sum(N(src))) {
          report("problem", stage, box, quoted(id), " has ",
                 show_count(N(id)), ", more than the ", left_text(src, s),
                 " joined in it.")
        } else {
          report("note", stage, box, quoted(id), " has ", show_count(N(id)),
                 ", more than the ", show_count(total), " joined in it. Fine ",
                 "if the unit changes; the label should say so.",
                 kind = "pool_more")
        }
      } else if (N(id) > total &&
                 (length(sides) == 0 || N(id) > sum(N(src)))) {
        report("note", stage, box, quoted(id), " has ", show_count(N(id)),
               ", more than the ", show_count(total), " above it. Fine if the ",
               "unit changes; the label should say so.", kind = "more")
      } else if (type == "select") {
        # a selection smaller than its source is what a selection does
        if (N(id) > total) {
          report("problem", stage, box, quoted(id), " has ",
                 show_count(N(id)), ", more than the ", left_text(src, s),
                 " above it.")
        }
      } else if (length(sides) > 0) {
        report("problem", stage, box, quoted(id), " has ", show_count(N(id)),
               ", not ", left_text(src, s), ".")
      } else {
        report("note", stage, box, quoted(id), " has ", show_count(N(id)),
               ", fewer than the ", show_count(total), " above it, and no ",
               "exclusion is drawn.", kind = "fewer")
      }
    }
  }

  out <- if (length(found)) do.call(rbind, found) else
    data.frame(level = character(), stage = character(), box = character(),
               message = character(), kind = character(),
               stringsAsFactors = FALSE)
  out <- group_notes(out)
  out <- out[order(out$level != "problem"),
             c("level", "stage", "box", "message"), drop = FALSE]
  rownames(out) <- NULL
  attr(out, "compared") <- made
  attr(out, "not_compared") <- unknown
  attr(out, "found") <- nrow(out)
  class(out) <- c("studydesignflow_check", "data.frame")
  out
}

# For a pool of `src` on row `r`: the most and the fewest different units it
# can hold, when some sources are copies (flow_copy()) of the same units;
# NULL when no copies are involved. Sources are grouped by the box they come
# from under their lowest common ancestor: copies of it hold at most what was
# left in it and at least the largest group; shares (splits) add up.
pool_units <- function(x, src, r) {
  if (length(src) < 2) return(NULL)
  st <- x$stages
  nd <- x$nodes
  sd <- x$side %||% empty_side()
  copy_stages <- st$id[st$type == "copy"]
  left_at <- function(id, row) {
    out <- sd$n[sd$parent == id & match(sd$stage, st$id) < row]
    v <- parse_count(c(nd$n[nd$id == id], out))
    if (anyNA(v)) NA_real_ else v[1] - sum(v[-1])
  }
  used <- FALSE
  bounds <- function(ids) {
    if (length(ids) == 1) return(rep(left_at(ids, r), 2))
    top <- lowest_common_ancestor(x, ids)
    child <- vapply(ids, function(i) {
      chain <- c(i, node_ancestors(x, i))
      k <- if (is.na(top)) length(chain) else match(top, chain) - 1L
      if (is.na(k) || k < 1) i else chain[k]
    }, integer(1))
    kids <- unique(child)
    if (length(kids) == 1) {
      return(rowSums(vapply(ids, function(i) rep(left_at(i, r), 2),
                            numeric(2))))
    }
    b <- vapply(split(ids, factor(child, levels = kids)), bounds, numeric(2))
    is_copy <- nd$stage[match(kids, nd$id)] %in% copy_stages
    if (!is.na(top) && any(is_copy)) {
      used <<- TRUE
      row <- match(nd$stage[match(kids[is_copy][1], nd$id)], st$id)
      c(min(left_at(top, row), sum(b[1, ])), max(b[2, ]))
    } else {
      rowSums(b)
    }
  }
  out <- bounds(src)
  if (!used) return(NULL)
  list(upper = out[1], lower = out[2])
}

# Notes of one kind in one stage become one row: the first, and how many more.
group_notes <- function(out) {
  key <- ifelse(is.na(out$kind), paste0("row", seq_len(nrow(out))),
                paste(out$stage, out$kind, sep = "\r"))
  first <- !duplicated(key)
  k <- as.integer(table(factor(key, levels = unique(key))))
  out <- out[first, , drop = FALSE]
  many <- k > 1
  out$message[many] <- paste0(
    out$message[many], " (The same in ", k[many] - 1, " more place",
    ifelse(k[many] > 2, "s", ""), " in this row.)"
  )
  out
}

# Plain numbers in counts ("1,250"); NA for anything else ("about 40").
parse_count <- function(n) {
  if (length(n) == 0) return(numeric())
  s <- gsub("[, ]", "", unname(n))
  ok <- !is.na(s) & grepl("^[0-9]+([.][0-9]+)?$", s)
  out <- rep(NA_real_, length(s))
  out[ok] <- as.numeric(s[ok])
  out
}

show_count <- function(v) unname(format_n(v))

differs <- function(a, b) abs(a - b) > 1e-8

# Some of the findings are not the report: they print as a plain data frame, so
# that "The numbers fit each other" is never said of a part of them.
#' @export
`[.studydesignflow_check` <- function(x, ...) {
  class(x) <- "data.frame"
  for (a in c("compared", "not_compared", "found")) attr(x, a) <- NULL
  x[...]
}

#' @export
print.studydesignflow_check <- function(x, ...) {
  made <- attr(x, "compared")
  unknown <- attr(x, "not_compared")
  count <- function(k, what) paste0(k, " ", what, if (k != 1) "s")
  bullets <- function(rows) {
    for (i in rows) {
      cat(strwrap(paste0(x$stage[i], ": ", x$message[i]), prefix = "  ",
                  initial = "- ", width = getOption("width", 80)), sep = "\n")
    }
  }
  if (is.null(made) || !identical(attr(x, "found"), nrow(x)) ||
      !all(c("level", "stage", "message") %in% names(x))) {
    return(invisible(NextMethod()))
  }
  p <- which(x$level == "problem")
  k <- which(x$level == "note")
  cat("Number check: ", count(length(p), "problem"), ", ",
      count(length(k), "note"), " (", count(made, "comparison"), " made",
      if (unknown > 0) paste0(", ", unknown, " not possible: a count is ",
                              "missing or is not a plain number"),
      ")\n", sep = "")
  if (length(p)) {
    cat("\nProblems\n")
    bullets(p)
  }
  if (length(k)) {
    cat("\nNotes\n")
    bullets(k)
  }
  if (made > 0 && length(p) == 0) {
    cat("\nThe numbers fit each other. That does not show they are right.\n")
  }
  invisible(x)
}
