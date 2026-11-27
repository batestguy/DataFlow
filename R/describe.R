# Plain-language description (alt text) -----------------------------------

#' Describe a flow diagram in plain sentences
#'
#' Writes a short text description of the design, stage by stage. Use it as
#' alternative text for the figure, or in a methods section. [autoplot()]
#' attaches it to the plot as alt text automatically.
#'
#' Boxes that share a label in the same row are told apart by the box they
#' sit under, e.g. "Batch 1 under Type A". Several top boxes are described
#' as "2 boxes side by side"; pools as "X and Y are combined into Z"; an
#' outcome drawn once for several branches (see [flow_outcome()]) as "Z is
#' measured in every branch, drawn as one box joined from ...". Step details
#' (see [step_details()]) follow the sentence of their step, e.g. "Done by
#' trained enumerators, with tablet questionnaire (CSPro), March to May 2025,
#' in Kaduna State; how: two visits per household.", and "For Village B:
#' done with paper forms." for a box with its own. For a
#' flow with data, a last sentence names the data frame(s) the counts were
#' computed from.
#'
#' @param x A `studydesignflow` object, or a fold grid from [flow_folds()].
#' @return A single string (class `studydesignflow_description`). Printing it wraps
#'   the text to the console width.
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_describe()
flow_describe <- function(x) {
  if (inherits(x, "studydesignflow_grid")) return(describe_grid(x))
  validate_studydesignflow(x)
  st <- x$stages
  nd <- x$nodes
  ed <- x$edges
  flat <- function(s) gsub("[[:space:]]*\n[[:space:]]*", " ", s)
  nice <- unique_names(x, flat)
  units <- box_units(x)
  # each = TRUE: the same text stands for several boxes, so "n = 36 each"
  # ("36 cultures each" with a unit)
  say <- function(id, each = FALSE) {
    i <- match(id, nd$id)
    out <- flat(nd$label[i])
    ev <- if (is.null(x$events)) NA else x$events[as.character(id)]
    extra <- c(if (!is.na(nd$n[i])) {
                 paste0(count_words(nd$n[i], units[[as.character(id)]]),
                        if (each) " each")
               },
               if (!is.na(ev)) paste0("events = ", ev, if (each) " each"),
               if (!is.na(nd$note[i])) flat_note(nd$note[i]))
    if (length(extra)) paste0(out, " (", paste(extra, collapse = "; "), ")")
    else out
  }
  name <- function(id) nice[as.character(id)]
  # the first row by its drawn name, or "top" when it has none
  top_row <- if (st$row_label[1] == "") "top" else st$name[1]
  # TRUE if `ids` are exactly the boxes of one stage; returns that stage name
  whole_row <- function(ids) {
    s <- unique(nd$stage[match(ids, nd$id)])
    if (length(s) == 1 && setequal(ids, nd$id[nd$stage == s])) {
      if (s == st$id[1]) top_row else st$name[st$id == s]
    } else {
      NA_character_
    }
  }

  root <- nd$id[nd$stage == st$id[1]]
  start <- if (length(root) == 1) say(root) else
    paste0(length(root), " boxes side by side",
           if (top_row != "top") paste0(" in the ", top_row, " row"), ": ",
           and_list(vapply(root, say, "")))
  sentences <- paste0("Flow diagram with ", nrow(st), " stage",
                      if (nrow(st) != 1) "s", ", read from top to bottom. ",
                      "It starts with ", start, ".")
  # who, with what, when, where and how (R/details.R), after each step
  sentences <- c(sentences, describe_details(x, st$id[1], name, flat))
  for (s in seq_len(nrow(st))[-1]) {
    ids <- nd$id[nd$stage == st$id[s]]
    type <- st$type[s]
    srcs <- lapply(ids, function(i) ed$from[ed$to == i])
    if (type == "exclude") {
      body <- describe_exclude(x, st$id[s], name, flat, units)
      sentences <- c(sentences, paste0(
        if (st$row_label[s] != "") paste0(st$name[s], ": "), body, "."),
        describe_details(x, st$id[s], name, flat))
      next
    }
    head <- st$name[s]
    if (type %in% c("split", "assign")) {
      verb <- if (type == "split") "split into" else
        assign_verb(st$method[s])
      # units are assigned, not the box they come from
      whose <- if (type == "assign") "those in " else ""
      body <- describe_split(ids, srcs, say, name, whole_row, verb, whose)
    } else if (type == "copy") {
      body <- describe_copy(x, ids, srcs, say, name, whole_row)
    } else if (type == "partition") {
      body <- paste0(describe_split(ids, srcs, say, name, whole_row,
                                    "partitioned into"),
                     describe_partition(x$partition[[as.character(st$id[s])]]))
    } else if (type %in% c("cluster", "select")) {
      # "Clusters, selected by <method>: 40 EAs selected from Urban; ..."
      if (!is.na(st$method[s])) {
        head <- paste0(head, ", selected by ", st$method[s])
      }
      body <- describe_attach(x, s, ids, srcs, say, name, whole_row,
                              link = "from")
    } else if (type == "pool") {
      # "X and Y are combined into Z (n = ..)", one clause per pool
      body <- paste(vapply(seq_along(ids), function(i) {
        paste0(and_list(name(srcs[[i]])), " are combined into ", say(ids[i]),
               if (counted_once(x, ids[i], srcs[[i]], s)) {
                 paste0(", each unit counted once, as some of the boxes are ",
                        "copies of the same units")
               })
      }, ""), collapse = "; ")
    } else if (type == "outcome" &&
               any(ed$kind[ed$to %in% ids] == "measure")) {
      body <- describe_measure(x, s, ids, srcs, say, name, whole_row)
    } else {
      body <- describe_attach(x, s, ids, srcs, say, name, whole_row)
      if (type == "outcome") body <- paste0("the outcome is ", body)
    }
    if (st$display[s] == "summary") {
      body <- paste0(body, "; these are summary boxes, and the boxes they ",
                     "stand for are not drawn one by one")
    }
    body <- paste0(body, unit_change(units, ids, srcs))
    sentences <- c(sentences, paste0(head, ": ", body, "."),
                   describe_details(x, st$id[s], name, flat))
  }
  if (has_data(x)) sentences <- c(sentences, describe_data(x))
  structure(paste(sentences, collapse = " "), class = "studydesignflow_description")
}

describe_split <- function(ids, srcs, say, name, whole_row,
                           verb = "split into", whose = "") {
  is <- if (whose == "") " is " else " are "
  each_are <- if (whose == "") " are each " else " are "
  parents <- unique(unlist(srcs))
  kids <- lapply(parents, function(p) {
    ids[vapply(srcs, function(z) p %in% z, logical(1))]
  })
  kid_text <- vapply(kids, function(k) and_list(vapply(k, say, "")), "")
  if (all(lengths(kids) == 1) && verb == "split into") {
    return(paste(paste0(name(parents), " leads to ", kid_text),
                 collapse = "; "))
  }
  if (length(unique(kid_text)) == 1) {
    if (length(parents) == 1) {
      return(paste0(whose, name(parents), is, verb, " ", kid_text[1]))
    }
    row <- whole_row(parents)
    if (!is.na(row)) {
      return(paste0(whose, "each of the ", length(parents), " boxes in the ",
                    row, " row", is, verb, " ", kid_text[1]))
    }
    return(paste0(whose, if (whose != "") "each of ", and_list(name(parents)),
                  each_are, verb, " ", kid_text[1]))
  }
  paste(paste0(whose, name(parents), is, verb, " ", kid_text),
        collapse = "; ")
}

# "each of Annotator 1 (n = 1,960) and Annotator 2 (n = 1,960) receives all
# the units in Records", one clause per parent, or one for a whole row with
# the same boxes under every parent.
describe_copy <- function(x, ids, srcs, say, name, whole_row) {
  parents <- unique(unlist(srcs))
  kids <- lapply(parents, function(p) {
    ids[vapply(srcs, function(z) p %in% z, logical(1))]
  })
  kid_text <- vapply(kids, function(k) {
    if (length(k) == 1) paste0(say(k), " receives")
    else paste0("each of ", and_list(vapply(k, say, "")), " receives")
  }, "")
  if (length(parents) > 1 && length(unique(kid_text)) == 1) {
    row <- whole_row(parents)
    if (!is.na(row)) {
      return(paste0("under each of the ", length(parents), " boxes in the ",
                    row, " row, ", kid_text[1], " all the units ",
                    if (any(parents %in% (x$side %||% empty_side())$parent))
                      "left " else "", "in that box"))
    }
  }
  # "left in" when exclusion boxes hang off the box being copied
  sd <- x$side %||% empty_side()
  left <- ifelse(parents %in% sd$parent, " all the units left in ",
                 " all the units in ")
  paste(paste0(kid_text, left, name(parents)), collapse = "; ")
}

# An outcome drawn once for several branches (flow_outcome()): "Decrease in
# volume is measured in every branch, drawn as one box joined from the 8
# boxes in the Solution row", or "... measured after each of X and Y, ...".
# One clause per box; the boxes of one call are a single box.
describe_measure <- function(x, s, ids, srcs, say, name, whole_row) {
  nd <- x$nodes
  ed <- x$edges
  st <- x$stages
  # the end boxes before this stage: boxes above it with no box below them
  above <- nd$id[match(nd$stage, st$id) < s]
  ends <- setdiff(above, ed$from[ed$to %in% above])
  parts <- vapply(seq_along(ids), function(i) {
    id <- ids[i]
    src <- srcs[[i]]
    if (length(src) == 1) return(paste0("the outcome is ", say(id), ", after ",
                                        name(src)))
    row <- whole_row(src)
    from_text <- if (!is.na(row)) {
      paste0("the ", length(src), " boxes in the ", row, " row")
    } else {
      paste0("the ", length(src), " boxes ", and_list(name(src)))
    }
    if (setequal(src, ends)) {
      where <- " is measured in every branch"
    } else {
      where <- paste0(" is measured after each of ", and_list(name(src)))
      from_text <- "them"
    }
    computed <- has_data(x) && !is.null(box_rows(x, id)) &&
      !id %in% edited_ids(x, "nodes") && !is.na(nd$n[nd$id == id]) &&
      identical(nd$n[nd$id == id],
                format_n(count_rows(x, box_rows(x, id),
                                    box_count_cols(x)[[as.character(id)]])))
    paste0(say(id), where, ", drawn as one box joined from ", from_text,
           if (computed) "; its count is of all these branches together",
           if (measured_copies(x, src)) {
             paste0("; the branches are copies of the same units, so each ",
                    "unit is measured in more than one of them")
           })
  }, "")
  paste(parts, collapse = "; ")
}

# "randomly assigned to", "assigned by minimisation to", "assigned to"
assign_verb <- function(method) {
  if (is.na(method)) return("assigned to")
  if (method == "random") return("randomly assigned to")
  paste0("assigned by ", method, " to")
}

# "Excluded (n = 112) after Assessed for eligibility: Declined (n = 31) and
# Other reasons (n = 13)", one clause per exclusion box.
describe_exclude <- function(x, stage_id, name, flat, units = box_units(x)) {
  s_units <- side_units(x, units)
  sd <- x$side[x$side$stage == stage_id, ]
  rs <- x$reasons %||% empty_reasons()
  parts <- vapply(seq_len(nrow(sd)), function(i) {
    u <- s_units[[as.character(sd$id[i])]]
    extra <- c(if (!is.na(sd$n[i])) count_words(sd$n[i], u),
               if (!is.na(sd$note[i])) flat_note(sd$note[i]))
    head <- paste0(flat(sd$label[i]),
                   if (length(extra)) paste0(" (", paste(extra, collapse = "; "),
                                             ")"),
                   " after ", name(sd$parent[i]))
    r <- rs[rs$side == sd$id[i], ]
    if (nrow(r) == 0) return(head)
    why <- ifelse(is.na(r$n), flat(r$label),
                  paste0(flat(r$label), " (", count_words(r$n, u), ")"))
    paste0(head, ": ", and_list(why))
  }, character(1))
  paste(parts, collapse = "; ")
}

describe_attach <- function(x, s, ids, srcs, say, name, whole_row,
                            link = "after") {
  st <- x$stages
  nd <- x$nodes
  texts <- vapply(ids, say, "")
  bracket <- lengths(srcs) > 1
  over <- st$over[s]
  group_of <- function(z) {
    if (is.na(over)) return(NA_integer_)
    over_id <- st$id[st$name == over]
    chain <- c(z[1], node_ancestors(x, z[1]))
    chain[nd$stage[match(chain, nd$id)] == over_id][1]
  }
  groups <- vapply(srcs, group_of, integer(1))
  part <- vapply(seq_along(ids), function(i) {
    if (bracket[i]) {
      paste0("across all ", length(srcs[[i]]), " boxes under ",
             name(groups[i]))
    } else {
      paste0(link, " ", name(srcs[[i]]))
    }
  }, "")
  if (length(unique(texts)) == 1) {
    each_text <- say(ids[1], each = length(ids) > 1)
    if (all(bracket)) {
      if (length(ids) == 1) return(paste0(texts[1], ", applied ", part))
      return(paste0(each_text, ", applied across all boxes under each ", over,
                    " box (", and_list(name(groups)), ")"))
    }
    if (!any(bracket)) {
      all_src <- unlist(srcs)
      if (length(all_src) == 1) return(paste0(texts[1], ", ", part))
      row <- whole_row(all_src)
      if (!is.na(row)) {
        return(paste0(each_text, ", for each of the ", length(all_src),
                      " boxes in the ", row, " row"))
      }
      return(paste0(each_text, ", ", link, " each of ",
                    and_list(name(all_src))))
    }
  }
  paste(paste(texts, part), collapse = "; ")
}

# A readable name for every box, unique within its row: boxes that share a
# label are told apart by the boxes above them ("Batch 1 under Type A").
unique_names <- function(x, flat) {
  nd <- x$nodes
  out <- stats::setNames(flat(nd$label), nd$id)
  for (s in unique(nd$stage)) {
    ids <- nd$id[nd$stage == s]
    chains <- lapply(ids, function(i) node_ancestors(x, i))
    names_s <- flat(nd$label[match(ids, nd$id)])
    depth <- 0
    while (anyDuplicated(names_s) &&
           depth < max(lengths(chains))) {
      depth <- depth + 1
      dup <- names_s %in% names_s[duplicated(names_s)]
      for (j in which(dup)) {
        if (length(chains[[j]]) >= depth) {
          anc <- chains[[j]][depth]
          names_s[j] <- paste0(names_s[j], " under ",
                               out[[as.character(anc)]])
        }
      }
    }
    out[as.character(ids)] <- names_s
  }
  out
}

# "; counted in casks" when the boxes of a stage count something else than
# the boxes they come from (see `unit =` in flow_split()), which had a unit:
# under boxes without one, the counts already say what they count.
unit_change <- function(units, ids, srcs) {
  new <- vapply(seq_along(ids), function(i) {
    u <- units[[as.character(ids[i])]]
    above <- unique(units[as.character(srcs[[i]])])
    if (is.na(u) || all(is.na(above)) ||
        (length(above) == 1 && identical(above, u))) {
      NA_character_
    } else {
      u
    }
  }, "")
  new <- unique(new[!is.na(new)])
  if (length(new) == 0) "" else paste0("; counted in ", and_list(new))
}

#' @export
print.studydesignflow_description <- function(x, ...) {
  cat(strwrap(unclass(x), width = getOption("width", 80)), sep = "\n")
  invisible(x)
}

and_list <- function(x) {
  x <- unname(as.character(x))
  if (length(x) <= 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

# Notes may span several lines. In a sentence the lines are joined with ", "
# (the "; " separator is kept for between the count and the note), or with a
# space where a line was only wrapped: it ends in punctuation, or the next
# line goes on in lower case or with a number and no count was just closed.
# A line that starts in lower case is taken to continue the line before it, so a
# list of lower-case items on lines of their own is read without commas.
flat_note <- function(s) {
  s <- gsub("([,;:.])[[:space:]]*\n[[:space:]]*", "\\1 ", s)
  s <- gsub("([^)[:space:]])[[:space:]]*\n[[:space:]]*(?=[a-z0-9])", "\\1 ", s,
            perl = TRUE)
  gsub("[[:space:]]*\n[[:space:]]*", ", ", s)
}

# "; all records of each patient_id are in one set; the sets follow each
# other in time by date (older data for training, newer for testing)",
# with "(not checked against data)" when they were only typed.
describe_partition <- function(p) {
  if (is.null(p)) return("")
  unchecked <- if (isTRUE(p$checked)) "" else " (not checked against data)"
  na <- p$na_group %||% 0
  paste0(
    if (!is.null(p$group)) paste0("; all records of each ", p$group,
                                  " are in one set", unchecked,
                                  if (na > 0) paste0(
                                    " (", na, " row", if (na != 1) "s",
                                    " with missing ", p$group,
                                    " not checked)")),
    if (!is.null(p$time)) paste0("; the sets follow each other in time by ",
                                 p$time, " (older data for training, newer ",
                                 "for testing)", unchecked)
  )
}

# TRUE when a pool joins copies of the same units and its count says each
# unit was counted once: rows from data, or a typed count no larger than the
# number of different units there can be.
counted_once <- function(x, id, src, r) {
  cp <- pool_units(x, src, r)
  if (is.null(cp)) return(FALSE)
  if (has_data(x) && !is.null(box_rows(x, id)) &&
      !id %in% edited_ids(x, "nodes")) {
    return(TRUE)
  }
  v <- parse_count(x$nodes$n[x$nodes$id == id])
  !is.na(v) && !is.na(cp$upper) && v <= cp$upper + 1e-8
}
