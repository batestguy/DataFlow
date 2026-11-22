# Plain-language description (alt text) -----------------------------------

#' Describe a flow diagram in plain sentences
#'
#' Writes a short text description of the design, stage by stage. Use it as
#' alternative text for the figure, or in a methods section. [autoplot()]
#' attaches it to the plot as alt text automatically.
#'
#' Boxes that share a label in the same row are told apart by the box they
#' sit under, e.g. "Batch 1 under Type A".
#'
#' @param x A `dataflow` object.
#' @return A single string (class `dataflow_description`). Printing it wraps
#'   the text to the console width.
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_describe()
flow_describe <- function(x) {
  validate_dataflow(x)
  st <- x$stages
  nd <- x$nodes
  ed <- x$edges
  flat <- function(s) gsub("[[:space:]]*\n[[:space:]]*", " ", s)
  nice <- unique_names(x, flat)
  say <- function(id) {
    i <- match(id, nd$id)
    out <- flat(nd$label[i])
    extra <- c(if (!is.na(nd$n[i])) paste0("n = ", nd$n[i]),
               if (!is.na(nd$note[i])) flat_note(nd$note[i]))
    if (length(extra)) paste0(out, " (", paste(extra, collapse = "; "), ")")
    else out
  }
  name <- function(id) nice[as.character(id)]
  # TRUE if `ids` are exactly the boxes of one stage; returns that stage name
  whole_row <- function(ids) {
    s <- unique(nd$stage[match(ids, nd$id)])
    if (length(s) == 1 && setequal(ids, nd$id[nd$stage == s])) {
      st$name[st$id == s]
    } else {
      NA_character_
    }
  }

  root <- nd$id[nd$stage == st$id[1]]
  sentences <- paste0("Flow diagram with ", nrow(st), " stage",
                      if (nrow(st) != 1) "s", ", read from top to bottom. ",
                      "It starts with ", say(root), ".")
  for (s in seq_len(nrow(st))[-1]) {
    ids <- nd$id[nd$stage == st$id[s]]
    type <- st$type[s]
    srcs <- lapply(ids, function(i) ed$from[ed$to == i])
    if (type == "exclude") {
      body <- describe_exclude(x, st$id[s], name, flat)
      sentences <- c(sentences, paste0(
        if (st$row_label[s] != "") paste0(st$name[s], ": "), body, "."))
      next
    }
    if (type %in% c("split", "assign")) {
      verb <- if (type == "split") "split into" else
        assign_verb(st$method[s])
      body <- describe_split(ids, srcs, say, name, whole_row, verb)
    } else {
      body <- describe_attach(x, s, ids, srcs, say, name, whole_row)
      if (type == "outcome") body <- paste0("the outcome is ", body)
    }
    sentences <- c(sentences, paste0(st$name[s], ": ", body, "."))
  }
  structure(paste(sentences, collapse = " "), class = "dataflow_description")
}

describe_split <- function(ids, srcs, say, name, whole_row,
                           verb = "split into") {
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
      return(paste0(name(parents), " is ", verb, " ", kid_text[1]))
    }
    row <- whole_row(parents)
    if (!is.na(row)) {
      return(paste0("each of the ", length(parents), " boxes in the ", row,
                    " row is ", verb, " ", kid_text[1]))
    }
    return(paste0(and_list(name(parents)), " are each ", verb, " ",
                  kid_text[1]))
  }
  paste(paste0(name(parents), " is ", verb, " ", kid_text), collapse = "; ")
}

# "randomly assigned to", "assigned by minimisation to", "assigned to"
assign_verb <- function(method) {
  if (is.na(method)) return("assigned to")
  if (method == "random") return("randomly assigned to")
  paste0("assigned by ", method, " to")
}

# "Excluded (n = 112) after Assessed for eligibility: Declined (n = 31) and
# Other reasons (n = 13)", one clause per exclusion box.
describe_exclude <- function(x, stage_id, name, flat) {
  sd <- x$side[x$side$stage == stage_id, ]
  rs <- x$reasons %||% empty_reasons()
  parts <- vapply(seq_len(nrow(sd)), function(i) {
    extra <- c(if (!is.na(sd$n[i])) paste0("n = ", sd$n[i]),
               if (!is.na(sd$note[i])) flat_note(sd$note[i]))
    head <- paste0(flat(sd$label[i]),
                   if (length(extra)) paste0(" (", paste(extra, collapse = "; "),
                                             ")"),
                   " after ", name(sd$parent[i]))
    r <- rs[rs$side == sd$id[i], ]
    if (nrow(r) == 0) return(head)
    why <- ifelse(is.na(r$n), flat(r$label),
                  paste0(flat(r$label), " (n = ", r$n, ")"))
    paste0(head, ": ", and_list(why))
  }, character(1))
  paste(parts, collapse = "; ")
}

describe_attach <- function(x, s, ids, srcs, say, name, whole_row) {
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
      paste0("after ", name(srcs[[i]]))
    }
  }, "")
  if (length(unique(texts)) == 1) {
    if (all(bracket)) {
      if (length(ids) == 1) return(paste0(texts[1], ", applied ", part))
      return(paste0(texts[1], ", applied across all boxes under each ", over,
                    " box (", and_list(name(groups)), ")"))
    }
    if (!any(bracket)) {
      all_src <- unlist(srcs)
      if (length(all_src) == 1) return(paste0(texts[1], ", ", part))
      row <- whole_row(all_src)
      if (!is.na(row)) {
        return(paste0(texts[1], ", for each of the ", length(all_src),
                      " boxes in the ", row, " row"))
      }
      return(paste0(texts[1], ", after each of ", and_list(name(all_src))))
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
                               flat(nd$label[nd$id == anc]))
        }
      }
    }
    out[as.character(ids)] <- names_s
  }
  out
}

#' @export
print.dataflow_description <- function(x, ...) {
  cat(strwrap(unclass(x), width = getOption("width", 80)), sep = "\n")
  invisible(x)
}

and_list <- function(x) {
  x <- unname(as.character(x))
  if (length(x) <= 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

# Notes may span several lines; in a sentence the lines are joined with ", "
# (the "; " separator is kept for between the count and the note).
flat_note <- function(s) gsub("[[:space:]]*\n[[:space:]]*", ", ", s)
