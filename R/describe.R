# Plain-language description (alt text) -----------------------------------

#' Describe a flow diagram in plain sentences
#'
#' Writes a short text description of the design, stage by stage. Use it as
#' alternative text for the figure, or in a methods section. [autoplot()]
#' attaches it to the plot as alt text automatically.
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
  say <- function(id) {
    i <- match(id, nd$id)
    out <- flat(nd$label[i])
    extra <- c(if (!is.na(nd$n[i])) paste0("n = ", nd$n[i]),
               if (!is.na(nd$note[i])) flat(nd$note[i]))
    if (length(extra)) paste0(out, " (", paste(extra, collapse = "; "), ")")
    else out
  }
  plain <- function(id) flat(nd$label[match(id, nd$id)])

  root <- nd$id[nd$stage == st$id[1]]
  first <- paste0("Flow diagram with ", nrow(st), " stage",
                  if (nrow(st) != 1) "s", ", read from top to bottom. ",
                  "It starts with ", say(root), ".")
  sentences <- first
  for (s in seq_len(nrow(st))[-1]) {
    ids <- nd$id[nd$stage == st$id[s]]
    name <- st$name[s]
    type <- st$type[s]
    srcs <- lapply(ids, function(i) ed$from[ed$to == i])
    if (type == "split") {
      parents <- unique(unlist(srcs))
      kids <- lapply(parents, function(p) ids[vapply(srcs, function(z) p %in% z,
                                                     logical(1))])
      kid_text <- vapply(kids, function(k) and_list(vapply(k, say, "")), "")
      if (all(lengths(kids) == 1)) {
        body <- paste(vapply(seq_along(parents), function(j) {
          paste0(plain(parents[j]), " leads to ", kid_text[j])
        }, ""), collapse = "; ")
      } else if (length(parents) > 1 && length(unique(kid_text)) == 1) {
        p_lab <- vapply(parents, plain, "")
        listed <- if (anyDuplicated(p_lab)) "" else
          paste0(" (", and_list(p_lab), ")")
        body <- paste0("each of the ", length(parents), " boxes above",
                       listed, " is split into ", kid_text[1])
      } else {
        body <- paste(vapply(seq_along(parents), function(j) {
          paste0(plain(parents[j]), " is split into ", kid_text[j])
        }, ""), collapse = "; ")
      }
    } else {
      texts <- vapply(ids, say, "")
      bracket <- lengths(srcs) > 1
      if (any(bracket) && !is.na(st$over[s])) {
        over_id <- st$id[st$name == st$over[s]]
        groups <- vapply(srcs, function(z) {
          chain <- c(z[1], node_ancestors(x, z[1]))
          plain(chain[nd$stage[match(chain, nd$id)] == over_id][1])
        }, "")
        if (length(unique(texts)) == 1) {
          body <- paste0(texts[1], ", applied across all boxes within each ",
                         st$over[s], " (", and_list(groups), ")")
        } else {
          body <- paste(paste0(texts, " across all boxes within ", groups),
                        collapse = "; ")
        }
      } else if (length(unique(texts)) == 1) {
        body <- paste0(texts[1], ", for each of the ", length(ids),
                       " box", if (length(ids) != 1) "es", " above")
      } else {
        body <- paste(paste0(texts, " after ",
                             vapply(srcs, function(z) and_list(plain(z)), "")),
                      collapse = "; ")
      }
      if (type == "outcome") body <- paste0("the outcome is ", body)
    }
    sentences <- c(sentences, paste0(name, ": ", body, "."))
  }
  structure(paste(sentences, collapse = " "), class = "dataflow_description")
}

#' @export
print.dataflow_description <- function(x, ...) {
  cat(strwrap(unclass(x), width = getOption("width", 80)), sep = "\n")
  invisible(x)
}

and_list <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}
