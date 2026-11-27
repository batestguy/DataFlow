# Step details: who did a step, with what, when, where and how --------------
#
# Stored only when used, so flows without details are unchanged. x$details is
# a list with up to three parts, each a list of `studydesignflow_details`
# objects (from step_details()), present only when not empty:
# * steps: named by stage id: the details of a whole step (one row);
# * boxes: named by box id: a box whose details differ from its step's;
# * side: named by exclusion box id, the same for exclusion boxes.
#
# The picture shows them as a numbered key (flow_layout(show_details =)): a
# marker "[1]" under the row label of a step, or after the label of a box,
# and the key under the diagram. The description always has them.

detail_fields <- c("who", "tool", "when", "where", "how")
detail_titles <- c(who = "Who", tool = "Tool", when = "When",
                   where = "Where", how = "How")

#' Say how a step was done: who, with what, when, where and how
#'
#' Details of a step of the flow, given to the `details =` argument of the
#' verbs that make boxes (such as [flow_select()] or [flow_assign()]) or set
#' later with [flow_edit()]. They belong to a whole step (one row of the
#' diagram) unless given for single boxes, for example two sites that used
#' different tools.
#'
#' The five fields follow what Datasheets for Datasets (Gebru et al.) and the
#' Croissant RAI vocabulary ask people to report about how data were
#' collected. The field names of a later export are not settled; the
#' correspondences below are a guide:
#'
#' * `who`: the people or agents who collected or labelled the data, e.g.
#'   `"trained enumerators"` or `"two radiologists"` (Datasheets asks who was
#'   involved in collecting the data; Croissant RAI describes annotators).
#' * `tool`: the instrument, sensor, software, API or platform used, e.g.
#'   `"tablet questionnaire (CSPro)"` (Datasheets asks what mechanisms were
#'   used, such as sensors, software or APIs; Croissant RAI has the collection
#'   type, the annotation platform and machine annotation tools).
#' * `when`: the time frame, e.g. `"March to May 2025"` (Datasheets asks over
#'   what time frame the data were collected; Croissant RAI has the data
#'   collection time frame).
#' * `where`: the place or site, e.g. `"Kaduna State"` (neither has a field of
#'   its own; both ask for it within the description of the collection).
#' * `how`: the protocol or procedure, e.g. `"two visits per household"`
#'   (Datasheets asks how the data were acquired and the sampling strategy;
#'   Croissant RAI has the collection and annotation protocols).
#'
#' Each field is one piece of text, written as it should be read. Give at
#' least one.
#'
#' @param who,tool,when,where,how One piece of text each, or `NULL` (the
#'   default) to leave the field out.
#' @return A `studydesignflow_details` object: the fields given, in the order
#'   who, tool, when, where, how.
#' @seealso [flow_layout()] (`show_details`) for how they are drawn, and
#'   [flow_describe()], which always includes them.
#' @export
#' @examples
#' step_details(who = "trained enumerators",
#'              tool = "tablet questionnaire (CSPro)",
#'              when = "March to May 2025")
#'
#' # details of one step, and of one box that differs
#' flow_start("Villages sampled", n = 40, unit = "villages") |>
#'   flow_split("Village", c("Village A", "Village B"), n = 20) |>
#'   flow_select("Households", "20 per village", n = 400,
#'               unit = "households",
#'               details = step_details(who = "trained enumerators")) |>
#'   flow_edit("Village B", details = step_details(tool = "paper forms")) |>
#'   flow_describe()
step_details <- function(who = NULL, tool = NULL, when = NULL, where = NULL,
                         how = NULL) {
  vals <- list(who = who, tool = tool, when = when, where = where, how = how)
  vals <- vals[!vapply(vals, is.null, NA)]
  if (length(vals) == 0) {
    stop("Give at least one of `who`, `tool`, `when`, `where` and `how`.",
         call. = FALSE)
  }
  for (f in names(vals)) {
    v <- vals[[f]]
    if (!is.character(v) || length(v) != 1 || is.na(v) ||
        !nzchar(trimws(v))) {
      stop("`", f, "` must be one piece of text, not empty.", call. = FALSE)
    }
  }
  structure(vals, class = "studydesignflow_details")
}

#' @export
print.studydesignflow_details <- function(x, ...) {
  cat("<step details>\n")
  for (f in names(x)) {
    cat(sprintf("  %-6s %s\n", paste0(detail_titles[[f]], ":"),
                gsub("[[:space:]]*\n[[:space:]]*", " ", x[[f]])))
  }
  invisible(x)
}

is_details <- function(d) inherits(d, "studydesignflow_details")

# A well-formed step_details() object (for validation of stored details).
good_details <- function(d) {
  is_details(d) && length(d) > 0 && !is.null(names(d)) &&
    all(names(d) %in% detail_fields) && !anyDuplicated(names(d)) &&
    all(vapply(d, function(v) {
      is.character(v) && length(v) == 1 && !is.na(v) && nzchar(trimws(v))
    }, NA))
}

details_shape <- paste0(
  "`details` must be step_details() for the whole step, or a list: one ",
  "entry per new box (step_details() or NULL), or entries named by box ",
  "label, with the name `step` for the whole step."
)

# The `details` argument of a verb, checked before the step is made.
check_details_arg <- function(details) {
  if (is.null(details) || is_details(details)) return(invisible())
  if (!is.list(details) || length(details) == 0) {
    stop(details_shape, call. = FALSE)
  }
  ok <- vapply(details, function(d) is.null(d) || is_details(d), NA)
  if (!all(ok)) {
    stop("Entry ", which(!ok)[1], " of the `details` list is not ",
         "step_details() or NULL. ", details_shape, call. = FALSE)
  }
  nm <- names(details)
  if (!is.null(nm) && any(nm != "")) {
    if (anyNA(nm) || any(nm == "")) {
      stop("Name every entry of the `details` list (by box label, or `step` ",
           "for the whole step), or none (one entry per new box).",
           call. = FALSE)
    }
    if (anyDuplicated(nm)) {
      stop("The `details` list names \"", nm[duplicated(nm)][1],
           "\" twice.", call. = FALSE)
    }
  }
  invisible()
}

# Store, or with d = NULL remove, the details of a step ("steps"), a box
# ("boxes") or an exclusion box ("side"). Empty parts, and an empty store,
# are dropped, so a flow whose details are all removed is as without them.
put_details <- function(x, part, id, d) {
  store <- x$details %||% list()
  one <- store[[part]] %||% list()
  one[[as.character(id)]] <- d
  store[[part]] <- if (length(one) > 0) one
  x$details <- if (length(store) > 0) store
  x
}

# Record the `details` of the step just made (the last stage): one
# step_details() for the step, or a list for its new boxes (see
# check_details_arg()). `side = TRUE` for a step of exclusion boxes.
add_details <- function(x, details, side = FALSE) {
  if (is.null(details)) return(x)
  check_details_arg(details)
  sid <- x$stages$id[nrow(x$stages)]
  if (is_details(details)) {
    return(validate_studydesignflow(put_details(x, "steps", sid, details)))
  }
  tab <- if (side) x$side else x$nodes
  ids <- tab$id[tab$stage == sid]
  labels <- tab$label[tab$stage == sid]
  part <- if (side) "side" else "boxes"
  nm <- names(details)
  if (is.null(nm) || all(nm == "")) {
    if (length(details) != length(ids)) {
      stop("`details` is a list of ", length(details), " but this step makes ",
           length(ids), " box", if (length(ids) != 1) "es", ". Give one ",
           "entry per new box (NULL for none), in the order flow_nodes() ",
           "lists them, or name the entries by box label.", call. = FALSE)
    }
    for (i in seq_along(ids)) {
      if (!is.null(details[[i]])) {
        x <- put_details(x, part, ids[i], details[[i]])
      }
    }
    return(validate_studydesignflow(x))
  }
  flat <- gsub("[[:space:]]*\n[[:space:]]*", " ", labels)
  for (j in seq_along(details)) {
    if (is.null(details[[j]])) next
    if (nm[j] == "step") {
      x <- put_details(x, "steps", sid, details[[j]])
      next
    }
    hit <- ids[labels == nm[j] | flat == nm[j]]
    if (length(hit) == 0) {
      stop("`details` names \"", nm[j], "\", but this step makes no box with ",
           "that label. Its boxes are ",
           and_list(paste0("\"", unique(flat), "\"")),
           "; use the name `step` for the whole step.", call. = FALSE)
    }
    for (h in hit) x <- put_details(x, part, h, details[[j]])
  }
  validate_studydesignflow(x)
}

# Check stored details (see validate_studydesignflow()).
check_details_store <- function(d, stage_ids, box_ids, side_ids) {
  if (is.null(d)) return(invisible())
  bad <- function(what) {
    stop("The stored details (`details`) are not valid: ", what, ".",
         call. = FALSE)
  }
  if (!is.list(d) || is.null(names(d)) ||
      !all(names(d) %in% c("steps", "boxes", "side"))) {
    bad("expected a list with parts `steps`, `boxes` and `side`")
  }
  ids <- list(steps = stage_ids, boxes = box_ids, side = side_ids)
  for (p in names(d)) {
    one <- d[[p]]
    if (!is.list(one) || length(one) == 0 || is.null(names(one)) ||
        !all(names(one) %in% as.character(ids[[p]]))) {
      bad(paste0("part `", p, "` must be named by the ids of existing ",
                 c(steps = "stages", boxes = "boxes",
                   side = "exclusion boxes")[[p]]))
    }
    if (!all(vapply(one, good_details, NA))) {
      bad(paste0("every entry of part `", p, "` must come from ",
                 "step_details()"))
    }
  }
  invisible()
}

# --- the numbered key in the picture ----------------------------------------

# The number of every step and box with details: rows top to bottom; in a
# row, the step first (its marker sits in the row-label column, left of the
# boxes), then its boxes and exclusion boxes from left to right. `place` lays
# out a flow (flow_layout() with the caller's settings): left-to-right order
# is taken from a layout whose labels already carry a marker of the widest
# number, so it is the order of the final picture.
detail_marks <- function(x, place) {
  d <- x$details
  st <- x$stages
  steps <- as.integer(names(d$steps))
  boxes <- as.integer(names(d$boxes))
  side <- as.integer(names(d$side))
  total <- length(steps) + length(boxes) + length(side)
  mark <- paste0(" [", total, "]")
  x2 <- x
  x2$details <- NULL
  i <- x2$nodes$id %in% boxes
  x2$nodes$label[i] <- paste0(x2$nodes$label[i], mark)
  i <- x2$side$id %in% side
  x2$side$label[i] <- paste0(x2$side$label[i], mark)
  lay <- suppressWarnings(place(x2))
  items <- rbind(
    data.frame(what = rep("steps", length(steps)), id = steps,
               row = match(steps, st$id), x = rep(-Inf, length(steps))),
    data.frame(what = rep("boxes", length(boxes)), id = boxes,
               row = lay$boxes$row[match(boxes, lay$boxes$id)],
               x = lay$boxes$x[match(boxes, lay$boxes$id)]),
    data.frame(what = rep("side", length(side)), id = side,
               row = lay$side$row[match(side, lay$side$id)],
               x = lay$side$x[match(side, lay$side$id)])
  )
  items <- items[order(items$row, items$x), ]
  items$k <- seq_len(nrow(items))
  rownames(items) <- NULL
  num <- function(w) {
    stats::setNames(items$k[items$what == w],
                    as.character(items$id[items$what == w]))
  }
  list(items = items, steps = num("steps"), boxes = num("boxes"),
       side = num("side"))
}

# Lines of the key under the diagram: per entry a heading "[k] <name>" in
# bold, then one line per field ("Who: trained enumerators"), wrapped by
# measured width to `width` inches, continuation lines indented under the
# value. Returns a data frame of lines with their position (left edge `x`,
# centre `y`), measured width and rectangle.
detail_key <- function(x, marks, x0, top, width, size, family) {
  flat <- function(s) gsub("[[:space:]]*\n[[:space:]]*", " ", s)
  nice <- unique_names(x, flat)
  sd <- x$side %||% empty_side()
  it <- marks$items
  step_in <- size / 72 * 1.4453
  indent <- 0.15
  entry_gap <- 0.06
  lines <- list()
  with_measure_device({
    wd <- function(s, face = "plain") text_width(s, size, family, face)
    y <- top
    for (j in seq_len(nrow(it))) {
      id <- it$id[j]
      d <- x$details[[it$what[j]]][[as.character(id)]]
      # headings name what the picture shows: the row label of a step (its
      # name when the label is hidden), and an exclusion box with the box it
      # hangs off, as in the description
      head <- switch(it$what[j],
        steps = {
          rl <- x$stages$row_label[x$stages$id == id]
          if (is.na(rl) || rl == "") x$stages$name[x$stages$id == id] else rl
        },
        boxes = nice[[as.character(id)]],
        side = paste(flat(sd$label[sd$id == id]), "after",
                     nice[[as.character(sd$parent[sd$id == id])]]))
      add <- function(text, left, face) {
        y <<- y - step_in
        lines[[length(lines) + 1]] <<- data.frame(
          entry = it$k[j], text = text, face = face, x = left,
          y = y + step_in / 2, w = wd(text, face), stringsAsFactors = FALSE
        )
      }
      add(paste0("[", it$k[j], "] ", flat(head)), x0, "bold")
      for (f in names(d)) {
        lead <- paste0(detail_titles[[f]], ": ")
        hang <- wd(lead)
        words <- strsplit(flat(d[[f]]), " ", fixed = TRUE)[[1]]
        words <- words[words != ""]
        avail <- width - indent
        cur <- lead
        first <- TRUE
        for (w in words) {
          try_text <- if (cur == "" || cur == lead) paste0(cur, w) else
            paste(cur, w)
          room <- if (first) avail else avail - hang
          if (cur != "" && cur != lead && wd(try_text) > room) {
            add(cur, x0 + indent + if (first) 0 else hang, "plain")
            first <- FALSE
            cur <- w
          } else {
            cur <- try_text
          }
        }
        add(cur, x0 + indent + if (first) 0 else hang, "plain")
      }
      y <- y - entry_gap
    }
  })
  key <- do.call(rbind, lines)
  h <- size / 72 * 1.2
  key$size <- rep(size, nrow(key))
  key$xmin <- key$x
  key$xmax <- key$x + key$w
  key$ymin <- key$y - h / 2
  key$ymax <- key$y + h / 2
  rownames(key) <- NULL
  key
}

# --- the description ----------------------------------------------------------

# "Done by trained enumerators, with tablet questionnaire (CSPro), March to
# May 2025, in Kaduna State; how: two visits per household". `lead` starts
# the sentence ("Done", or "done" after "For Village B: ").
details_words <- function(d, lead = "Done") {
  v <- vapply(d, function(s) {
    s <- gsub("[[:space:]]*\n[[:space:]]*", " ", trimws(s))
    sub("[.]+$", "", s)
  }, "")
  first <- c(if (!is.null(d$who)) paste("by", v[["who"]]),
             if (!is.null(d$tool)) paste("with", v[["tool"]]),
             if (!is.null(d$when)) v[["when"]],
             if (!is.null(d$where)) paste("in", v[["where"]]))
  how <- if (!is.null(d$how)) v[["how"]]
  if (length(first) == 0) {
    return(paste0(if (lead == "Done") "How" else "how", ": ", how))
  }
  paste0(lead, " ", paste(first, collapse = ", "),
         if (!is.null(how)) paste0("; how: ", how))
}

# Sentences for the details of stage `sid` and of its boxes (or exclusion
# boxes), in the order of the boxes.
describe_details <- function(x, sid, name, flat) {
  d <- x$details
  if (is.null(d)) return(character())
  out <- character()
  step <- d$steps[[as.character(sid)]]
  if (!is.null(step)) out <- paste0(details_words(step), ".")
  if (x$stages$type[x$stages$id == sid] == "exclude") {
    sd <- x$side[x$side$stage == sid, ]
    for (i in seq_len(nrow(sd))) {
      one <- d$side[[as.character(sd$id[i])]]
      if (is.null(one)) next
      out <- c(out, paste0("For ", flat(sd$label[i]), " after ",
                           name(sd$parent[i]), ": ",
                           details_words(one, "done"), "."))
    }
  } else {
    for (id in x$nodes$id[x$nodes$stage == sid]) {
      one <- d$boxes[[as.character(id)]]
      if (is.null(one)) next
      out <- c(out, paste0("For ", name(id), ": ",
                           details_words(one, "done"), "."))
    }
  }
  out
}
