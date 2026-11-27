# Kinds of step: done to physical things, or done to data -----------------
#
# A step is done either to physical things (animals, samples, people) or to
# data (cleaning, coding, merging records). Stored only when used, so flows
# without kinds are unchanged. x$kind is a list with up to three parts, each
# a named character vector of "physical" or "data", present only when not
# empty:
# * steps: named by stage id: the kind of a whole step (one row);
# * boxes: named by box id: a box whose kind differs from its step's;
# * side: named by exclusion box id, the same for exclusion boxes.
#
# A box's kind is its own, else its step's, else none (NA). It is never taken
# from the box above: a step done to data below one done to things is the
# point. The picture draws boxes of data steps with a dashed edge, and a key
# under the diagram says what the edges mean (flow_layout(show_kind_key =)).

step_kinds <- c("physical", "data")

kind_shape <- paste0(
  "`kind` must be \"physical\" or \"data\" for the whole step, or a list ",
  "(or character vector): one entry per new box (\"physical\", \"data\", ",
  "or NULL or NA for none), or entries named by box label, with the name ",
  "`step` for the whole step."
)

# One value of `kind`: "physical" or "data" (a unique abbreviation is taken,
# as by match.arg()). With `none = TRUE`, NULL and NA give NA (none).
kind_value <- function(v, none = TRUE) {
  if (none && (is.null(v) || (is.atomic(v) && length(v) == 1 && is.na(v)))) {
    return(NA_character_)
  }
  if (!is.character(v) || length(v) != 1 || is.na(v)) {
    stop("Each `kind` must be \"physical\" or \"data\"",
         if (none) " (or NULL or NA for none)", ".", call. = FALSE)
  }
  hit <- pmatch(v, step_kinds)
  if (is.na(hit)) {
    stop("`kind` must be \"physical\" or \"data\", not \"", v, "\".",
         call. = FALSE)
  }
  step_kinds[hit]
}

# The `kind` argument of a verb, made regular: NULL (none), one value for the
# whole step (a single unnamed string), or a list of entries (NA for none),
# named or not (see kind_shape). Errors on anything else.
norm_kind_arg <- function(kind) {
  if (is.null(kind)) return(NULL)
  if (is.character(kind) && length(kind) == 1 && is.null(names(kind))) {
    return(kind_value(kind, none = FALSE))
  }
  if (!(is.list(kind) || is.character(kind) || is.logical(kind)) ||
      length(kind) == 0 || inherits(kind, "studydesignflow_details")) {
    stop(kind_shape, call. = FALSE)
  }
  vals <- lapply(seq_along(kind), function(i) {
    tryCatch(kind_value(kind[[i]]), error = function(e) {
      stop("Entry ", i, " of `kind` is not \"physical\", \"data\", NULL or ",
           "NA. ", kind_shape, call. = FALSE)
    })
  })
  nm <- names(kind)
  if (!is.null(nm) && any(nm != "")) {
    if (anyNA(nm) || any(nm == "")) {
      stop("Name every entry of `kind` (by box label, or `step` for the ",
           "whole step), or none (one entry per new box).", call. = FALSE)
    }
    if (anyDuplicated(nm)) {
      stop("`kind` names \"", nm[duplicated(nm)][1], "\" twice.",
           call. = FALSE)
    }
    names(vals) <- nm
  }
  vals
}

# The `kind` argument of a verb, checked before the step is made.
check_kind_arg <- function(kind) {
  norm_kind_arg(kind)
  invisible()
}

# Store, or with v = NA remove, the kind of a step ("steps"), a box ("boxes")
# or an exclusion box ("side"). Empty parts, and an empty store, are dropped,
# so a flow whose kinds are all removed is as without them.
put_kind <- function(x, part, id, v) {
  store <- x$kind %||% list()
  one <- store[[part]] %||% character()
  one[as.character(id)] <- v
  one <- one[!is.na(one)]
  store[[part]] <- if (length(one) > 0) one
  x$kind <- if (length(store) > 0) store
  x
}

# Record the `kind` of the step just made (the last stage): one value for the
# step, or entries for its new boxes (see norm_kind_arg()). `side = TRUE` for
# a step of exclusion boxes.
add_kind <- function(x, kind, side = FALSE) {
  kind <- norm_kind_arg(kind)
  if (is.null(kind)) return(x)
  sid <- x$stages$id[nrow(x$stages)]
  if (is.character(kind)) {
    return(validate_studydesignflow(put_kind(x, "steps", sid, kind)))
  }
  tab <- if (side) x$side else x$nodes
  ids <- tab$id[tab$stage == sid]
  labels <- tab$label[tab$stage == sid]
  part <- if (side) "side" else "boxes"
  nm <- names(kind)
  if (is.null(nm)) {
    if (length(kind) != length(ids)) {
      stop("`kind` has ", length(kind),
           if (length(kind) == 1) " entry" else " entries",
           " but this step makes ",
           length(ids), " box", if (length(ids) != 1) "es", ". Give one ",
           "entry per new box (NULL or NA for none), in the order ",
           "flow_nodes() lists them, or name the entries by box label.",
           call. = FALSE)
    }
    for (i in seq_along(ids)) {
      if (!is.na(kind[[i]])) x <- put_kind(x, part, ids[i], kind[[i]])
    }
    return(validate_studydesignflow(x))
  }
  flat <- gsub("[[:space:]]*\n[[:space:]]*", " ", labels)
  for (j in seq_along(kind)) {
    if (is.na(kind[[j]])) next
    if (nm[j] == "step") {
      x <- put_kind(x, "steps", sid, kind[[j]])
      next
    }
    hit <- ids[labels == nm[j] | flat == nm[j]]
    if (length(hit) == 0) {
      stop("`kind` names \"", nm[j], "\", but this step makes no box with ",
           "that label. Its boxes are ",
           and_list(paste0("\"", unique(flat), "\"")),
           "; use the name `step` for the whole step.", call. = FALSE)
    }
    for (h in hit) x <- put_kind(x, part, h, kind[[j]])
  }
  validate_studydesignflow(x)
}

# Check stored kinds (see validate_studydesignflow()).
check_kind_store <- function(k, stage_ids, box_ids, side_ids) {
  if (is.null(k)) return(invisible())
  bad <- function(what) {
    stop("The stored kinds of step (`kind`) are not valid: ", what, ".",
         call. = FALSE)
  }
  if (!is.list(k) || is.null(names(k)) ||
      !all(names(k) %in% c("steps", "boxes", "side"))) {
    bad("expected a list with parts `steps`, `boxes` and `side`")
  }
  ids <- list(steps = stage_ids, boxes = box_ids, side = side_ids)
  for (p in names(k)) {
    one <- k[[p]]
    if (!is.character(one) || length(one) == 0 || is.null(names(one)) ||
        !all(names(one) %in% as.character(ids[[p]])) ||
        anyDuplicated(names(one))) {
      bad(paste0("part `", p, "` must be named by the ids of existing ",
                 c(steps = "stages", boxes = "boxes",
                   side = "exclusion boxes")[[p]]))
    }
    if (anyNA(one) || !all(one %in% step_kinds)) {
      bad(paste0("every entry of part `", p, "` must be \"physical\" or ",
                 "\"data\""))
    }
  }
  invisible()
}

# The kind of every box (named by box id) and of every exclusion box (named
# by exclusion box id): its own, else its step's, else NA.
pick_kind <- function(v, ids) {
  if (is.null(v)) return(rep(NA_character_, length(ids)))
  unname(v[as.character(ids)])
}
box_kinds <- function(x) {
  nd <- x$nodes
  k <- x$kind
  own <- pick_kind(k$boxes, nd$id)
  out <- ifelse(is.na(own), pick_kind(k$steps, nd$stage), own)
  stats::setNames(as.character(out), nd$id)
}
side_kinds <- function(x) {
  sd <- x$side %||% empty_side()
  k <- x$kind
  own <- pick_kind(k$side, sd$id)
  out <- ifelse(is.na(own), pick_kind(k$steps, sd$stage), own)
  stats::setNames(as.character(out), sd$id)
}

# --- the key in the picture ---------------------------------------------------

kind_key_text <- c(data = "Dashed edge: a step done to data")

# The key of kinds under the diagram: one line, shown when a box is done to
# data. There is no line for solid edges (user, 2026-11-27): boxes without a
# kind are solid too, so a line saying "solid = physical" would call them
# physical. Physical steps are named in the description instead. Each line is a
# small sample box (solid or dashed edge) followed by its meaning in words,
# so the key does not depend on colour, or on seeing the dashes. Returns
# NULL when no box has a kind, else a data frame: `kind`, `text`, `size`,
# the text's left edge `x`, centre `y` and width `w`, the sample box
# (`sxmin`, `sxmax`, `symin`, `symax`, `linetype`), and the rectangle of the
# whole line (`xmin` to `ymax`).
kind_key <- function(kinds, x0, top, size, family) {
  shown <- intersect("data", kinds)
  if (length(shown) == 0) return(NULL)
  sw <- 0.3
  sh <- 0.13
  gap <- 0.08
  text_h <- size / 72 * 1.2
  line_h <- max(sh, text_h)
  step <- line_h + 0.06
  text <- unname(kind_key_text[shown])
  w <- with_measure_device(text_width(text, size, family, "plain"))
  k <- length(shown)
  y <- top - line_h / 2 - (seq_len(k) - 1) * step
  data.frame(
    kind = shown, text = text, size = rep(size, k),
    x = rep(x0 + sw + gap, k), y = y, w = unname(w),
    sxmin = rep(x0, k), sxmax = rep(x0 + sw, k),
    symin = y - sh / 2, symax = y + sh / 2,
    linetype = rep("dashed", k),
    xmin = rep(x0, k), xmax = x0 + sw + gap + unname(w),
    ymin = y - line_h / 2, ymax = y + line_h / 2,
    stringsAsFactors = FALSE
  )
}

# --- the description ----------------------------------------------------------

# Sentences naming the steps and boxes done to data or to physical things:
# "Steps done to data (drawn with a dashed edge): Cleaning; Linkage." and
# "Boxes done to physical things: Records under Site B." for boxes whose own
# kind differs from their step's. Nothing when no kind is set.
describe_kind <- function(x, name, flat) {
  k <- x$kind
  if (is.null(k)) return(character())
  st <- x$stages
  sd <- x$side %||% empty_side()
  what <- c(data = "data (drawn with a dashed edge)",
            physical = "physical things")
  out <- character()
  steps <- k$steps %||% character()
  step_of <- function(sid) {
    v <- unname(steps[as.character(sid)])
    if (length(v) == 0) NA_character_ else v
  }
  for (kd in c("data", "physical")) {
    sids <- st$id[st$id %in% as.integer(names(steps)[steps == kd])]
    if (length(sids) > 0) {
      out <- c(out, paste0("Step", if (length(sids) != 1) "s",
                           " done to ", what[[kd]], ": ",
                           paste(st$name[match(sids, st$id)],
                                 collapse = "; "), "."))
    }
  }
  for (kd in c("data", "physical")) {
    boxes <- character()
    # in the order of the rows, then of the boxes
    for (sid in st$id) {
      for (id in x$nodes$id[x$nodes$stage == sid]) {
        v <- pick_kind(k$boxes, id)
        if (!is.na(v) && v == kd && !identical(step_of(sid), kd)) {
          boxes <- c(boxes, name(id))
        }
      }
      for (i in which(sd$stage == sid)) {
        v <- pick_kind(k$side, sd$id[i])
        if (!is.na(v) && v == kd && !identical(step_of(sid), kd)) {
          boxes <- c(boxes, paste(flat(sd$label[i]), "after",
                                  name(sd$parent[i])))
        }
      }
    }
    if (length(boxes) > 0) {
      out <- c(out, paste0(if (length(boxes) == 1) "Box" else "Boxes",
                           " done to ", what[[kd]], ": ",
                           paste(boxes, collapse = "; "), "."))
    }
  }
  out
}
