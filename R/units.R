# Units: what each number counts ----------------------------------------------
#
# Stored only when used, so flows without units are unchanged:
# * x$units: the units given to flow boxes (`unit =` on the verbs, or
#   flow_edit()), a character vector named by box id. "" means "no unit"
#   (it stops a unit coming from above).
# * x$side_units: the same for exclusion boxes, named by exclusion box id.
# * x$data$counts: in a flow with data, the count column given to a step
#   (`count =` on the verbs), named by box id.
#
# Units and count columns are resolved when read (box_units(),
# box_count_cols()): a box without its own takes that of the box above it
# along the drawn lines; a box joined from several boxes (bracket, pool,
# outcome measured in every branch) takes theirs when they all agree. So a
# unit changed with flow_edit() carries down to the boxes below.

# A `unit` argument: NULL, or text (NA for none given).
format_unit <- function(unit) {
  if (is.null(unit)) return(NA_character_)
  if (is.logical(unit) && all(is.na(unit))) return(as.character(unit))
  if (!is.character(unit)) {
    stop("`unit` must be text, e.g. \"households\".", call. = FALSE)
  }
  unit
}

# Record units given for new boxes (`what` = "units" for flow boxes,
# "side_units" for exclusion boxes); NA values are not recorded.
set_units <- function(x, ids, vals, what = "units") {
  keep <- !is.na(vals)
  if (!any(keep)) return(x)
  x[[what]][as.character(ids[keep])] <- vals[keep]
  x
}

# A value per box, carried down the drawn lines: a box's own value, else that
# of its single source, else (several sources) theirs when they agree, else
# `mixed`. Top boxes without their own value get `top`. Boxes are made after
# the boxes they are joined from, so increasing id order works.
inherit_down <- function(x, own, top = NA_character_, mixed = NA_character_) {
  nd <- x$nodes
  ed <- x$edges
  out <- stats::setNames(rep(NA_character_, nrow(nd)), nd$id)
  for (id in sort(nd$id)) {
    key <- as.character(id)
    if (!is.null(own) && key %in% names(own) && !is.na(own[[key]])) {
      out[[key]] <- own[[key]]
      next
    }
    src <- ed$from[ed$to == id]
    if (length(src) == 0) {
      out[[key]] <- top
    } else {
      v <- unique(out[as.character(src)])
      out[[key]] <- if (length(v) == 1) v else mixed
    }
  }
  out
}

# The unit of every flow box, named by id; NA for none.
box_units <- function(x) {
  if (is.null(x$units)) {
    return(stats::setNames(rep(NA_character_, nrow(x$nodes)), x$nodes$id))
  }
  out <- inherit_down(x, x$units)
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

# The unit of every exclusion box, named by id: its own, else its parent's.
side_units <- function(x, units = box_units(x)) {
  sd <- x$side %||% empty_side()
  out <- stats::setNames(unname(units[as.character(sd$parent)]), sd$id)
  if (!is.null(x$side_units)) {
    own <- x$side_units[as.character(sd$id)]
    has <- !is.na(own)
    out[has] <- own[has]
    out[!is.na(out) & out == ""] <- NA_character_
  }
  out
}

# In a flow with data: the column whose distinct values each box counts,
# named by id; NA counts rows. The flow's own `count` (flow_start()) is the
# default at the top, and for boxes joined from boxes that count differently.
box_count_cols <- function(x) {
  top <- x$data$count %||% NA_character_
  if (is.null(x$data$counts)) {
    return(stats::setNames(rep(top, nrow(x$nodes)), x$nodes$id))
  }
  inherit_down(x, x$data$counts, top = top, mixed = top)
}

# The count column of a new box joined from `src`: `count` if given, else
# inherited as in box_count_cols().
new_count_col <- function(x, count, src, cols = box_count_cols(x)) {
  if (!is.null(count)) return(count)
  v <- unique(cols[as.character(src)])
  if (length(v) == 1) v else x$data$count %||% NA_character_
}

# A step's `count =`: a column present in every data frame of the flow.
check_count_col <- function(x, count) {
  if (is.null(count)) return(invisible())
  need_data(x, "count")
  check_string(count, "count")
  for (k in seq_along(x$data$sources)) {
    if (!count %in% names(x$data$sources[[k]])) {
      stop("`count` column \"", count, "\" is not in the data frame `",
           x$data$names[k], "`.", call. = FALSE)
    }
  }
  invisible()
}

# Record a step's count column for its new boxes.
set_count_col <- function(x, ids, count) {
  if (is.null(count) || length(ids) == 0) return(x)
  x$data$counts[as.character(ids)] <- count
  x
}

# A count drawn with a format: `{label}` and `{n}` replaced; for a box with a
# unit, "n = {n}" reads "{n} {unit}", and `{unit}` is the unit ("" for
# none).
fill_count <- function(fmt, label, n, unit = NA_character_) {
  if (!is.na(unit)) fmt <- gsub("n = {n}", "{n} {unit}", fmt, fixed = TRUE)
  out <- gsub("{label}", label, fmt, fixed = TRUE)
  out <- gsub("{n}", n, out, fixed = TRUE)
  gsub("{unit}", if (is.na(unit)) "" else unit, out, fixed = TRUE)
}

# "n = 50", or "50 cultures" with a unit.
count_words <- function(n, unit) {
  unit <- rep_len(unit, length(n))
  ifelse(is.na(unit), paste0("n = ", n), paste(n, unit))
}

# Check stored units and count columns (see validate_studydesignflow()).
check_unit_store <- function(v, ids, what) {
  if (is.null(v)) return(invisible())
  if (!is.character(v) || is.null(names(v)) || anyNA(v) ||
      !all(names(v) %in% as.character(ids))) {
    stop("The ", what, " must be text, one per box, named by the ids of ",
         "existing boxes.", call. = FALSE)
  }
  invisible()
}
