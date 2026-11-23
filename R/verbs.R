# Verbs: each takes and returns a dataflow --------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`", arg, "` must be a single piece of text.", call. = FALSE)
  }
}

# The row label drawn in the left margin: the stage name unless the caller
# gives another, or "" for none.
pick_row_label <- function(row_label, stage) {
  if (is.null(row_label)) return(stage)
  check_string(row_label, "row_label")
  row_label
}

check_flow <- function(x) {
  if (!inherits(x, "dataflow")) {
    stop("The first argument must be a dataflow. Start one with flow_start().",
         call. = FALSE)
  }
}

check_display <- function(display, summary_ok = FALSE) {
  if (!is.character(display) || length(display) != 1 ||
      !display %in% c("all", "summary")) {
    stop("`display` must be \"all\" or \"summary\".", call. = FALSE)
  }
  if (display == "summary" && !summary_ok) {
    stop("display = \"summary\" is for steps that split boxes ",
         "(flow_split(), flow_assign(), flow_cluster()). This step already ",
         "draws one box per group.", call. = FALSE)
  }
  display
}

# Counts are drawn exactly as given. Numbers are only formatted (no
# scientific notation, thousands separated by commas); text is kept verbatim.
format_n <- function(n) {
  if (is.null(n)) return(NA_character_)
  if (is.numeric(n)) {
    return(vapply(n, function(v) {
      if (is.na(v)) NA_character_
      else format(v, digits = 15, big.mark = ",", scientific = FALSE,
                  trim = TRUE)
    }, character(1)))
  }
  if (is.character(n)) return(n)
  stop("Counts (`n`) must be numbers or text.", call. = FALSE)
}

format_note <- function(note) {
  if (is.null(note)) return(NA_character_)
  if (!is.character(note)) stop("`note` must be text.", call. = FALSE)
  note
}

# Recycle a per-box value (n or note) to `total` boxes. Accepts a single
# value, one value per box, or one value per label in `per` (repeated for
# every target).
recycle_values <- function(values, total, arg, per = NULL) {
  if (is.list(values)) values <- unlist(lapply(values, function(v) v %||% NA))
  len <- length(values)
  if (len == 1) return(rep(values, total))
  if (len == total) return(values)
  if (!is.null(per) && len == per && total %% per == 0) {
    return(rep(values, total / per))
  }
  stop("`", arg, "` has ", len, " values but there ",
       if (total == 1) "is 1 box" else paste("are", total, "boxes"),
       " to fill. Give one value, or one per box",
       if (!is.null(per)) paste0(", or one per label (", per, ")"), ".",
       call. = FALSE)
}

add_stage <- function(x, name, row_label, type, display,
                      over = NA_character_, method = NA_character_) {
  if (name %in% x$stages$name) {
    stop("There is already a stage called \"", name,
         "\". Stage names must be unique.", call. = FALSE)
  }
  id <- if (nrow(x$stages) == 0) 1L else max(x$stages$id) + 1L
  x$stages <- rbind(x$stages, data.frame(
    id = id, name = name, row_label = row_label, type = type,
    display = display, over = over, method = method,
    stringsAsFactors = FALSE
  ))
  list(x = x, id = id)
}

add_nodes <- function(x, stage, label, n, note, anchor) {
  start <- if (nrow(x$nodes) == 0) 1L else max(x$nodes$id) + 1L
  ids <- seq.int(start, length.out = length(label))
  x$nodes <- rbind(x$nodes, data.frame(
    id = ids, stage = stage, label = label, n = n, note = note,
    anchor = as.integer(anchor), stringsAsFactors = FALSE
  ))
  list(x = x, ids = ids)
}

add_edges <- function(x, from, to, kind) {
  if (length(from) == 0) return(x)
  x$edges <- rbind(x$edges, data.frame(
    from = as.integer(from), to = as.integer(to), kind = kind,
    stringsAsFactors = FALSE
  ))
  x
}

# Which boxes a verb attaches to: the current leaves, or those picked by
# `from` (box ids, or labels of current leaves).
resolve_targets <- function(x, from) {
  if (is.null(from)) return(x$leaves)
  if (length(from) == 0 || anyNA(from)) {
    stop("`from` is empty or has missing values.", call. = FALSE)
  }
  from <- unique(from)
  if (is.numeric(from)) {
    if (any(from != round(from))) {
      stop("Box ids in `from` must be whole numbers.", call. = FALSE)
    }
    bad <- setdiff(from, x$nodes$id)
    if (length(bad) > 0) {
      stop("No box with id ", paste(bad, collapse = ", "),
           ". See flow_nodes() for ids.", call. = FALSE)
    }
    return(as.integer(from))
  }
  if (is.character(from)) {
    hit <- x$leaves[x$nodes$label[match(x$leaves, x$nodes$id)] %in% from]
    if (length(hit) == 0) {
      stop("`from` matched none of the current end boxes. See flow_nodes().",
           call. = FALSE)
    }
    return(hit)
  }
  stop("`from` must be box ids (numbers) or box labels (text).", call. = FALSE)
}

# Replace each target in the leaf list by the boxes that now follow it,
# keeping left-to-right order.
replace_leaves <- function(leaves, targets, new_by_target) {
  out <- integer()
  for (l in leaves) {
    i <- match(l, targets)
    out <- c(out, if (is.na(i)) l else new_by_target[[i]])
  }
  # targets picked by id that were not leaves: append their new boxes
  extra <- setdiff(unlist(new_by_target), out)
  unique(c(out, extra))
}

#' Start a flow diagram
#'
#' Creates the top box of the diagram, or several top boxes side by side.
#'
#' Several labels make several starting streams, for example the two columns
#' of a PRISMA 2020 flow diagram (databases and registers; other sources).
#' Every top box is an end box, so the next verb attaches to all of them;
#' use `from =` to continue one stream only, while the other waits and
#' continues on a later row. [flow_pool()] joins streams.
#'
#' @param label Text for the top box, e.g. `"Cultures"`, or a character
#'   vector for several top boxes side by side.
#' @param n Optional count, drawn exactly as given (a number or text): one
#'   value, or one per label.
#' @param note Optional extra line drawn in smaller italic text in the box:
#'   one value, or one per label.
#' @param stage Optional row label for the top row. By default the top row
#'   has no label.
#' @param data Optional data frame (or, with several labels, a list of data
#'   frames, one per top box) to compute counts from, instead of typing them.
#'   The data are stored once in the flow; each box records which rows are
#'   behind it (see [flow_data()]). Later steps compute their counts from
#'   the data when given `by =` ([flow_split()], [flow_assign()],
#'   [flow_cluster()]), `where =` ([flow_select()], [flow_exclude()]),
#'   formula `reasons` ([flow_exclude()]), or no `n` ([flow_apply()],
#'   [flow_outcome()], [flow_pool()]). The name of the data frame is
#'   recorded and given in [flow_describe()]. Giving both `n` and `data` is
#'   an error.
#' @param count With `data`: `NULL` (the default) counts rows; a column name
#'   counts the distinct non-missing values of that column instead (e.g.
#'   patients, when there is one row per visit).
#' @return A `dataflow` object.
#' @export
#' @examples
#' flow_start("Cultures", n = 100)
#'
#' # counts from a data frame
#' people <- data.frame(id = 1:6, age = c(17, 25, 40, 33, 15, 52))
#' flow_start("Assessed", data = people) |>
#'   flow_exclude("Excluded", reasons = list("Under 18" = ~ age < 18)) |>
#'   flow_apply("Included", "Included")
#'
#' # two starting streams, continued one at a time
#' flow_start(c("Cases", "Controls"), n = c(120, 240)) |>
#'   flow_exclude("Excluded", n = 8, from = "Cases") |>
#'   flow_pool("Analysis", "Analysed together", n = 352)
flow_start <- function(label, n = NULL, note = NULL, stage = NULL,
                       data = NULL, count = NULL) {
  if (!is.character(label) || length(label) == 0 || anyNA(label) ||
      any(label == "")) {
    stop("`label` must be text and not empty.", call. = FALSE)
  }
  if (!is.null(stage)) check_string(stage, "stage")
  k <- length(label)
  if (k == 1 && (length(n) > 1 || length(note) > 1)) {
    stop("The top box takes a single `n` and `note`.", call. = FALSE)
  }
  store <- NULL
  if (!is.null(data)) {
    if (!is.null(n)) {
      stop("Give `n` or `data`, not both: with data, counts are computed.",
           call. = FALSE)
    }
    store <- new_data_store(data, k, data_names(substitute(data), data),
                            count)
    n <- vapply(seq_len(k), function(i) {
      d <- list(data = store)
      count_rows(d, store$offsets[i] + seq_len(nrow(store$sources[[i]])))
    }, numeric(1))
  } else if (!is.null(count)) {
    stop("`count` is for counting from `data`; give `data` too.",
         call. = FALSE)
  }
  n_vals <- recycle_values(format_n(n), k, "n")
  note_vals <- recycle_values(format_note(note), k, "note")
  x <- new_dataflow(
    stages = data.frame(id = integer(), name = character(),
                        row_label = character(), type = character(),
                        display = character(), over = character(),
                        method = character(), stringsAsFactors = FALSE),
    nodes = data.frame(id = integer(), stage = integer(), label = character(),
                       n = character(), note = character(),
                       anchor = integer(), stringsAsFactors = FALSE),
    edges = data.frame(from = integer(), to = integer(), kind = character(),
                       stringsAsFactors = FALSE),
    side = empty_side(),
    leaves = integer()
  )
  name <- stage %||% (if (k == 1) label else "Start")
  s <- add_stage(x, name, stage %||% "", "start", "all")
  a <- add_nodes(s$x, s$id, label, n_vals, note_vals, rep(NA, k))
  x <- a$x
  x$leaves <- a$ids
  if (!is.null(store)) {
    x$data <- store
    x <- set_rows(x, a$ids, lapply(seq_len(k), function(i) {
      store$offsets[i] + seq_len(nrow(store$sources[[i]]))
    }))
  }
  validate_dataflow(x)
}

#' Split every current box into several
#'
#' Each current end box (or each box chosen with `from`) gets one child box
#' per label. Arrows join each box to its children.
#'
#' @param .flow A `dataflow` object.
#' @param stage Name of the new stage; drawn as the row label.
#' @param labels Either a character vector, used for every current box, or a
#'   list with one character vector per current box (for different children
#'   under different boxes, including one-to-one steps). With
#'   `display = "summary"` it may also be the number of boxes under each
#'   current box: one number for all, or one per box. With text labels, a
#'   summary shows only how many there are, not the labels themselves.
#' @param n Optional counts, drawn exactly as given: one value for all new
#'   boxes, one per label, one per new box, or a list matching `labels`.
#'   With `display = "summary"`: one value, or one per summary box.
#' @param note Optional notes, recycled like `n`.
#' @param display `"all"` draws every box. `"summary"` draws one box under
#'   each current box standing for all of its boxes in this stage, labelled
#'   by `summary`; use it for stages too large to draw box by box.
#' @param summary Label of each summary box. `{k}` is replaced by the number
#'   of boxes it stands for and `{stage}` by the stage name, e.g.
#'   `"{k} districts"`.
#' @param from Optional box ids or labels to split instead of all current end
#'   boxes. See [flow_nodes()].
#' @param row_label Text drawn in the left margin for this row. Defaults to
#'   `stage`; use `""` for no label (e.g. when a row continues the section
#'   labelled above it).
#' @param by For a flow with data (see [flow_start()]): a column name. Each
#'   box is split into one box per value of the column present in its rows
#'   (in factor level order for a factor, otherwise sorted), and counts are
#'   computed. `labels` may then be left out; if given, it fixes the order
#'   and must include every value present, so nothing is dropped silently.
#'   It may name values to draw other text, e.g.
#'   `c("Ex. 1" = "exp1", "Ex. 2" = "exp2")`, and may include planned values
#'   with no rows yet (drawn with a count of 0). Missing values in the
#'   column are an error: remove those rows
#'   first with [flow_exclude()]. With `display = "summary"`, `{k}` is the
#'   number of distinct values and the count is that of the box being split.
#'   Without `by`, new boxes have no rows behind them, so automatic counts
#'   below them stay blank.
#' @return The updated `dataflow`.
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_split("Culture type", list("Type A", "Type B"), n = 50)
flow_split <- function(.flow, stage, labels = NULL, n = NULL, note = NULL,
                       display = "all", from = NULL, row_label = NULL,
                       summary = "{k} {stage}", by = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display, summary_ok = TRUE)
  split_boxes(.flow, stage, labels, n, note, from,
              pick_row_label(row_label, stage), "split", display,
              summary = summary, by = by)
}

# Shared engine for one-to-many steps (split, assign, cluster).
split_boxes <- function(.flow, stage, labels, n, note, from, row_label, type,
                        display, method = NA_character_,
                        summary = "{k} {stage}", by = NULL) {
  targets <- resolve_targets(.flow, from)
  child_rows <- NULL
  if (!is.null(by)) {
    sp <- split_by(.flow, targets, by, labels, display, n)
    labels <- sp$labels
    n <- sp$n
    child_rows <- unlist(sp$rows, recursive = FALSE)
  } else if (is.null(labels)) {
    stop("Give the `labels` of the new boxes (or, for a flow with data, ",
         "`by =` a column).", call. = FALSE)
  }
  if (display == "summary") {
    check_string(summary, "summary")
    k <- summary_counts(labels, length(targets))
    labels <- as.list(vapply(k, function(one) {
      out <- gsub("{k}", format_n(one), summary, fixed = TRUE)
      gsub("{stage}", stage, out, fixed = TRUE)
    }, character(1)))
    if (any(unlist(labels) == "")) {
      stop("`summary` gives an empty label.", call. = FALSE)
    }
  } else if (is.numeric(labels)) {
    stop("A number of boxes can only be drawn as one summary box; use ",
         "display = \"summary\", or give the box labels as text.",
         call. = FALSE)
  }
  if (is.list(labels)) {
    if (length(labels) != length(targets)) {
      stop("`labels` is a list of ", length(labels), " but there are ",
           length(targets), " boxes to split. Give one element per box.",
           call. = FALSE)
    }
    per_target <- lapply(labels, as.character)
    per <- NULL
  } else {
    if (!is.character(labels) || length(labels) == 0) {
      stop("`labels` must be text (a character vector or a list).",
           call. = FALSE)
    }
    per_target <- rep(list(labels), length(targets))
    per <- length(labels)
  }
  if (any(lengths(per_target) == 0)) {
    stop("Every box needs at least one label to split into.", call. = FALSE)
  }
  if (anyNA(unlist(per_target)) || any(unlist(per_target) == "")) {
    stop("Labels must not be missing or empty.", call. = FALSE)
  }
  total <- sum(lengths(per_target))
  n_vals <- per_box_values(n, format_n, per_target, "n", per)
  note_vals <- per_box_values(note, format_note, per_target, "note", per)

  s <- add_stage(.flow, stage, row_label, type, display, method = method)
  x <- s$x
  anchors <- rep(targets, lengths(per_target))
  a <- add_nodes(x, s$id, unlist(per_target), n_vals, note_vals, anchors)
  x <- add_edges(a$x, anchors, a$ids, "arrow")
  x$leaves <- replace_leaves(x$leaves, targets,
                             split(a$ids, factor(anchors, levels = targets)))
  if (!is.null(child_rows)) x <- set_rows(x, a$ids, child_rows)
  validate_dataflow(x)
}

# Labels, counts and rows for a split computed from the column `by`: per box
# being split, one box per value present in its rows (factor level order, or
# sorted), or with display = "summary" one box standing for them all.
split_by <- function(x, targets, by, labels, display, n) {
  need_data(x, "by")
  check_string(by, "by")
  if (!is.null(n)) {
    stop("Give `n` or `by`, not both: with `by`, counts are computed.",
         call. = FALSE)
  }
  vals <- disp <- NULL
  if (!is.null(labels)) {
    if (!is.character(labels) || anyNA(labels) || any(labels == "") ||
        anyDuplicated(labels)) {
      stop("With `by`, `labels` must be the values of \"", by, "\" as text, ",
           "each once, in the order to draw them (named to show other ",
           "text, e.g. c(\"Ex. 1\" = \"exp1\")).", call. = FALSE)
    }
    vals <- unname(labels)
    disp <- names(labels) %||% vals
    disp[is.na(disp) | disp == ""] <- vals[is.na(disp) | disp == ""]
    if (anyDuplicated(disp)) {
      stop("With `by`, the names of `labels` (the text drawn) must differ.",
           call. = FALSE)
    }
  }
  rows <- rows_of_targets(x, targets, "`by`")
  out <- list(labels = list(), n = list(), rows = list())
  for (i in seq_along(targets)) {
    r <- rows[[i]]
    v <- data_values(x, r, by)
    box <- x$nodes$label[x$nodes$id == targets[i]]
    if (anyNA(v)) {
      stop("Column \"", by, "\" has ", sum(is.na(v)), " missing value",
           if (sum(is.na(v)) != 1) "s", " in the rows of box \"", box,
           "\". Remove them from the flow first, e.g. with ",
           "flow_exclude(where = ~ is.na(", by, ")).", call. = FALSE)
    }
    present <- if (is.factor(v)) {
      levels(v)[levels(v) %in% as.character(v)]
    } else {
      as.character(sort(unique(v)))
    }
    if (!is.null(labels)) {
      missing <- setdiff(present, vals)
      if (length(missing) > 0) {
        stop("`labels` must cover every value of \"", by, "\" present; ",
             "under box \"", box, "\" it misses ",
             paste0("\"", missing, "\"", collapse = ", "), ".",
             call. = FALSE)
      }
    }
    if (length(present) == 0 && (is.null(labels) || display == "summary")) {
      stop("Box \"", box, "\" has no rows left to split by \"", by, "\".",
           call. = FALSE)
    }
    if (display == "summary") {
      out$labels[[i]] <- length(present)
      out$n[[i]] <- count_rows(x, r)
      out$rows[[i]] <- list(r)
    } else {
      lv <- vals %||% present
      vc <- as.character(v)
      sub <- lapply(lv, function(l) r[vc == l])
      out$labels[[i]] <- disp %||% lv
      out$n[[i]] <- vapply(sub, function(z) count_rows(x, z), numeric(1))
      out$rows[[i]] <- sub
    }
  }
  if (display == "summary") out$n <- unlist(out$n)
  out
}

# How many boxes each summary box stands for, one per box being split: from
# numbers (one, or one per box), a text vector (its length, for every box), or
# a list (one element per box, each text or a single number).
summary_counts <- function(labels, k_targets) {
  count_of <- function(v) {
    if (is.numeric(v)) {
      if (length(v) != 1 || is.na(v) || v < 1 || v != round(v)) {
        stop("A number of boxes must be a single whole number of 1 or more.",
             call. = FALSE)
      }
      return(v)
    }
    if (is.character(v) && length(v) > 0 && !anyNA(v) && all(v != "")) {
      return(length(v))
    }
    stop("`labels` must be text or numbers of boxes.", call. = FALSE)
  }
  if (is.list(labels)) {
    if (length(labels) != k_targets) {
      stop("`labels` is a list of ", length(labels), " but there are ",
           k_targets, " boxes to split. Give one element per box.",
           call. = FALSE)
    }
    return(vapply(labels, count_of, numeric(1)))
  }
  if (is.numeric(labels)) {
    if (length(labels) != 1 && length(labels) != k_targets) {
      stop("Give one number of boxes, or one per box being split (",
           k_targets, ").", call. = FALSE)
    }
    return(vapply(rep_len(labels, k_targets), count_of, numeric(1)))
  }
  rep(count_of(labels), k_targets)
}

# Values for the boxes of a split. A list gives one element per box being
# split, each recycled to that box's children; anything else goes through
# recycle_values().
per_box_values <- function(values, fmt, per_target, arg, per) {
  total <- sum(lengths(per_target))
  if (!is.list(values)) return(recycle_values(fmt(values), total, arg, per))
  if (length(values) != length(per_target)) {
    stop("`", arg, "` is a list of ", length(values), " but there are ",
         length(per_target), " boxes to split. Give one element per box.",
         call. = FALSE)
  }
  unlist(lapply(seq_along(values), function(i) {
    recycle_values(fmt(values[[i]]), length(per_target[[i]]), arg)
  }))
}

# Shared engine for verbs that add one box per group of current boxes.
# Groups of one are joined by an arrow; larger groups by a bracket, or by
# converging pool lines with `join = "pool"`. `groups` (a list of box ids)
# gives the groups directly instead of `from`/`over`.
attach_per_group <- function(x, stage, label, n, note, over, from, type,
                             display, row_label = stage,
                             method = NA_character_, groups = NULL,
                             join = "bracket", where = NULL) {
  if (!is.null(groups)) {
    targets <- unlist(groups)
  } else {
    targets <- resolve_targets(x, from)
  }
  if (!is.null(groups)) {
    # given directly
  } else if (is.null(over)) {
    groups <- as.list(targets)
  } else {
    groups <- group_over(x, targets, over)
  }
  for (g in groups) {
    nested <- vapply(g, function(m) any(g %in% node_upstream(x, m)), NA)
    if (any(nested)) {
      stop("A group cannot contain a box together with a box below it. ",
           "Choose boxes on the same branch level with `from`.", call. = FALSE)
    }
  }
  k <- length(groups)
  if (!is.character(label) || anyNA(label) || any(label == "")) {
    stop("`label` must be text and not empty.", call. = FALSE)
  }
  labels <- recycle_values(label, k, "label")
  n_vals <- recycle_values(format_n(n), k, "n")
  note_vals <- recycle_values(format_note(note), k, "note")
  group_rows <- NULL
  if (has_data(x)) {
    group_rows <- data_group_rows(x, groups, type, n, where)
    if (is.null(n)) {
      n_vals <- format_n(vapply(group_rows, function(r) count_rows(x, r),
                                numeric(1)))
    }
  } else if (!is.null(where)) {
    need_data(x, "where")
  }
  anchors <- vapply(groups, function(g) {
    if (length(g) == 1) g else lowest_common_ancestor(x, g)
  }, integer(1))

  s <- add_stage(x, stage, row_label, type, display,
                 over %||% NA_character_, method = method)
  a <- add_nodes(s$x, s$id, labels, n_vals, note_vals, anchors)
  x <- a$x
  if (!is.null(group_rows)) x <- set_rows(x, a$ids, group_rows)
  for (i in seq_len(k)) {
    kind <- if (length(groups[[i]]) == 1) "arrow" else join
    x <- add_edges(x, groups[[i]], rep(a$ids[i], length(groups[[i]])), kind)
  }
  # each group's boxes are replaced by the one new box, at the first position
  new_by_target <- vector("list", length(targets))
  for (i in seq_len(k)) {
    g <- groups[[i]]
    new_by_target[[match(g[1], targets)]] <- a$ids[i]
    for (j in g[-1]) new_by_target[[match(j, targets)]] <- integer()
  }
  x$leaves <- replace_leaves(x$leaves, targets, new_by_target)
  validate_dataflow(x)
}

# Rows behind each new box of attach_per_group() in a flow with data. A
# selection with `where` keeps the matching rows; a selection without it has
# no rows (which units were selected is not in the data). Other steps carry
# all rows of their group, or none if a box in the group has none.
data_group_rows <- function(x, groups, type, n, where) {
  if (!is.null(where)) {
    if (!is.null(n)) {
      stop("Give `n` or `where`, not both: with `where`, counts are ",
           "computed.", call. = FALSE)
    }
    return(lapply(groups, function(g) {
      r <- rows_of_targets(x, g, "`where`")[[1]]
      r[eval_condition(x, r, where, "where")]
    }))
  }
  if (type == "select") return(rep(list(NULL), length(groups)))
  lapply(groups, function(g) {
    rs <- lapply(g, function(t) left_rows(x, t))
    if (any(vapply(rs, is.null, NA))) NULL else sort(unique(unlist(rs)))
  })
}

# Group boxes by the box they sit under in the stage named `over`.
group_over <- function(x, targets, over) {
  check_string(over, "over")
  over_id <- x$stages$id[x$stages$name == over]
  if (length(over_id) == 0) {
    stop("`over` must name an earlier stage; there is no stage \"", over,
         "\".", call. = FALSE)
  }
  key <- vapply(targets, function(t) {
    chain <- c(t, node_ancestors(x, t))
    hit <- chain[x$nodes$stage[match(chain, x$nodes$id)] == over_id]
    if (length(hit) == 0) NA_integer_ else hit[1]
  }, integer(1))
  if (anyNA(key)) {
    stop("Some boxes do not sit under the stage \"", over, "\".",
         call. = FALSE)
  }
  unname(split(targets, factor(key, levels = unique(key))))
}

#' Apply something to every current box, or to groups of them
#'
#' Adds a box for a treatment, condition or task applied to the current
#' boxes. With `over = NULL` each current box gets its own new box, joined by
#' an arrow. With `over` naming an earlier stage, the current boxes are
#' grouped by the box they sit under in that stage, and each group is joined
#' by a bracket to a single new box.
#'
#' @inheritParams flow_split
#' @param label Text for the new box(es): one value, or one per group.
#' @param n,note Optional count and note: one value, or one per group. In a
#'   flow with data (see [flow_start()]), a missing `n` is computed from the
#'   rows of each group; boxes made without data have no rows, so the count
#'   below them stays blank. A typed `n` always wins (e.g. "600 trials" is
#'   not a number of rows).
#' @param over Optional name of an earlier stage to group by (draws brackets).
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_split("Composition", c("A", "B", "C")) |>
#'   flow_apply("Condition", "9 conditions each", over = "Experiment ID")
flow_apply <- function(.flow, stage, label, n = NULL, note = NULL,
                       over = NULL, display = "all", from = NULL,
                       row_label = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display)
  attach_per_group(.flow, stage, label, n, note, over, from, "apply", display,
                   pick_row_label(row_label, stage))
}

#' Add the outcome or measurement at the end of each branch
#'
#' @inheritParams flow_apply
#' @export
#' @examples
#' flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_outcome("Number of trials", "600 each")
flow_outcome <- function(.flow, stage, label, n = NULL, note = NULL,
                         display = "all", from = NULL, row_label = NULL) {
  check_flow(.flow)
  check_string(stage, "stage")
  display <- check_display(display)
  attach_per_group(.flow, stage, label, n, note, NULL, from, "outcome",
                   display, pick_row_label(row_label, stage))
}
