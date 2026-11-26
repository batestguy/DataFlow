# Layout engine: studydesignflow -> coordinates (inches). Pure data, no drawing.

#' Compute the positions of everything in a flow diagram
#'
#' Turns a `studydesignflow` into coordinates, in inches, for every box, arrow,
#' bracket and row label. This is what [autoplot.studydesignflow()] draws; it is
#' exported so layouts can be inspected and tested as data.
#'
#' Boxes are sized from their text. Each box is centred over the boxes that
#' follow it; a box joined by a bracket is centred under the boxes it joins,
#' and a pooled box under the centres of its sources. Several starting
#' streams sit side by side, `group_gap` apart. Rows sit at fixed heights, one
#' row per stage.
#'
#' @param x A `studydesignflow` object.
#' @param count_format How a box's label and count are combined. `{label}`
#'   and `{n}` are replaced; used only for boxes that have a count. Outcome
#'   events (see [flow_start()]) are added to the count as
#'   "(n = 412; events = 37)", or placed where `{events}` appears if the
#'   format has it.
#' @param base_size Font size of box text, in points.
#' @param family Font family (`""` is the device default sans-serif).
#' @param hgap,vgap Horizontal gap between boxes and vertical gap between
#'   rows, in inches.
#' @param group_gap Horizontal gap between neighbouring groups (boxes that
#'   have boxes below them) and between starting streams, in inches, so
#'   groups read as separate.
#' @param pad_x,pad_y Space between text and box edge, in inches.
#' @param min_width Narrowest box, in inches.
#' @param row_label_width Row labels longer than this many characters are
#'   wrapped onto several lines.
#' @param connector `"elbow"` (right-angled lines, the default) or
#'   `"straight"` (one straight arrow per box).
#' @param side_format How an exclusion box's heading and count are combined,
#'   as in `count_format`. Reasons are listed below it, one per line.
#' @param side_gap Length of the arrow from the flow line to an exclusion
#'   box, in inches.
#' @param show_method Write how boxes were assigned or sampled (the `method` of
#'   [flow_assign()], [flow_cluster()] and [flow_select()]) under the row label,
#'   in italics and brackets? On by default; the method is always in the
#'   description.
#' @param copy_format The line in italics marking a box made by
#'   [flow_copy()], which receives all the units of the box above. `{n}` is
#'   replaced by the box's count. Used only when that count is what is left in
#'   the box above; otherwise `copy_none` is used. Either may be `NA` for no
#'   line, though then nothing in the picture says that the boxes are copies.
#' @param copy_none The line for a copied box whose count is missing or
#'   differs from what is left in the box above.
#' @return A `studydesignflow_layout`: a list of data frames `boxes`, `side`
#'   (exclusion boxes), `segments`, `brackets` and `row_labels`, plus
#'   `extent` (xmin, xmax, ymin, ymax) and `size` (natural width and height
#'   in inches).
#' @export
#' @examples
#' lay <- flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50) |>
#'   flow_layout()
#' lay$boxes[, c("label", "x", "y", "w", "h")]
flow_layout <- function(x, count_format = "{label}\n(n = {n})",
                        base_size = 9, family = "", hgap = 0.12, vgap = 0.45,
                        group_gap = 0.3,
                        pad_x = 0.08, pad_y = 0.06, min_width = 0.3,
                        row_label_width = 16,
                        connector = c("elbow", "straight"),
                        side_format = "{label} (n = {n})", side_gap = 0.3,
                        show_method = TRUE,
                        copy_format = "the same {n} as above",
                        copy_none = "copied from the box above") {
  validate_studydesignflow(x)
  connector <- match.arg(connector)
  if (!isTRUE(show_method) && !isFALSE(show_method)) {
    stop("`show_method` must be TRUE or FALSE.", call. = FALSE)
  }
  st <- x$stages
  nd <- copy_notes(x, copy_format, copy_none)
  ed <- x$edges
  sd <- x$side %||% empty_side()
  rs <- x$reasons %||% empty_reasons()
  # Height of `k` lines of text. Lines are drawn 1.4453 times the font size
  # apart (R's 1.2 times the size, times the line height of 1.2), so 1.2 per
  # line is too little from five lines on.
  text_in <- function(k, size) pmax(1.2 * k, 1.4453 * (k - 1) + 0.36) * size / 72
  # the same without spare room, for row labels, which have no box around them
  label_in <- function(k, size) (1.4453 * (k - 1) + 0.92) * size / 72
  measure_family <- family
  if (!identical(family, "") &&
      !family %in% names(grDevices::pdfFonts())) {
    warning("Font family \"", family, "\" is not one the PDF device knows, ",
            "so box sizes are measured with the standard sans-serif font. ",
            "Text drawn in \"", family, "\" may not fit its boxes exactly.",
            call. = FALSE)
    measure_family <- ""
  }

  # --- box text and size -------------------------------------------------
  text <- ifelse(
    is.na(nd$n), nd$label,
    vapply(seq_len(nrow(nd)), function(i) {
      out <- gsub("{label}", nd$label[i], count_format, fixed = TRUE)
      gsub("{n}", nd$n[i], out, fixed = TRUE)
    }, character(1))
  )
  if (!is.null(x$events) || grepl("{events}", count_format, fixed = TRUE) ||
      grepl("{non_events}", count_format, fixed = TRUE)) {
    text <- box_text_events(nd, x$events, count_format, x$non_events)
  }
  has_note <- !is.na(nd$note)
  note_size <- base_size * 0.85
  rl <- st$row_label
  rl_wrapped <- vapply(rl, function(s) {
    if (s == "") "" else paste(strwrap(s, row_label_width), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)

  method_text <- rep("", nrow(st))
  if (show_method) {
    has_method <- !is.na(st$method)
    method_text[has_method] <- vapply(
      paste0("(", st$method[has_method], ")"),
      function(s) paste(strwrap(s, row_label_width + 2), collapse = "\n"), ""
    )
  }

  # exclusion boxes: heading (with count) then one line per reason
  side_text <- vapply(seq_len(nrow(sd)), function(i) {
    head <- if (is.na(sd$n[i])) sd$label[i] else {
      out <- gsub("{label}", sd$label[i], side_format, fixed = TRUE)
      gsub("{n}", sd$n[i], out, fixed = TRUE)
    }
    r <- rs[rs$side == sd$id[i], ]
    lines <- ifelse(is.na(r$n), r$label, paste0(r$label, " (n = ", r$n, ")"))
    paste(c(head, if (length(lines)) paste0("- ", lines)), collapse = "\n")
  }, character(1))
  side_has_note <- !is.na(sd$note)

  widths <- with_measure_device({
    list(
      main = text_width(text, base_size, measure_family, "plain"),
      note = ifelse(has_note,
                    text_width(ifelse(has_note, nd$note, ""), note_size,
                               measure_family, "italic"), 0),
      row = text_width(rl_wrapped, base_size, measure_family, "bold"),
      method = text_width(method_text, note_size, measure_family, "italic"),
      side = text_width(side_text, base_size, measure_family, "plain"),
      side_note = ifelse(side_has_note,
                         text_width(ifelse(side_has_note, sd$note, ""),
                                    note_size, measure_family, "italic"), 0)
    )
  })
  n_lines <- function(s) lengths(strsplit(s, "\n", fixed = TRUE))
  main_h <- text_in(n_lines(text), base_size)
  note_h <- ifelse(has_note, text_in(n_lines(ifelse(has_note, nd$note, "")),
                                     note_size) + 0.04, 0)
  w <- pmax(min_width, pmax(widths$main, widths$note) + 2 * pad_x)
  text_h <- main_h + note_h
  h_box <- text_h + 2 * pad_y

  side_main_h <- text_in(n_lines(side_text), base_size)
  side_note_h <- ifelse(side_has_note,
                        text_in(n_lines(ifelse(side_has_note, sd$note, "")),
                                note_size) + 0.04, 0)
  side_w <- pmax(min_width, pmax(widths$side, widths$side_note) + 2 * pad_x)
  side_text_h <- side_main_h + side_note_h
  side_h <- side_text_h + 2 * pad_y

  # --- rows ---------------------------------------------------------------
  row <- match(nd$stage, st$id)
  side_row <- match(sd$stage, st$id)
  row_h <- vapply(seq_len(nrow(st)), function(r) {
    max(c(0, h_box[row == r], side_h[side_row == r]))
  }, numeric(1))
  # heights of the row label and of the method under it
  rl_h <- ifelse(rl_wrapped == "", 0,
                 label_in(n_lines(rl_wrapped), base_size))
  mt_h <- ifelse(method_text == "", 0,
                 label_in(n_lines(method_text), note_size) + 0.06)
  row_top <- -c(0, cumsum(row_h + vgap))[seq_len(nrow(st))]
  # a label with a method must not run into the labels of the rows around it:
  # the gap above the lower row is made longer, the boxes keep their height
  block <- rl_h + mt_h
  for (r in seq_len(nrow(st))[-1]) {
    above <- seq_len(r - 1)
    above <- above[block[above] > 0 & (mt_h[above] > 0 | mt_h[r] > 0)]
    if (block[r] == 0 || length(above) == 0) next
    top <- min((row_top - row_h / 2)[above] - block[above] / 2) - 0.05
    short <- (row_top[r] - row_h[r] / 2 + block[r] / 2) - top
    if (short > 0) {
      below <- r:nrow(st)
      row_top[below] <- row_top[below] - short
    }
  }
  row_y <- row_top - row_h / 2
  row_bottom <- row_top - row_h
  h <- row_h[row]
  y <- row_y[row]
  is_side_row <- st$type == "exclude"
  # Rows crossed by lines going down from boxes `parents` (on row `from`) to
  # row `to`, split into the "trunk" and the rest. The trunk is where the line
  # still runs down the parents' centres: through the exclusion rows directly
  # below `from`, and on past the last exclusion box hanging off any of the
  # parents, so the lines that branch out below never cross those boxes.
  crossed_rows <- function(from, to, parents) {
    rows <- if (to - 1 >= from + 1) seq.int(from + 1, to - 1) else integer()
    lead <- rows[cumprod(is_side_row[rows]) == 1]
    own <- side_row[sd$parent %in% parents]
    own <- own[own %in% rows]
    if (length(c(lead, own)) == 0) return(list(trunk = integer(), rest = rows))
    last <- max(c(lead, own))
    list(trunk = rows[rows <= last], rest = rows[rows > last])
  }

  per_row <- table(c(row, side_row))
  if (any(per_row > 20)) {
    warning("Some rows have more than 20 boxes and may not fit a page. ",
            "Draw large stages as one box per group with ",
            "display = \"summary\", or use shorter labels.", call. = FALSE)
  }

  # --- horizontal positions (contour packing over the anchor tree) -------
  incoming <- table(factor(ed$to, levels = nd$id))
  multi <- nd$id[incoming > 1]
  width_of <- stats::setNames(w, nd$id)
  row_of <- stats::setNames(row, nd$id)
  kids_of <- split(nd$id, factor(nd$anchor, levels = nd$id))

  # Rows that a box's connecting lines pass through on the way to or from
  # it (long arrows that skip rows; drop lines from a bracket source that sits
  # higher than the others). They are reserved in the layout so no other box
  # is placed on top of them.
  lane_rows <- stats::setNames(vector("list", nrow(nd)), nd$id)
  # for boxes joined from several sources across exclusion rows: the last
  # exclusion row the join line sits below
  bracket_trunk <- list()
  add_lane <- function(id, from_row, to_row) {
    if (to_row > from_row) {
      key <- as.character(id)
      lane_rows[[key]] <<- c(lane_rows[[key]], seq.int(from_row + 1, to_row))
    }
  }
  for (id in nd$id) {
    src <- ed$from[ed$to == id]
    if (length(src) == 0) next
    start <- max(row_of[as.character(src)])
    if (length(src) == 1) {
      # the parent's line runs down its centre past exclusion rows directly
      # below it; from there the line to this box runs down this box's centre
      cr <- crossed_rows(start, row_of[[as.character(id)]], src)
      key <- as.character(src)
      lane_rows[[key]] <- c(lane_rows[[key]], cr$trunk)
      key <- as.character(id)
      lane_rows[[key]] <- c(lane_rows[[key]], cr$rest)
    } else {
      cr <- crossed_rows(start, row_of[[as.character(id)]], src)
      if (length(cr$trunk)) {
        # the join sits below the exclusion rows: every source runs a line
        # down its centre to it, and the merged box's line starts there
        bracket_trunk[[as.character(id)]] <- max(cr$trunk)
        for (s in src) add_lane(s, row_of[[as.character(s)]], max(cr$trunk))
        key <- as.character(id)
        lane_rows[[key]] <- c(lane_rows[[key]], cr$rest)
      } else {
        add_lane(id, start, row_of[[as.character(id)]] - 1)
        for (s in src) add_lane(s, row_of[[as.character(s)]], start)
      }
    }
  }
  # exclusion boxes hang to the right of their parent's line
  side_of <- split(seq_len(nrow(sd)), factor(sd$parent, levels = nd$id))
  lane_w <- 0.03

  merge_contour <- function(a, b) {
    m <- rbind(a, b)
    if (nrow(m) == 0) return(m)
    data.frame(
      row = sort(unique(m$row)),
      l = tapply(m$l, m$row, min)[as.character(sort(unique(m$row)))],
      r = tapply(m$r, m$row, max)[as.character(sort(unique(m$row)))],
      row.names = NULL
    )
  }
  shift_to_clear <- function(acc, cont, gap = hgap) {
    # smallest offset putting `cont` to the right of `acc` on shared rows
    if (nrow(acc) == 0) return(0)
    shared <- intersect(acc$row, cont$row)
    if (length(shared) == 0) {
      return(max(acc$r) + gap - min(cont$l))
    }
    max(acc$r[match(shared, acc$row)] + gap - cont$l[match(shared, cont$row)])
  }
  overlaps <- function(acc, cont, off) {
    shared <- intersect(acc$row, cont$row)
    if (length(shared) == 0) return(FALSE)
    al <- acc$l[match(shared, acc$row)]
    ar <- acc$r[match(shared, acc$row)]
    cl <- cont$l[match(shared, cont$row)] + off
    cr <- cont$r[match(shared, cont$row)] + off
    any(cl < ar + hgap & cr > al - hgap)
  }

  # Place a box joined from several sources (bracket or pool), centred
  # under them, once the sources have positions in `pos`.
  place_joined <- function(m, pos, acc) {
    src <- as.character(ed$from[ed$to == m])
    if (ed$kind[match(m, ed$to)] == "pool") {
      # pool lines leave the sources' centres
      left <- min(pos[src])
      right <- max(pos[src])
    } else {
      # same span rule as the drawn bracket: outer edges of the lowest
      # sources, centres of any higher ones
      low <- row_of[src] == max(row_of[src])
      if (!is.null(bracket_trunk[[as.character(m)]])) low[] <- FALSE
      left <- min(c(pos[src[low]] - width_of[src[low]] / 2, pos[src[!low]]))
      right <- max(c(pos[src[low]] + width_of[src[low]] / 2, pos[src[!low]]))
    }
    sub <- layout_subtree(m)
    off <- (left + right) / 2
    if (overlaps(acc, sub$cont, off)) off <- shift_to_clear(acc, sub$cont)
    list(pos = c(pos, sub$pos + off),
         acc = merge_contour(acc, shift_cont(sub$cont, off)))
  }

  layout_subtree <- function(v) {
    key <- as.character(v)
    kids <- kids_of[[key]]
    arrow_kids <- kids[!kids %in% multi]
    multi_kids <- kids[kids %in% multi]
    pos <- numeric()
    acc <- data.frame(row = integer(), l = numeric(), r = numeric())
    prev_single <- TRUE
    for (k in arrow_kids) {
      sub <- layout_subtree(k)
      single <- nrow(sub$cont) == 1
      gap <- if (single && prev_single) hgap else group_gap
      prev_single <- single
      off <- shift_to_clear(acc, sub$cont, gap)
      pos <- c(pos, sub$pos + off)
      acc <- merge_contour(acc, shift_cont(sub$cont, off))
    }
    vx <- if (length(arrow_kids) > 0) {
      mean(range(pos[as.character(arrow_kids)]))
    } else {
      0
    }
    for (m in multi_kids) {
      placed <- place_joined(m, pos, acc)
      pos <- placed$pos
      acc <- placed$acc
    }
    pos[key] <- vx
    si <- side_of[[key]]
    # the line from this box down to each of its exclusion boxes
    stub <- unlist(lapply(side_row[si], function(sr) {
      if (sr - 1 >= row_of[[key]] + 1) seq.int(row_of[[key]] + 1, sr - 1)
    }))
    lanes <- unique(c(lane_rows[[key]], stub))
    own <- data.frame(row = c(row_of[[key]], lanes),
                      l = c(vx - width_of[[key]] / 2,
                            rep(vx - lane_w, length(lanes))),
                      r = c(vx + width_of[[key]] / 2,
                            rep(vx + lane_w, length(lanes))))
    if (length(si) > 0) {
      own <- rbind(own, data.frame(row = side_row[si], l = vx - lane_w,
                                   r = vx + side_gap + side_w[si]))
    }
    acc <- merge_contour(acc, own)
    list(pos = pos - vx,
         cont = shift_cont(acc, -vx))
  }
  # Top boxes (one per starting stream) side by side, `group_gap` apart.
  # A pool joining several streams has no common ancestor: it is placed
  # after them, centred under its sources, as if the streams hung off one
  # invisible root.
  roots <- nd$id[is.na(nd$anchor) & row == 1]
  floating <- nd$id[is.na(nd$anchor) & row != 1]
  pos <- numeric()
  acc <- data.frame(row = integer(), l = numeric(), r = numeric())
  for (rt in roots) {
    sub <- layout_subtree(rt)
    off <- shift_to_clear(acc, sub$cont, group_gap)
    pos <- c(pos, sub$pos + off)
    acc <- merge_contour(acc, shift_cont(sub$cont, off))
  }
  for (m in floating) {
    placed <- place_joined(m, pos, acc)
    pos <- placed$pos
    acc <- placed$acc
  }
  xpos <- unname(pos[as.character(nd$id)])
  xpos <- xpos - min(xpos - w / 2)

  type <- st$type[row]
  boxes <- data.frame(
    id = nd$id, stage = st$name[row], type = type, row = row,
    label = nd$label, n = nd$n, note = nd$note, text = text,
    x = xpos, y = y, w = w, h = h,
    xmin = xpos - w / 2, xmax = xpos + w / 2,
    ymin = y - h / 2, ymax = y + h / 2,
    text_y = y + text_h / 2 - main_h / 2,
    note_y = ifelse(has_note,
                    y + text_h / 2 - main_h - 0.04 - (note_h - 0.04) / 2,
                    NA),
    stringsAsFactors = FALSE
  )

  pbx <- boxes[match(sd$parent, boxes$id), ]
  side_x <- pbx$x + side_gap + side_w / 2
  side_y <- row_y[side_row]
  side <- data.frame(
    id = sd$id, parent = sd$parent, stage = st$name[side_row],
    row = side_row, label = sd$label, n = sd$n, note = sd$note,
    text = side_text, x = side_x, y = side_y, w = side_w, h = side_h,
    xmin = side_x - side_w / 2, xmax = side_x + side_w / 2,
    ymin = side_y - side_h / 2, ymax = side_y + side_h / 2,
    text_x = side_x - side_w / 2 + pad_x,
    text_y = side_y + side_text_h / 2 - side_main_h / 2,
    note_y = ifelse(side_has_note,
                    side_y + side_text_h / 2 - side_main_h - 0.04 -
                      (side_note_h - 0.04) / 2, NA),
    stringsAsFactors = FALSE
  )

  # --- connectors ----------------------------------------------------------
  b <- function(id) boxes[match(id, boxes$id), ]
  seg <- list()
  add_seg <- function(x0, y0, x1, y1, head, kind) {
    seg[[length(seg) + 1]] <<- data.frame(
      x = x0, y = y0, xend = x1, yend = y1, head = head, kind = kind
    )
  }
  arrows <- ed[ed$kind == "arrow", ]
  for (p in unique(arrows$from)) {
    kids <- arrows$to[arrows$from == p]
    pb <- b(p)
    for (r in unique(boxes$row[match(kids, boxes$id)])) {
      kb <- b(kids[boxes$row[match(kids, boxes$id)] == r])
      skips <- r > pb$row + 1
      straight <- (connector == "straight" && !skips) ||
        (nrow(kb) == 1 && abs(kb$x - pb$x) < 1e-6)
      if (straight) {
        for (i in seq_len(nrow(kb))) {
          add_seg(pb$x, pb$ymin, kb$x[i], kb$ymax[i], TRUE, "arrow")
        }
      } else {
        # the shared horizontal line sits below any exclusion rows directly
        # under the parent, so it never runs through an exclusion box
        trunk <- crossed_rows(pb$row, r, p)$trunk
        ymid <- if (length(trunk)) row_bottom[max(trunk)] - vgap / 2 else
          pb$ymin - vgap / 2
        add_seg(pb$x, pb$ymin, pb$x, ymid, FALSE, "elbow")
        add_seg(min(kb$x, pb$x), ymid, max(kb$x, pb$x), ymid, FALSE, "elbow")
        for (i in seq_len(nrow(kb))) {
          add_seg(kb$x[i], ymid, kb$x[i], kb$ymax[i], TRUE, "arrow")
        }
      }
    }
  }
  brk <- list()
  tick <- 0.06
  drop <- 0.1
  for (m in unique(ed$to[ed$kind == "bracket"])) {
    sb <- b(ed$from[ed$to == m])
    mb <- b(m)
    # the bracket spans the outer edges of the lowest boxes; boxes higher up
    # join it with a drop line from their centre
    low <- sb$ymin <= min(sb$ymin) + 1e-9
    trunk_row <- bracket_trunk[[as.character(m)]]
    if (is.null(trunk_row)) {
      xl <- min(c(sb$xmin[low], sb$x[!low]))
      xr <- max(c(sb$xmax[low], sb$x[!low]))
      yb <- min(sb$ymin) - drop
      brk[[length(brk) + 1]] <- data.frame(
        group = m, x = c(xl, xl, xr, xr), y = c(yb + tick, yb, yb, yb + tick)
      )
    } else {
      # exclusion boxes hang below the sources: join their centre lines
      # with a plain horizontal line below the exclusion rows
      low[] <- FALSE
      xl <- min(sb$x)
      xr <- max(sb$x)
      yb <- row_bottom[trunk_row] - drop
      brk[[length(brk) + 1]] <- data.frame(group = m, x = c(xl, xr),
                                           y = c(yb, yb))
    }
    # sources not at the bracket's edge drop a line to it from their centre
    high <- sb[!low, ]
    for (i in seq_len(nrow(high))) {
      add_seg(high$x[i], high$ymin[i], high$x[i], yb, FALSE, "bracket")
    }
    xm <- (xl + xr) / 2
    if (abs(xm - mb$x) < 1e-6) {
      add_seg(xm, yb, mb$x, mb$ymax, TRUE, "bracket")
    } else {
      yk <- yb - (vgap - drop) / 2
      add_seg(xm, yb, xm, yk, FALSE, "bracket")
      add_seg(xm, yk, mb$x, yk, FALSE, "bracket")
      add_seg(mb$x, yk, mb$x, mb$ymax, TRUE, "bracket")
    }
  }
  # pools: converging lines, one down from each source's centre to a shared
  # horizontal line (below any exclusion rows under the sources), then one
  # arrow into the top of the pooled box
  for (m in unique(ed$to[ed$kind == "pool"])) {
    sb <- b(ed$from[ed$to == m])
    mb <- b(m)
    trunk_row <- bracket_trunk[[as.character(m)]]
    yb <- if (is.null(trunk_row)) min(sb$ymin) - vgap / 2 else
      row_bottom[trunk_row] - vgap / 2
    for (i in seq_len(nrow(sb))) {
      add_seg(sb$x[i], sb$ymin[i], sb$x[i], yb, FALSE, "pool")
    }
    add_seg(min(c(sb$x, mb$x)), yb, max(c(sb$x, mb$x)), yb, FALSE, "pool")
    add_seg(mb$x, yb, mb$x, mb$ymax, TRUE, "pool")
  }
  # exclusion boxes: the parent's line continues down to the box's middle
  # (if nothing else draws it), then an arrow runs right into the box
  for (i in seq_len(nrow(side))) {
    pb <- b(side$parent[i])
    add_seg(pb$x, pb$ymin, pb$x, side$y[i], FALSE, "side")
    add_seg(pb$x, side$y[i], side$xmin[i], side$y[i], TRUE, "side")
  }
  segments <- if (length(seg)) merge_segments(do.call(rbind, seg)) else
    data.frame(x = numeric(), y = numeric(), xend = numeric(),
               yend = numeric(), head = logical(), kind = character())
  brackets <- if (length(brk)) do.call(rbind, brk) else
    data.frame(group = integer(), x = numeric(), y = numeric())

  # --- row labels and extent ---------------------------------------------
  label_gap <- 0.25
  has_rl <- rl_wrapped != "" | method_text != ""
  rl_w <- pmax(ifelse(rl_wrapped == "", 0, widths$row),
               ifelse(method_text == "", 0, widths$method))
  rl_x <- min(boxes$xmin) - label_gap - max(c(0, rl_w[has_rl]))
  row_labels <- data.frame(
    stage = st$name[has_rl], label = rl_wrapped[has_rl],
    x = rep(rl_x, sum(has_rl)), y = (row_y + mt_h / 2)[has_rl],
    w = rl_w[has_rl], stringsAsFactors = FALSE
  )
  if (show_method) {
    row_labels$method <- ifelse(method_text == "", NA, method_text)[has_rl]
    row_labels$method_y <- (row_y - rl_h / 2 - 0.03)[has_rl]
  }
  margin <- 0.1
  xmin <- (if (any(has_rl)) rl_x else min(boxes$xmin)) - margin
  xmax <- max(c(boxes$xmax, side$xmax)) + margin
  ymin <- min(c(boxes$ymin, side$ymin)) - margin
  ymax <- max(boxes$ymax) + margin
  # a row label, with its method, can be taller than the boxes of its row
  ymin <- min(ymin, (row_y - block / 2 - 0.03)[block > 0])
  ymax <- max(ymax, (row_y + block / 2 + 0.03)[block > 0])

  structure(
    list(
      boxes = boxes, side = side, segments = segments, brackets = brackets,
      row_labels = row_labels,
      extent = c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      size = c(width = xmax - xmin, height = ymax - ymin),
      params = list(base_size = base_size, family = family,
                    note_size = note_size)
    ),
    class = "studydesignflow_layout"
  )
}

#' @export
print.studydesignflow_layout <- function(x, ...) {
  cat(sprintf("<studydesignflow_layout> %d boxes, %d line segments, %d brackets; ",
              nrow(x$boxes), nrow(x$segments),
              length(unique(x$brackets$group))))
  cat(sprintf("natural size %.2f x %.2f in\n", x$size[["width"]],
              x$size[["height"]]))
  invisible(x)
}

# Text measurement on an off-screen PDF device (no file is written), so
# results do not depend on whichever device happens to be open.
with_measure_device <- function(code) {
  old <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  on.exit({
    grDevices::dev.off()
    if (old > 1) grDevices::dev.set(old)
  }, add = TRUE)
  force(code)
}

text_width <- function(s, size, family, face) {
  vapply(s, function(one) {
    if (is.na(one) || one == "") return(0)
    g <- grid::textGrob(one, gp = grid::gpar(fontsize = size,
                                             fontfamily = family,
                                             fontface = face))
    grid::convertWidth(grid::grobWidth(g), "inches", valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

shift_cont <- function(cont, off) {
  cont$l <- cont$l + off
  cont$r <- cont$r + off
  cont
}

# Draw each stretch of line once. Several connectors can run along the same
# line (an arrow down to a box and the stub down to an exclusion box beside
# it, or a pool line); drawn on top of each other they look thicker at some
# resolutions. Segments with an arrowhead are kept whole. Other horizontal or
# vertical segments are taken longest first, and only the parts of them not
# yet drawn on the same line are kept, in their original order and
# direction. Slanted segments are kept as they are.
merge_segments <- function(sg, eps = 1e-9) {
  n <- nrow(sg)
  if (n < 2) return(sg)
  dx <- abs(sg$x - sg$xend)
  dy <- abs(sg$y - sg$yend)
  vert <- dx < eps & dy > eps
  horiz <- dy < eps & dx > eps
  pieces <- lapply(seq_len(n), function(i) sg[i, , drop = FALSE])
  subtract <- function(lo, hi, cov) {
    out <- list(c(lo, hi))
    for (cv in cov) {
      nxt <- list()
      for (p in out) {
        if (cv[2] <= p[1] + eps || cv[1] >= p[2] - eps) {
          nxt <- c(nxt, list(p))
        } else {
          if (cv[1] - p[1] > eps) nxt <- c(nxt, list(c(p[1], cv[1])))
          if (p[2] - cv[2] > eps) nxt <- c(nxt, list(c(cv[2], p[2])))
        }
      }
      out <- nxt
    }
    out
  }
  for (o in c("v", "h")) {
    sel <- which(if (o == "v") vert else horiz)
    if (length(sel) < 2) next
    line <- if (o == "v") sg$x[sel] else sg$y[sel]
    for (grp in split(sel, round(line, 9))) {
      if (length(grp) < 2) next
      a0 <- if (o == "v") sg$y[grp] else sg$x[grp]
      a1 <- if (o == "v") sg$yend[grp] else sg$xend[grp]
      lo <- pmin(a0, a1)
      hi <- pmax(a0, a1)
      ord <- order(!sg$head[grp], -(hi - lo), grp)
      cov <- list()
      for (j in ord) {
        i <- grp[j]
        if (sg$head[i]) {
          cov <- c(cov, list(c(lo[j], hi[j])))
          next
        }
        keep <- subtract(lo[j], hi[j], cov)
        cov <- c(cov, list(c(lo[j], hi[j])))
        if (length(keep) == 1 && abs(keep[[1]][1] - lo[j]) < eps &&
            abs(keep[[1]][2] - hi[j]) < eps) {
          next
        }
        down <- a0[j] > a1[j]
        rows <- lapply(keep, function(k) {
          r <- sg[i, , drop = FALSE]
          from <- if (down) k[2] else k[1]
          to <- if (down) k[1] else k[2]
          if (o == "v") {
            r$y <- from
            r$yend <- to
          } else {
            r$x <- from
            r$xend <- to
          }
          r
        })
        pieces[[i]] <- if (length(rows)) do.call(rbind, rows) else
          sg[0, , drop = FALSE]
      }
    }
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

# Box text when some boxes have outcome events: "(n = 412; events = 37)" with
# the default format, "(events = 37)" for a box without a count, or the
# events placed at `{events}` in a format that has it.
box_text_events <- function(nd, events, count_format, non_events = NULL) {
  ev <- if (is.null(events)) rep(NA_character_, nrow(nd)) else
    unname(events[as.character(nd$id)])
  # rows without the event: computed from data, or n - events when typed
  num <- function(v) suppressWarnings(as.numeric(gsub(",", "", v)))
  ne <- if (is.null(non_events)) rep(NA_character_, nrow(nd)) else
    unname(non_events[as.character(nd$id)])
  typed_ne <- num(nd$n) - num(ev)
  ne <- ifelse(is.na(ne) & !is.na(typed_ne) & is.na(non_events_known(
    non_events, nd$id)), format_n(typed_ne), ne)
  own <- grepl("{events}", count_format, fixed = TRUE) ||
    grepl("{non_events}", count_format, fixed = TRUE)
  vapply(seq_len(nrow(nd)), function(i) {
    n <- nd$n[i]
    e <- ev[i]
    if (is.na(n) && (is.na(e) || own)) return(nd$label[i])
    fmt <- count_format
    if (!own && !is.na(e)) {
      if (is.na(n)) return(paste0(nd$label[i], "
(events = ", e, ")"))
      n <- paste0(n, "; events = ", e)
    }
    out <- gsub("{label}", nd$label[i], fmt, fixed = TRUE)
    out <- gsub("{n}", if (is.na(n)) "" else n, out, fixed = TRUE)
    out <- gsub("{non_events}", if (is.na(ne[i])) "" else ne[i], out,
                fixed = TRUE)
    gsub("{events}", if (is.na(e)) "" else e, out, fixed = TRUE)
  }, character(1))
}

# Whether rows without the event were computed from data for each box
# (NA = not known from data).
non_events_known <- function(non_events, ids) {
  if (is.null(non_events)) return(rep(NA, length(ids)))
  ifelse(as.character(ids) %in% names(non_events), TRUE, NA)
}

# Boxes made by flow_copy() get a first note line saying they hold the same
# units as the box above ("the same 1,960 as above").
copy_notes <- function(x, copy_format, copy_none) {
  nd <- x$nodes
  for (a in list(list(copy_format, "copy_format"), list(copy_none, "copy_none"))) {
    if (length(a[[1]]) != 1 || !(is.na(a[[1]]) || is.character(a[[1]]))) {
      stop("`", a[[2]], "` must be one piece of text, or NA.", call. = FALSE)
    }
  }
  is_copy <- nd$stage %in% x$stages$id[x$stages$type == "copy"]
  if (!any(is_copy)) return(nd)
  # "the same {n}" only when the count is what is left in the box above
  parent <- x$edges$from[match(nd$id, x$edges$to)]
  same <- vapply(seq_len(nrow(nd)), function(i) {
    if (!is_copy[i] || is.na(nd$n[i])) return(FALSE)
    above <- left_count_text(x, parent[i])
    if (is.na(above)) return(FALSE)
    v <- parse_count(c(nd$n[i], above))
    identical(nd$n[i], above) || (!anyNA(v) && !differs(v[1], v[2]))
  }, logical(1))
  line <- rep(copy_none, nrow(nd))
  if (!is.na(copy_format)) {
    line[same] <- vapply(nd$n[same], function(v) {
      gsub("{n}", v, copy_format, fixed = TRUE)
    }, character(1), USE.NAMES = FALSE)
  } else {
    line[same] <- NA
  }
  line[!is_copy] <- NA
  add <- !is.na(line)
  nd$note[add] <- ifelse(is.na(nd$note[add]), line[add],
                         paste0(line[add], "\n", nd$note[add]))
  nd
}
