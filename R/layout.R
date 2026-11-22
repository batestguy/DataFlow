# Layout engine: dataflow -> coordinates (inches). Pure data, no drawing. --

#' Compute the positions of everything in a flow diagram
#'
#' Turns a `dataflow` into coordinates, in inches, for every box, arrow,
#' bracket and row label. This is what [autoplot.dataflow()] draws; it is
#' exported so layouts can be inspected and tested as data.
#'
#' Boxes are sized from their text. Each box is centred over the boxes that
#' follow it; a box joined by a bracket is centred under the boxes it joins.
#' Rows sit at fixed heights, one row per stage.
#'
#' @param x A `dataflow` object.
#' @param count_format How a box's label and count are combined. `{label}`
#'   and `{n}` are replaced; used only for boxes that have a count.
#' @param base_size Font size of box text, in points.
#' @param family Font family (`""` is the device default sans-serif).
#' @param hgap,vgap Horizontal gap between boxes and vertical gap between
#'   rows, in inches.
#' @param group_gap Horizontal gap between neighbouring groups (boxes that
#'   have boxes below them), in inches, so groups read as separate.
#' @param pad_x,pad_y Space between text and box edge, in inches.
#' @param min_width Narrowest box, in inches.
#' @param row_label_width Row labels longer than this many characters are
#'   wrapped onto several lines.
#' @param connector `"elbow"` (right-angled lines, the default) or
#'   `"straight"` (one straight arrow per box).
#' @return A `dataflow_layout`: a list of data frames `boxes`, `segments`,
#'   `brackets` and `row_labels`, plus `extent` (xmin, xmax, ymin, ymax) and
#'   `size` (natural width and height in inches).
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
                        connector = c("elbow", "straight")) {
  validate_dataflow(x)
  connector <- match.arg(connector)
  st <- x$stages
  nd <- x$nodes
  ed <- x$edges
  line_in <- function(size) size * 1.2 / 72
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
  has_note <- !is.na(nd$note)
  note_size <- base_size * 0.85
  rl <- st$row_label
  rl_wrapped <- vapply(rl, function(s) {
    if (s == "") "" else paste(strwrap(s, row_label_width), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)

  widths <- with_measure_device({
    list(
      main = text_width(text, base_size, measure_family, "plain"),
      note = ifelse(has_note,
                    text_width(ifelse(has_note, nd$note, ""), note_size,
                               measure_family, "italic"), 0),
      row = text_width(rl_wrapped, base_size, measure_family, "bold")
    )
  })
  n_lines <- function(s) lengths(strsplit(s, "\n", fixed = TRUE))
  main_h <- n_lines(text) * line_in(base_size)
  note_h <- ifelse(has_note, n_lines(ifelse(has_note, nd$note, "")) *
                     line_in(note_size) + 0.04, 0)
  w <- pmax(min_width, pmax(widths$main, widths$note) + 2 * pad_x)
  text_h <- main_h + note_h
  h_box <- text_h + 2 * pad_y

  # --- rows ---------------------------------------------------------------
  row <- match(nd$stage, st$id)
  row_h <- vapply(seq_len(nrow(st)), function(r) {
    if (any(row == r)) max(h_box[row == r]) else 0
  }, numeric(1))
  row_top <- -c(0, cumsum(row_h + vgap))[seq_len(nrow(st))]
  row_y <- row_top - row_h / 2
  h <- row_h[row]
  y <- row_y[row]

  per_row <- table(row)
  if (any(per_row > 20)) {
    warning("Some rows have more than 20 boxes and may not fit a page. ",
            "Consider shorter labels or fewer boxes; a summary display for ",
            "large stages is planned.", call. = FALSE)
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
    add_lane(id, start, row_of[[as.character(id)]] - 1)
    if (length(src) > 1) {
      for (s in src) add_lane(s, row_of[[as.character(s)]], start)
    }
  }
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
      src <- as.character(ed$from[ed$to == m])
      # same span rule as the drawn bracket: outer edges of the lowest
      # sources, centres of any higher ones
      low <- row_of[src] == max(row_of[src])
      left <- min(c(pos[src[low]] - width_of[src[low]] / 2, pos[src[!low]]))
      right <- max(c(pos[src[low]] + width_of[src[low]] / 2, pos[src[!low]]))
      sub <- layout_subtree(m)
      off <- (left + right) / 2
      if (overlaps(acc, sub$cont, off)) off <- shift_to_clear(acc, sub$cont)
      pos <- c(pos, sub$pos + off)
      acc <- merge_contour(acc, shift_cont(sub$cont, off))
    }
    pos[key] <- vx
    lanes <- unique(lane_rows[[key]])
    own <- data.frame(row = c(row_of[[key]], lanes),
                      l = c(vx - width_of[[key]] / 2,
                            rep(vx - lane_w, length(lanes))),
                      r = c(vx + width_of[[key]] / 2,
                            rep(vx + lane_w, length(lanes))))
    acc <- merge_contour(acc, own)
    list(pos = pos - vx,
         cont = shift_cont(acc, -vx))
  }
  roots <- nd$id[is.na(nd$anchor)]
  pos <- numeric()
  acc <- data.frame(row = integer(), l = numeric(), r = numeric())
  for (rt in roots) {
    sub <- layout_subtree(rt)
    off <- shift_to_clear(acc, sub$cont, group_gap)
    pos <- c(pos, sub$pos + off)
    acc <- merge_contour(acc, shift_cont(sub$cont, off))
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
        ymid <- pb$ymin - vgap / 2
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
    xl <- min(c(sb$xmin[low], sb$x[!low]))
    xr <- max(c(sb$xmax[low], sb$x[!low]))
    yb <- min(sb$ymin) - drop
    brk[[length(brk) + 1]] <- data.frame(
      group = m, x = c(xl, xl, xr, xr), y = c(yb + tick, yb, yb, yb + tick)
    )
    # sources sitting higher than the lowest one drop a line to the bracket
    high <- sb[sb$ymin > min(sb$ymin) + 1e-9, ]
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
  segments <- if (length(seg)) do.call(rbind, seg) else
    data.frame(x = numeric(), y = numeric(), xend = numeric(),
               yend = numeric(), head = logical(), kind = character())
  brackets <- if (length(brk)) do.call(rbind, brk) else
    data.frame(group = integer(), x = numeric(), y = numeric())

  # --- row labels and extent ---------------------------------------------
  label_gap <- 0.25
  has_rl <- rl_wrapped != ""
  rl_x <- min(boxes$xmin) - label_gap - max(c(0, widths$row[has_rl]))
  row_labels <- data.frame(
    stage = st$name[has_rl], label = rl_wrapped[has_rl],
    x = rep(rl_x, sum(has_rl)), y = row_y[has_rl],
    w = widths$row[has_rl], stringsAsFactors = FALSE
  )
  margin <- 0.1
  xmin <- (if (any(has_rl)) rl_x else min(boxes$xmin)) - margin
  xmax <- max(boxes$xmax) + margin
  ymin <- min(boxes$ymin) - margin
  ymax <- max(boxes$ymax) + margin

  structure(
    list(
      boxes = boxes, segments = segments, brackets = brackets,
      row_labels = row_labels,
      extent = c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      size = c(width = xmax - xmin, height = ymax - ymin),
      params = list(base_size = base_size, family = family,
                    note_size = note_size)
    ),
    class = "dataflow_layout"
  )
}

#' @export
print.dataflow_layout <- function(x, ...) {
  cat(sprintf("<dataflow_layout> %d boxes, %d line segments, %d brackets; ",
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
