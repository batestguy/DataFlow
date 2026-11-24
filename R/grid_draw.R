# Fold grids: layout (pure data, inches), drawing and description ----------

grid_fill <- c(Training = "#D9D9D9", Validation = "#A6A6A6",
               Test = "#4D4D4D", Gap = "#FFFFFF", "Not used" = "#F2F2F2")
grid_text_colour <- c(Training = "#000000", Validation = "#000000",
                      Test = "#FFFFFF", Gap = "#000000",
                      "Not used" = "#595959")
# short forms used when the full role name does not fit
grid_abbrev <- c(Training = "Train", Validation = "Valid.", Test = "Test",
                 Gap = "Gap", "Not used" = "Unused")

#' Compute the positions of everything in a fold grid
#'
#' Turns a `dataflow_grid` (from [flow_folds()]) into coordinates, in
#' inches: one rectangle per block, row labels, fold headers or a time axis,
#' counts that do not fit their block, and a legend when some block is too
#' narrow for its role name. Exported so grids can be inspected and tested
#' as data.
#'
#' Each block shows its role and count ("Test (n = 18)") when they fit, else
#' its role, else a short form ("Unused"), else nothing; the legend then
#' explains the short forms and blank blocks, so the grid never relies on
#' fill colour alone. Counts that do not fit their block are written at the
#' right end of the row, in the same "(n = 18)" form. Fills are greys that
#' stay distinct in greyscale; gaps have a dashed border, and a gap
#' narrower than `min_gap` is drawn `min_gap` wide (wider than scale), which
#' the legend says.
#'
#' @param x A `dataflow_grid`.
#' @param base_size Font size, in points.
#' @param family Font family (`""` is the device default sans-serif).
#' @param width Width of the grid area (without row labels), in inches.
#' @param row_height Height of each row, in inches. By default taller when
#'   blocks show counts.
#' @param row_gap Vertical gap between rows, in inches.
#' @param min_gap Narrowest drawn width of a gap block, in inches.
#' @return A `dataflow_grid_layout`: data frames `blocks` (true positions
#'   `xmin`, `xmax`, and drawn positions `draw_xmin`, `draw_xmax`),
#'   `row_labels`, `row_counts`, `headers`, `ticks`, `axis`, `legend`, plus
#'   `extent` and `size` (natural width and height in inches).
#' @export
#' @examples
#' lay <- grid_layout(flow_folds(k = 5, n = 82))
#' lay$blocks[1:5, c("split", "role", "xmin", "xmax", "text")]
grid_layout <- function(x, base_size = 9, family = "", width = 5.5,
                        row_height = NULL, row_gap = 0.1, min_gap = 0.12) {
  if (!inherits(x, "dataflow_grid")) {
    stop("`x` must be a fold grid made by flow_folds().", call. = FALSE)
  }
  bl <- x$blocks
  rt <- x$settings$role_text
  line_in <- base_size * 1.2 / 72
  pad <- 0.04
  has_n <- any(!is.na(bl$n))
  rh <- row_height %||% (if (has_n) 2 * line_in + 2 * pad + 0.04 else
    line_in + 2 * pad + 0.08)
  S <- nrow(x$splits)
  P <- max(1, x$n_pos)
  header_h <- if (x$axis == "folds") line_in + 0.08 else 0

  # --- blocks -------------------------------------------------------------
  bl$xmin <- (bl$start - 1) / P * width
  bl$xmax <- bl$end / P * width
  row <- match(bl$split, x$splits$id)
  bl$ymax <- -header_h - (row - 1) * (rh + row_gap)
  bl$ymin <- bl$ymax - rh
  bl$y <- (bl$ymin + bl$ymax) / 2
  # narrow gaps are drawn wider than scale, centred on their true position
  narrow <- bl$role == "Gap" & (bl$xmax - bl$xmin) < min_gap
  mid <- (bl$xmin + bl$xmax) / 2
  bl$draw_xmin <- ifelse(narrow, mid - min_gap / 2, bl$xmin)
  bl$draw_xmax <- ifelse(narrow, mid + min_gap / 2, bl$xmax)
  bl$widened <- narrow
  bl$x <- mid
  role_name <- ifelse(is.na(bl$label), unname(rt[bl$role]), bl$label)
  full <- ifelse(is.na(bl$n), role_name,
                 paste0(role_name, "\n(n = ", bl$n, ")"))
  short <- ifelse(is.na(bl$label), unname(grid_abbrev[bl$role]), bl$label)
  measured <- with_measure_device(list(
    full = text_width(full, base_size, family, "plain"),
    name = text_width(role_name, base_size, family, "plain"),
    short = text_width(short, base_size, family, "plain")
  ))
  room <- bl$draw_xmax - bl$draw_xmin - 2 * pad
  tall <- ifelse(is.na(bl$n), 1, 2) * line_in + 2 * pad <= rh
  cand <- cbind(full = measured$full <= room & tall,
                name = measured$name <= room,
                short = measured$short <= room & short != role_name)
  pick <- unname(apply(cand, 1, function(r) c(names(which(r)), "none")[1]))
  bl$text <- ifelse(pick == "full", full, ifelse(
    pick == "name", role_name, ifelse(pick == "short", short, "")))
  bl$shown <- pick
  bl$fill <- unname(grid_fill[bl$role])
  bl$text_colour <- unname(grid_text_colour[bl$role])
  bl$linetype <- ifelse(bl$role == "Gap", "dashed", "solid")

  # --- row labels, counts that did not fit, headers ------------------------
  label_gap <- 0.15
  rl_text <- x$splits$label
  rl_w <- with_measure_device(text_width(rl_text, base_size, family, "bold"))
  rl_x <- -label_gap - max(rl_w)
  row_y <- -header_h - (seq_len(S) - 1) * (rh + row_gap) - rh / 2
  row_labels <- data.frame(label = rl_text, x = rl_x, y = row_y, w = rl_w,
                           stringsAsFactors = FALSE)
  hidden <- !is.na(bl$n) & pick != "full"
  rc_text <- vapply(x$splits$id, function(s) {
    h <- bl[bl$split == s & hidden, ]
    if (nrow(h) == 0) return("")
    paste0(unname(rt[h$role]), " (n = ", h$n, ")", collapse = ", ")
  }, character(1))
  row_end <- vapply(x$splits$id, function(s) max(bl$xmax[bl$split == s]),
                    numeric(1))
  row_counts <- data.frame(label = rc_text, x = row_end + 0.1, y = row_y,
                           stringsAsFactors = FALSE)[rc_text != "", ]
  rc_w <- if (nrow(row_counts)) {
    with_measure_device(text_width(row_counts$label, base_size, family,
                                   "plain"))
  } else {
    numeric()
  }
  headers <- if (x$axis == "folds") {
    data.frame(label = paste("Fold", x$fold_names),
               x = (seq_len(P) - 0.5) / P * width,
               y = -header_h / 2, stringsAsFactors = FALSE)
  } else {
    data.frame(label = character(), x = numeric(), y = numeric())
  }
  bottom <- -header_h - S * (rh + row_gap) + row_gap

  # --- time axis ------------------------------------------------------------
  ticks <- data.frame(label = character(), x = numeric(), y = numeric())
  axis <- data.frame(x = numeric(), y = numeric(), xend = numeric(),
                     yend = numeric())
  axis_title <- NULL
  if (x$axis == "time") {
    ya <- bottom - 0.08
    if (is.null(x$times)) {
      idx <- pretty(c(1, P))
      idx <- idx[idx >= 1 & idx <= P & idx == round(idx)]
      lab <- format_n(idx)
    } else {
      tk <- time_ticks(x$times)
      idx <- tk$idx
      lab <- tk$label
    }
    tx <- (idx - 0.5) / P * width
    ticks <- data.frame(label = lab, x = tx, y = ya - 0.05 - line_in / 2,
                        stringsAsFactors = FALSE)
    axis <- rbind(data.frame(x = 0, y = ya, xend = width, yend = ya),
                  data.frame(x = tx, y = ya, xend = tx, yend = ya - 0.05))
    axis_title <- data.frame(label = grid_axis_title(x), x = width / 2,
                             y = ya - 0.05 - line_in * 1.6,
                             stringsAsFactors = FALSE)
    bottom <- axis_title$y - line_in / 2
  }

  # --- legend: only the short forms and blank blocks actually drawn ---------
  legend <- data.frame(role = character(), label = character(),
                       xmin = numeric(), xmax = numeric(), ymin = numeric(),
                       ymax = numeric(), fill = character(),
                       linetype = character(), stringsAsFactors = FALSE)
  need <- grid_roles[grid_roles %in% bl$role[bl$shown %in% c("short", "none")
                                             | bl$widened]]
  xs <- 0
  if (length(need)) {
    leg_text <- vapply(need, function(r) {
      b <- bl[bl$role == r, ]
      txt <- if (any(b$shown == "short")) {
        paste0(grid_abbrev[[r]], " = ", rt[[r]])
      } else {
        rt[[r]]
      }
      if (any(b$widened)) txt <- paste0(txt, " (drawn wider than scale)")
      txt
    }, character(1))
    leg_w <- with_measure_device(text_width(leg_text, base_size, family,
                                            "plain"))
    sw <- 0.22
    yl <- bottom - 0.12 - line_in / 2
    for (i in seq_along(need)) {
      legend <- rbind(legend, data.frame(
        role = need[i], label = leg_text[[i]], xmin = xs, xmax = xs + sw,
        ymin = yl - line_in / 2, ymax = yl + line_in / 2,
        fill = unname(grid_fill[need[i]]),
        linetype = if (need[i] == "Gap") "dashed" else "solid",
        stringsAsFactors = FALSE))
      xs <- xs + sw + 0.06 + leg_w[i] + 0.2
    }
    bottom <- yl - line_in / 2
  }

  margin <- 0.1
  right <- max(c(width, xs, row_counts$x + rc_w, bl$draw_xmax))
  left <- min(rl_x, bl$draw_xmin)
  extent <- c(xmin = left - margin, xmax = right + margin,
              ymin = bottom - margin, ymax = margin)
  structure(list(
    blocks = bl, row_labels = row_labels, row_counts = row_counts,
    headers = headers, ticks = ticks, axis = axis, axis_title = axis_title,
    legend = legend, extent = extent,
    size = c(width = unname(extent["xmax"] - extent["xmin"]),
             height = unname(extent["ymax"] - extent["ymin"])),
    params = list(base_size = base_size, family = family)
  ), class = "dataflow_grid_layout")
}

# Axis title: "Time (months)" when typed; from data, the time unit if given
# ("Month"), else the time column, capitalised.
grid_axis_title <- function(x) {
  unit <- x$settings$time_unit
  if (is.null(x$times)) return(paste0("Time (", unit, ")"))
  word <- if (unit != "periods") sub("s$", "", unit) else x$settings$time
  paste0(toupper(substr(word, 1, 1)), substring(word, 2))
}

#' @export
print.dataflow_grid_layout <- function(x, ...) {
  cat(sprintf("<dataflow_grid_layout> %d blocks in %d rows; natural size %.2f x %.2f in\n",
              nrow(x$blocks), nrow(x$row_labels), x$size[["width"]],
              x$size[["height"]]))
  invisible(x)
}

#' Draw a fold grid
#'
#' `autoplot()` returns a ggplot object; `plot()` draws it and returns it
#' invisibly. The plot carries the grid's description (from
#' [flow_describe()]) as alt text and its natural size in the
#' `"dataflow_size"` attribute, used by [flow_save()].
#'
#' @param object,x A `dataflow_grid` from [flow_folds()].
#' @param ... Passed to [grid_layout()] (e.g. `base_size`, `width`).
#' @param colour Colour of borders and text outside blocks.
#' @param linewidth Line width of borders.
#' @return A ggplot object.
#' @method autoplot dataflow_grid
#' @export
#' @examples
#' p <- ggplot2::autoplot(flow_folds(k = 5))
autoplot.dataflow_grid <- function(object, ..., colour = "black",
                                   linewidth = 0.4) {
  lay <- grid_layout(object, ...)
  bl <- lay$blocks
  # gaps last, so a gap drawn wider than scale sits on top of its neighbours
  bl <- bl[order(bl$role == "Gap"), ]
  ps <- lay$params
  ex <- lay$extent
  txt <- function(data, ...) {
    ggplot2::geom_text(data = data,
                       ggplot2::aes(x = .data$x, y = .data$y,
                                    label = .data$label),
                       size = ps$base_size, size.unit = "pt",
                       family = ps$family, lineheight = 1.2, ...)
  }
  shown <- bl[bl$text != "", ]
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bl,
      ggplot2::aes(xmin = .data$draw_xmin, xmax = .data$draw_xmax,
                   ymin = .data$ymin, ymax = .data$ymax),
      fill = bl$fill, colour = colour, linewidth = linewidth,
      linetype = bl$linetype
    ) +
    ggplot2::geom_text(
      data = shown,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
      colour = shown$text_colour, size = ps$base_size,
      size.unit = "pt", family = ps$family, lineheight = 1.2
    ) +
    txt(lay$row_labels, hjust = 0, fontface = "bold", colour = colour)
  if (nrow(lay$row_counts)) {
    p <- p + txt(lay$row_counts, hjust = 0, colour = colour)
  }
  if (nrow(lay$headers)) p <- p + txt(lay$headers, colour = colour)
  if (nrow(lay$axis)) {
    p <- p +
      ggplot2::geom_segment(
        data = lay$axis,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                     yend = .data$yend),
        colour = colour, linewidth = linewidth
      ) +
      txt(lay$ticks, colour = colour) +
      txt(lay$axis_title, colour = colour)
  }
  lg <- lay$legend
  if (nrow(lg)) {
    p <- p +
      ggplot2::geom_rect(
        data = lg,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                     ymax = .data$ymax),
        fill = lg$fill, colour = colour, linewidth = linewidth,
        linetype = lg$linetype
      ) +
      txt(data.frame(label = lg$label, x = lg$xmax + 0.06,
                     y = (lg$ymin + lg$ymax) / 2),
          hjust = 0, colour = colour)
  }
  p <- p +
    ggplot2::coord_fixed(xlim = ex[c("xmin", "xmax")],
                         ylim = ex[c("ymin", "ymax")],
                         expand = FALSE, clip = "off") +
    ggplot2::labs(alt = unclass(flow_describe(object))) +
    theme_dataflow(base_size = ps$base_size, base_family = ps$family)
  attr(p, "dataflow_size") <- lay$size
  p
}

#' @rdname autoplot.dataflow_grid
#' @method plot dataflow_grid
#' @export
plot.dataflow_grid <- function(x, ...) {
  p <- autoplot.dataflow_grid(x, ...)
  print(p)
  invisible(p)
}

# Plain-language description of a fold grid.
describe_grid <- function(x) {
  st <- x$settings
  bl <- x$blocks
  rt <- st$role_text
  unit <- st$time_unit
  per <- function(k) paste(k, if (k == 1) sub("s$", "", unit) else unit)
  S <- nrow(x$splits)
  out <- character()
  if (x$type == "kfold") {
    k <- x$n_pos
    first <- bl[bl$split == 1, ]
    ns <- first$n[order(first$start)]
    out <- c(out, paste0(
      "Cross-validation grid with ", k, " folds, one row per split: in ",
      "each split one fold is the ", tolower(rt[["Test"]]), " set",
      if (isTRUE(st$validation)) paste0(", the next fold is the ",
                                        tolower(rt[["Validation"]]), " set"),
      " and the other ", k - if (isTRUE(st$validation)) 2 else 1,
      " folds are used for ", tolower(rt[["Training"]]),
      "; each fold is the ", tolower(rt[["Test"]]), " set once."))
    if (!all(is.na(ns))) {
      out <- c(out, paste0("The folds hold ",
                           and_list(paste0("n = ", ns)), "."))
    }
  } else if (x$type == "assigned") {
    out <- c(out, paste0(
      "Grid of the splits as assigned in the data frame `", x$data$name,
      "` (column ", st$split_col, " for the split, ", st$role_col,
      " for each record's role): ", S, " split", if (S != 1) "s",
      if (x$axis == "time") {
        paste0(", drawn on a time axis by ", st$time)
      } else {
        ", each bar as long as the split's number of rows"
      }, "."))
    if (isTRUE(st$time_checked)) {
      out <- c(out, paste0("Checked: in every split, training rows come ",
                           "before validation and test rows by ", st$time,
                           "."))
    }
  } else if (x$type == "blocked") {
    gap <- st$gap
    out <- c(out, paste0(
      "Blocked cross-validation for time-ordered data: the ", per(x$n_pos),
      " are cut into ", S, " contiguous blocks; each block is the ",
      tolower(rt[["Test"]]), " set once and the other blocks are used for ",
      tolower(rt[["Training"]]),
      if (gap > 0) paste0(", leaving out a gap of ", per(gap),
                          " on each side of the test block"),
      ". Training also uses data after the test block, so this is not an ",
      "evaluation of forecasting."))
  } else {
    step <- st$skip + 1
    head <- paste0(S, " splits, ", x$type, " window: ")
    train <- if (x$type == "expanding") {
      paste0("training starts with ", per(st$initial), " and grows by ",
             per(step))
    } else {
      paste0("training is a moving window of ", per(st$initial),
             " that moves on by ", per(step))
    }
    test <- paste0("each test window is the next ", per(st$assess),
                   if (st$gap > 0) paste0(" after a ", st$gap, "-",
                                          sub("s$", "", unit), " gap")
                   else " after training")
    out <- c(out, paste0("Time-series evaluation grid with ", head, train,
                         "; ", test, "; training always precedes testing ",
                         "(as these settings define the windows)."))
    if (any(bl$role == "Not used")) {
      out <- c(out, "Data outside a split's windows are marked as not used.")
    }
  }
  if (x$type != "kfold" && !all(is.na(bl$n))) {
    out <- c(out, describe_split_counts(x))
  }
  if (any(grid_layout_widened(x))) {
    out <- c(out, "In the picture, gaps narrower than the drawing allows are drawn wider than scale.")
  }
  if (!is.null(x$times)) {
    out <- c(out, paste0("Time runs from ", format_time(x$times[1]), " to ",
                         format_time(x$times[length(x$times)]), " (",
                         length(x$times), " distinct values of ", st$time,
                         ")."))
    if (!regular_times(x$times)) {
      out <- c(out, paste0("Positions are the distinct values of ", st$time,
                           "; their spacing is not regular, so a period is ",
                           "not a fixed stretch of time."))
    }
  }
  if (!is.null(st$group)) {
    na <- st$na_group %||% 0
    out <- c(out, if (isTRUE(st$group_checked)) {
      paste0(if (x$type == "assigned") {
        paste0("Checked: within each split, all records of each ", st$group,
               " have one role")
      } else {
        paste0("All records of each ", st$group, " are in one fold")
      }, if (na > 0) paste0(" (", na, " row", if (na != 1) "s",
                              " with missing ", st$group, " not checked)"),
      ".")
    } else if (x$type == "kfold") {
      paste0("All records of each ", st$group, " are in one fold ",
             "(not checked against data).")
    } else {
      paste0("Records were grouped by ", st$group, " (as stated; not ",
             "checked against data).")
    })
  }
  if (!is.null(st$method)) {
    out <- c(out, paste0("Folds were made by ", st$method,
                         " (as stated; not checked against data)."))
  }
  if (!is.null(x$data)) {
    ed <- bl[bl$id %in% x$edited, ]
    typed <- if (nrow(ed) == 0) "" else if (x$type == "kfold") {
      f <- ed[!duplicated(ed$start), ]
      paste0(", except ", nrow(f), " fold size", if (nrow(f) != 1) "s",
             " typed by hand: ",
             and_list(paste0("fold ", x$fold_names[f$start], " (n = ", f$n,
                             ")")))
    } else {
      paste0(", except ", nrow(ed), " typed by hand: ",
             and_list(paste0(tolower(rt[ed$role]), " in split ",
                             x$splits$label[match(ed$split, x$splits$id)],
                             " (n = ", ed$n, ")")))
    }
    out <- c(out, paste0("Counts were computed from the data frame `",
                         x$data$name, "` (", format_n(nrow(x$data$df)),
                         " rows)", typed, "."))
  }
  structure(paste(out, collapse = " "), class = "dataflow_description")
}

# "Numbers per split: split 1: training (n = 180), gap (n = 3), test
# (n = 18), not used (n = 87); ...". With more than 8 splits, the first two
# and the last are listed and the rest summarised, to keep alt text short.
describe_split_counts <- function(x) {
  bl <- x$blocks
  rt <- x$settings$role_text
  one <- function(s) {
    b <- bl[bl$split == s & !is.na(bl$n), ]
    b <- b[order(b$start), ]
    agg <- tapply(as.numeric(gsub(",", "", b$n)), factor(b$role,
                  levels = grid_roles), sum)
    agg <- agg[!is.na(agg)]
    paste0(tolower(x$splits$label[x$splits$id == s]), ": ",
           paste0(tolower(rt[names(agg)]), " (n = ", format_n(agg), ")",
                  collapse = ", "))
  }
  ids <- x$splits$id
  if (length(ids) <= 8) {
    return(paste0("Numbers per split: ",
                  paste(vapply(ids, one, ""), collapse = "; "), "."))
  }
  shown <- c(ids[1:2], ids[length(ids)])
  paste0("Numbers per split, for the first two and the last of ",
         length(ids), " splits: ",
         paste(vapply(shown, one, ""), collapse = "; "),
         "; the splits between follow the same pattern.")
}

# Whether any gap block would be drawn wider than scale at the default
# layout settings.
grid_layout_widened <- function(x, width = 5.5, min_gap = 0.12) {
  b <- x$blocks
  b$role == "Gap" & (b$end - b$start + 1) / max(1, x$n_pos) * width < min_gap
}

# Regular spacing: equal steps, or (dates) one per month or per year on the
# same day of the month.
regular_times <- function(times) {
  if (length(times) < 3) return(TRUE)
  if (inherits(times, "Date")) {
    d <- as.numeric(diff(times))
    if (length(unique(d)) == 1) return(TRUE)
    lt <- as.POSIXlt(times)
    if (length(unique(lt$mday)) == 1) {
      m <- lt$year * 12 + lt$mon
      if (length(unique(diff(m))) == 1) return(TRUE)
    }
    return(FALSE)
  }
  d <- diff(as.numeric(times))
  isTRUE(all(abs(d - d[1]) < 1e-9 * max(1, abs(d[1]))))
}

# Tick positions (period index) and labels for a time axis from data: round
# values from pretty() (e.g. years for monthly dates), each placed at the
# first period on or after it; ticks too close together are dropped.
time_ticks <- function(times) {
  P <- length(times)
  pr <- tryCatch(pretty(times, n = 6), error = function(e) NULL)
  if (is.null(pr) || !length(pr)) {
    idx <- unique(round(seq(1, P, length.out = min(6, P))))
    return(list(idx = idx, label = format_time(times[idx])))
  }
  labs <- attr(pr, "labels") %||% format_time(pr)
  keep <- pr >= times[1] & pr <= times[P]
  pr <- pr[keep]
  labs <- labs[keep]
  idx <- vapply(seq_along(pr), function(i) which(times >= pr[i])[1], 1L)
  ok <- !duplicated(idx)
  list(idx = idx[ok], label = labs[ok])
}
