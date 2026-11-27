# Renderer: layout -> ggplot ----------------------------------------------

#' Draw a flow diagram
#'
#' `autoplot()` returns a ggplot object you can restyle or save; `plot()`
#' draws it and returns it invisibly. The plot carries a plain-language
#' description (from [flow_describe()]) as alt text.
#'
#' The diagram is laid out in inches at its natural size, stored in the
#' plot's `"studydesignflow_size"` attribute. [flow_save()] uses that size by default,
#' so text and boxes stay matched. Drawing it much smaller than its natural
#' size can make text spill out of the boxes.
#'
#' @param object,x A `studydesignflow` object.
#' @param ... Passed to [flow_layout()] (e.g. `count_format`, `base_size`,
#'   `hgap`, `vgap`, `connector`, `show_method`, `show_details`,
#'   `show_kind_key`).
#' @param fill,colour Box fill and border colour.
#' @param outcome_fill Fill for outcome boxes at the end of each branch.
#' @param linewidth Line width for boxes and connectors.
#' @param row_labels Draw stage names down the left margin? The markers of
#'   step details (see [step_details()]) are drawn either way, so that every
#'   entry of the key has its marker.
#' @return A ggplot object.
#' @method autoplot studydesignflow
#' @export
#' @examples
#' d <- flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50)
#' p <- ggplot2::autoplot(d)
autoplot.studydesignflow <- function(object, ..., fill = "white", colour = "black",
                              outcome_fill = "#EDEDED", linewidth = 0.4,
                              row_labels = TRUE) {
  lay <- flow_layout(object, ...)
  left_na <- missing_outcome_left(object)
  if (left_na > 0) {
    warning(left_na, " row", if (left_na != 1) "s" else "", " in the end ",
            "boxes ", if (left_na != 1) "have" else "has", " a missing ",
            "outcome (`events` gives NA): counted in n, but neither as ",
            "events nor as rows without the event.", call. = FALSE)
  }
  bx <- lay$boxes
  bx$fill <- ifelse(bx$type == "outcome", outcome_fill, fill)
  # kinds of step (R/kind.R): boxes of a step done to data have a dashed
  # edge; without kinds the boxes are drawn exactly as before
  edge <- function(k) ifelse(!is.na(k) & k == "data", "dashed", "solid")
  rect_layer <- function(data, fill) {
    xy <- ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                       ymin = .data$ymin, ymax = .data$ymax)
    if (!"kind" %in% names(data)) {
      ggplot2::geom_rect(data = data, xy, fill = fill, colour = colour,
                         linewidth = linewidth)
    } else {
      ggplot2::geom_rect(data = data, xy, fill = fill, colour = colour,
                         linewidth = linewidth, linetype = edge(data$kind))
    }
  }
  sg <- lay$segments
  ex <- lay$extent
  ps <- lay$params
  head <- grid::arrow(length = grid::unit(0.065, "inches"), angle = 22,
                      type = "closed")

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = sg[!sg$head, ],
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                   yend = .data$yend),
      linewidth = linewidth, colour = colour, lineend = "square"
    ) +
    ggplot2::geom_segment(
      data = sg[sg$head, ],
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                   yend = .data$yend),
      linewidth = linewidth, colour = colour, arrow = head,
      arrow.fill = colour, linejoin = "mitre"
    ) +
    ggplot2::geom_path(
      data = lay$brackets,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$group),
      linewidth = linewidth, colour = colour
    ) +
    rect_layer(bx, bx$fill) +
    ggplot2::geom_text(
      data = bx,
      ggplot2::aes(x = .data$x, y = .data$text_y, label = .data$text),
      size = ps$base_size, size.unit = "pt", family = ps$family,
      lineheight = 1.2, colour = colour
    )
  sb <- lay$side
  if (nrow(sb) > 0) {
    p <- p +
      rect_layer(sb, fill) +
      ggplot2::geom_text(
        data = sb,
        ggplot2::aes(x = .data$text_x, y = .data$text_y, label = .data$text),
        size = ps$base_size, size.unit = "pt", family = ps$family,
        hjust = 0, lineheight = 1.2, colour = colour
      )
    side_notes <- sb[!is.na(sb$note), ]
    if (nrow(side_notes) > 0) {
      p <- p + ggplot2::geom_text(
        data = side_notes,
        ggplot2::aes(x = .data$text_x, y = .data$note_y, label = .data$note),
        size = ps$note_size, size.unit = "pt", family = ps$family,
        fontface = "italic", hjust = 0, lineheight = 1.2, colour = colour
      )
    }
  }
  notes <- bx[!is.na(bx$note), ]
  if (nrow(notes) > 0) {
    p <- p + ggplot2::geom_text(
      data = notes,
      ggplot2::aes(x = .data$x, y = .data$note_y, label = .data$note),
      size = ps$note_size, size.unit = "pt", family = ps$family,
      fontface = "italic", lineheight = 1.2, colour = colour
    )
  }
  if (row_labels && nrow(lay$row_labels) > 0) {
    p <- p + ggplot2::geom_text(
      data = lay$row_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = ps$base_size, size.unit = "pt", family = ps$family,
      fontface = "bold", hjust = 0, lineheight = 1.2, colour = colour
    )
    methods <- lay$row_labels[!is.na(lay$row_labels$method %||% NA), ]
    if (nrow(methods) > 0) {
      p <- p + ggplot2::geom_text(
        data = methods,
        ggplot2::aes(x = .data$x, y = .data$method_y, label = .data$method),
        size = ps$note_size, size.unit = "pt", family = ps$family,
        fontface = "italic", hjust = 0, lineheight = 1.2, colour = colour
      )
    }
  }
  # step details (show_details in flow_layout()): the markers of steps, in
  # the row-label column, and the key under the diagram
  marks <- lay$row_labels[!is.na(lay$row_labels$detail %||% NA), ]
  if (nrow(marks) > 0) {
    p <- p + ggplot2::geom_text(
      data = marks,
      ggplot2::aes(x = .data$x, y = .data$detail_y, label = .data$detail),
      size = ps$note_size, size.unit = "pt", family = ps$family,
      hjust = 0, colour = colour
    )
  }
  # the key of kinds of step: a sample box with a solid or dashed edge, then
  # its meaning in words
  kk <- lay$kind_key
  if (!is.null(kk)) {
    p <- p +
      ggplot2::geom_rect(
        data = kk,
        ggplot2::aes(xmin = .data$sxmin, xmax = .data$sxmax,
                     ymin = .data$symin, ymax = .data$symax),
        fill = fill, colour = colour, linewidth = linewidth,
        linetype = kk$linetype
      ) +
      ggplot2::geom_text(
        data = kk,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
        size = kk$size, size.unit = "pt", family = ps$family,
        hjust = 0, colour = colour
      )
  }
  if (!is.null(lay$key)) {
    p <- p + ggplot2::geom_text(
      data = lay$key,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
      size = lay$key$size, size.unit = "pt", family = ps$family,
      fontface = lay$key$face, hjust = 0, colour = colour
    )
  }
  p <- p +
    ggplot2::coord_fixed(xlim = ex[c("xmin", "xmax")],
                         ylim = ex[c("ymin", "ymax")],
                         expand = FALSE, clip = "off") +
    ggplot2::labs(alt = unclass(flow_describe(object))) +
    theme_studydesignflow(base_size = ps$base_size, base_family = ps$family)
  attr(p, "studydesignflow_size") <- lay$size
  p
}

#' @rdname autoplot.studydesignflow
#' @method plot studydesignflow
#' @export
plot.studydesignflow <- function(x, ...) {
  p <- autoplot.studydesignflow(x, ...)
  print(p)
  invisible(p)
}

#' A plain theme for flow diagrams
#'
#' White background, no axes or grid, no outer margin (the layout includes
#' its own). Add it to any ggplot.
#'
#' @param base_size Base font size, in points.
#' @param base_family Base font family.
#' @return A ggplot2 theme.
#' @export
theme_studydesignflow <- function(base_size = 9, base_family = "") {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.position = "none"
    )
}

#' Save a flow diagram to PDF, PNG or another format
#'
#' A thin wrapper on [ggplot2::ggsave()] that uses the diagram's natural size
#' unless you give one.
#'
#' @param x A `studydesignflow` object, a fold grid from [flow_folds()], or a plot
#'   made from either.
#' @param filename File to write; the extension picks the format.
#' @param width,height Size in inches. Defaults to the natural size.
#' @param dpi Resolution for bitmap formats.
#' @param bg Background colour.
#' @param device Optional device passed to [ggplot2::ggsave()]; by default
#'   it is chosen from the file extension.
#' @param ... Passed to [autoplot.studydesignflow()] (or
#'   [autoplot.studydesignflow_grid()]) when `x` is a studydesignflow (or fold grid),
#'   otherwise to [ggplot2::ggsave()].
#' @return `filename`, invisibly.
#' @export
#' @examples
#' d <- flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50)
#' f <- tempfile(fileext = ".png")
#' flow_save(d, f)
flow_save <- function(x, filename, width = NULL, height = NULL, dpi = 300,
                      bg = "white", device = NULL, ...) {
  if (inherits(x, "studydesignflow")) {
    p <- autoplot.studydesignflow(x, ...)
    dots <- list()
  } else if (inherits(x, "studydesignflow_grid")) {
    p <- autoplot.studydesignflow_grid(x, ...)
    dots <- list()
  } else if (inherits(x, "ggplot")) {
    p <- x
    dots <- list(...)
  } else {
    stop("`x` must be a studydesignflow, a fold grid, or a plot made from one.",
         call. = FALSE)
  }
  size <- attr(p, "studydesignflow_size")
  width <- width %||% size[["width"]]
  height <- height %||% size[["height"]]
  if (is.null(width) || is.null(height)) {
    stop("Give `width` and `height`; this plot has no natural size.",
         call. = FALSE)
  }
  do.call(ggplot2::ggsave, c(list(filename = filename, plot = p,
                                  width = width, height = height,
                                  units = "in", dpi = dpi, bg = bg,
                                  device = device),
                             dots))
  invisible(filename)
}
