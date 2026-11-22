# Renderer: layout -> ggplot ----------------------------------------------

#' Draw a flow diagram
#'
#' `autoplot()` returns a ggplot object you can restyle or save; `plot()`
#' draws it and returns it invisibly. The plot carries a plain-language
#' description (from [flow_describe()]) as alt text.
#'
#' The diagram is laid out in inches at its natural size, stored in the
#' plot's `"dataflow_size"` attribute. [flow_save()] uses that size by default,
#' so text and boxes stay matched. Drawing it much smaller than its natural
#' size can make text spill out of the boxes.
#'
#' @param object,x A `dataflow` object.
#' @param ... Passed to [flow_layout()] (e.g. `count_format`, `base_size`,
#'   `hgap`, `vgap`, `connector`).
#' @param fill,colour Box fill and border colour.
#' @param outcome_fill Fill for outcome boxes at the end of each branch.
#' @param linewidth Line width for boxes and connectors.
#' @param row_labels Draw stage names down the left margin?
#' @return A ggplot object.
#' @method autoplot dataflow
#' @export
#' @examples
#' d <- flow_start("Cultures", n = 100) |>
#'   flow_split("Experiment ID", c("Ex. 1", "Ex. 2"), n = 50)
#' p <- ggplot2::autoplot(d)
autoplot.dataflow <- function(object, ..., fill = "white", colour = "black",
                              outcome_fill = "#EDEDED", linewidth = 0.4,
                              row_labels = TRUE) {
  lay <- flow_layout(object, ...)
  bx <- lay$boxes
  bx$fill <- ifelse(bx$type == "outcome", outcome_fill, fill)
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
    ggplot2::geom_rect(
      data = bx,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                   ymax = .data$ymax),
      fill = bx$fill, colour = colour, linewidth = linewidth
    ) +
    ggplot2::geom_text(
      data = bx,
      ggplot2::aes(x = .data$x, y = .data$text_y, label = .data$text),
      size = ps$base_size, size.unit = "pt", family = ps$family,
      lineheight = 1.2, colour = colour
    )
  sb <- lay$side
  if (nrow(sb) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = sb,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                     ymax = .data$ymax),
        fill = fill, colour = colour, linewidth = linewidth
      ) +
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

#' @rdname autoplot.dataflow
#' @method plot dataflow
#' @export
plot.dataflow <- function(x, ...) {
  p <- autoplot.dataflow(x, ...)
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
theme_dataflow <- function(base_size = 9, base_family = "") {
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
#' @param x A `dataflow` object or a plot made by [autoplot.dataflow()].
#' @param filename File to write; the extension picks the format.
#' @param width,height Size in inches. Defaults to the natural size.
#' @param dpi Resolution for bitmap formats.
#' @param bg Background colour.
#' @param device Optional device passed to [ggplot2::ggsave()]; by default
#'   it is chosen from the file extension.
#' @param ... Passed to [autoplot.dataflow()] when `x` is a dataflow,
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
  if (inherits(x, "dataflow")) {
    p <- autoplot.dataflow(x, ...)
    dots <- list()
  } else if (inherits(x, "ggplot")) {
    p <- x
    dots <- list(...)
  } else {
    stop("`x` must be a dataflow or a plot made from one.", call. = FALSE)
  }
  size <- attr(p, "dataflow_size")
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
