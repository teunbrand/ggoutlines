# Main function -----------------------------------------------------------

#' Outlined points
#'
#' Like [`ggplot2::geom_point()`], this draws points in the plot that can be
#' used to make scatterplots. However, the points have an outline, that can
#' make it easier to discern points when their colour and background colour
#' are similar.
#'
#' @inheritParams ggplot2::geom_point
#' @param by_group A `logical(1)` for setting groupwise outlines. If `TRUE`,
#'   will draw outlines for groups of points. If `FALSE` (default), the
#'   point-cloud as a whole gets an outline.
#'
#' @return A `<LayerInstance>` object that can be added to a plot.
#' @export
#' @eval ggplot2:::rd_aesthetics("geom", "point_outline")
#'
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl)))
#'
#' # Draw points with outlines
#' p + geom_point_outline(size = 10)
#'
#' # Discern groups by drawing groupwise outlines
#' p + geom_point_outline(size = 10, by_group = TRUE)
geom_point_outline <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  by_group    = FALSE,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomPointOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      na.rm     = na.rm,
      by_group  = by_group,
      ...
    )
  )
}

# Key ---------------------------------------------------------------------

#' @export
#' @rdname ggoutlines-keys
draw_key_point_outline <- function(data, params, size) {
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- ggplot2:::translate_shape_string(data$shape)
  }
  stroke_size <- data$stroke %||% 0.5
  stroke_size[is.na(stroke_size)] <- 0

  grob_outline_points(
    0.5, 0.5,
    pch = data$shape,
    gp = gpar(
      col  = alpha(data$colour %||% "black", data$alpha),
      fill = alpha(data$fill %||% "black",   data$alpha),
      fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke/2,
      lwd = stroke_size * .stroke/2
    ),
    stroke_col = data$stroke_colour,
    stroke_lwd = data$stroke_linewidth * .stroke/2
  )
}


# Classes -----------------------------------------------------------------

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointOutline <- ggproto(
  "GeomPointOutline", GeomPoint,

  default_aes = aes(
    shape  = 19,
    colour = "white",
    size   = 1.5,
    fill   = NA,
    alpha  = NA,
    stroke = 0.5,
    stroke_colour = "black",
    stroke_linewidth = 1
  ),

  draw_panel = function(
    self, data, panel_params, coord,
    na.rm = FALSE, by_group = FALSE
  ) {
    if (is.character(data$shape)) {
      data$shape <- ggplot2:::translate_shape_string(data$shape)
    }

    coords <- coord$transform(data, panel_params)
    stroke <- coords$stroke
    stroke[is.na(stroke)] <- 0

    group <- if (by_group) coords$group else NULL
    fill <- alpha(coords$fill, coords$alpha)
    if (all(is.na(fill))) {
      fill <- NULL
    }

    grob_outline_points(
      x = coords$x,
      y = coords$y,
      pch = coords$shape,
      gp = gpar(
        col  = alpha(coords$colour, coords$alpha),
        fill = fill,
        fontsize = coords$size * .pt + stroke * .stroke/2,
        lwd  = coords$stroke * .stroke/2
      ),
      id = group,
      stroke_col = coords$stroke_colour,
      stroke_lwd = coords$stroke_linewidth * .stroke/2
    )
  },

  draw_key = draw_key_point_outline
)
