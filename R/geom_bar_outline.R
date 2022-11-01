# Main function -----------------------------------------------------------

#' Outlined bar charts
#'
#' Like [`ggplot2::geom_bar()`] and [`ggplot2::geom_col()`], this draws a
#' barchart in the plot. However, the bars have an outline, that can make it
#' easier to discern groups.
#'
#' @inheritParams ggplot2::geom_bar
#' @param radius A [grid::unit()] object of length 1 setting the radius for
#'   rounded corners.
#' @param by_group A `logical(1)` for setting groupwise outlines. If `TRUE`
#'   (default), will draw outlines for groups. If `FALSE`, the collection of
#'   bars as a whole get an outline.
#'
#' @return A `<LayerInstance>` object that can be added to a plot.
#' @export
#' @eval ggplot2:::rd_aesthetics("geom", "bar_outline")
#'
#' @examples
#' # Bar doesn't need y-aesthetic
#' ggplot(mpg, aes(class, fill = factor(cyl))) +
#'   geom_bar_outline()
#'
#' # Groups sharing outlines
#' ggplot(mpg, aes(class, fill = factor(cyl))) +
#'   geom_bar_outline(by_group = FALSE)
#'
#' # Setup data
#' df <- data.frame(
#'   trt = c("a", "a", "b", "b", "c", "c"),
#'   outcome = c(2.3, 1.9, 3.2, 2.8, 2.0, 3.5),
#'   group = rep(c("A", "B"), 3)
#' )
#'
#' # Dodged barchart sharing outlines
#' ggplot(df, aes(trt, outcome, fill = group)) +
#'   geom_col_outline(
#'     position = "dodge",
#'     by_group = FALSE
#'   )
#'
#' # Histogram with outline
#' ggplot(iris, aes(Sepal.Length)) +
#'   geom_histogram_outline()
#'
#' # Histogram with outline indicating totals
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_histogram_outline(by_group = FALSE)
geom_bar_outline <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "count",
  position = "stack",
  ...,
  just     = 0.5,
  radius   = NULL,
  by_group = TRUE,
  width    = NULL,
  na.rm    = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomBarOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      just  = just,
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      by_group = by_group,
      radius   = radius,
      ...
    )
  )
}

#' @export
#' @rdname geom_bar_outline
geom_col_outline <- function(..., stat = "identity") {
  geom_bar_outline(..., stat = stat)
}

#' @export
#' @rdname geom_bar_outline
geom_histogram_outline <- function(..., stat = "bin") {
  geom_bar_outline(..., stat = stat)
}

# Classes -----------------------------------------------------------------

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_rect_outline.R
GeomBarOutline <- ggproto(
  "GeomBarOutline", GeomRectOutline,

  required_aes = c("x", "y"),

  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  extra_params = c("just", "na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||% params$width %||%
      (resolution(data$x, FALSE) * 0.9)
    data$just <- params$just %||% 0.5
    data <- transform(
      data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = x - width * (1 - just), xmax = x + width * just,
      width = NULL, just = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", width = NULL, flipped_aes = FALSE,
                        radius = NULL, by_group = TRUE) {
    ggproto_parent(GeomRectOutline, self)$draw_panel(
      data,
      panel_params,
      coord,
      lineend  = lineend,
      linejoin = linejoin,
      radius   = radius,
      by_group = by_group
    )
  }
)
