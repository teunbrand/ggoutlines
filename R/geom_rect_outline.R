# Constructors ------------------------------------------------------------

#' Outlined rectangles
#'
#' Like [`ggplot2::geom_rect()`] and [`ggplot2::geom_tile()`], this draws
#' rectangles in the plot. However, the rectangles have an outline, that can
#' make it easier to discern groups.
#'
#' @inheritParams ggplot2::geom_rect
#' @param by_group A `logical(1)` for setting groupwise outlines. If `TRUE`,
#'   will draw outlines for individual groups. If `FALSE` (default), the
#'   collection of rectangles as a whole gets an outline.
#' @param radius A [grid::unit()] object of length 1 setting the radius for
#'   rounded corners.
#'
#' @return A `<LayerInstance>` object that can be added to a plot.
#' @export
#' @eval ggplot2:::rd_aesthetics("geom", "rect_outline")
#'
#' @examples
#' p <- ggplot(tetris, aes(x, y, fill = type)) +
#'   coord_equal()
#'
#' # Outline the collection of tiles
#' p + geom_tile_outline(colour = "black")
#'
#' # Outline groups of tiles
#' p + geom_tile_outline(by_group = TRUE, colour = "black")
#'
#' # Round corners of tiles
#' p + geom_tile_outline(radius = unit(2, "mm"), by_group = TRUE)
#'
#' # Rectangles
#' df <- data.frame(
#'   xmin = c(0, 4, 6, 8,  10, 0, 4, 6, 8,  10),
#'   xmax = c(4, 6, 8, 10, 14, 4, 6, 8, 10, 14),
#'   ymin = rep(1:2, each = 5),
#'   ymax = rep(2:3, each = 5),
#'   z = factor(rep(1:5, each = 2))
#' )
#'
#' ggplot(df, aes()) +
#'   geom_rect_outline(aes(fill = z), colour = "grey50")
#'
#' ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
#'   geom_rect_outline(aes(fill = z), colour = "grey50", by_group = TRUE)
geom_rect_outline <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  by_group = FALSE,
  ...,
  linejoin    = "mitre",
  radius      = NULL,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomRectOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      linejoin = linejoin,
      by_group = by_group,
      na.rm    = na.rm,
      radius   = radius,
      ...
    )
  )
}

#' @export
#' @rdname geom_rect_outline
#' @eval ggplot2:::rd_aesthetics("geom", "tile_outline")
geom_tile_outline <- function(
    mapping  = NULL,
    data     = NULL,
    stat     = "identity",
    position = "identity",
    by_group = FALSE,
    ...,
    linejoin    = "mitre",
    radius      = NULL,
    na.rm       = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomTileOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      linejoin = linejoin,
      by_group = by_group,
      na.rm    = na.rm,
      radius   = radius,
      ...
    )
  )
}

# Key ---------------------------------------------------------------------

#' @export
#' @rdname ggoutlines-keys
draw_key_rect_outline <- function(data, params, size) {

  data$linewidth <- data$linewidth %||% 0.5
  lwd  <- min(data$linewidth, min(size) / 4)
  slwd <- (data$stroke_linewidth %||% 1)
  r <- params$radius
  if (!is.null(r)) {
    r <- min(unit(0.5, "npc"), r)
  }

  grob_outline_rect(
    width  = unit(1, "npc") - unit(lwd + slwd, "mm"),
    height = unit(1, "npc") - unit(lwd + slwd, "mm"),
    gp = gpar(
      col  = data$colour %||% NA,
      fill = alpha(data$fill %||% "grey20", data$alpha),
      lwd  = lwd * .pt,
      linetype = data$linetype   %||% 1,
      linejoin = params$linejoin %||% "mitre",
      lineend  = params$lineend  %||% "butt"
    ),
    stroke_col = data$stroke_colour %||% "black",
    stroke_lwd = slwd * .pt,
    r = r
  )
}

# Classes -----------------------------------------------------------------

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectOutline <- ggproto(
  "GeomRectOutline", GeomRect,

  default_aes = aes(
    colour    = NA,
    fill      = "grey35",
    linewidth = 0.5,
    linetype  = 1,
    alpha     = NA,
    stroke_colour = "black",
    stroke_linewidth = 1
  ),

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", by_group = FALSE, radius = NULL) {

    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )
      polys <- data[rep(seq_len(nrow(data)), each = 5), , drop = FALSE]
      polys$x <- vec_interleave(
        data$xmin, data$xmax, data$xmax, data$xmin, data$xmin
      )
      polys$y <- vec_interleave(
        data$ymax, data$ymax, data$ymin, data$ymin, data$ymax
      )
      polys[, c("xmin", "xmax", "ymin", "ymax")] <- NULL
      polys$group <- rep(seq_len(nrow(data)), each = 5)

      grob <- GeomPolygonOutline$draw_panel(
        polys, panel_params, coord, lineend = lineend, linejoin = linejoin,
        by_group = by_group
      )
      return(grob)
    }

    coords <- coord$transform(data, panel_params)

    group <- if (by_group) coords$group else NULL

    grob_outline_rect(
      x      = coords$xmin,
      y      = coords$ymax,
      width  = coords$xmax - coords$xmin,
      height = coords$ymax - coords$ymin,
      default.units = "native",
      just   = c("left", "top"),
      id     = group,
      r      = radius,
      gp = gpar(
        col  = coords$colour,
        fill = alpha(coords$fill, coords$alpha),
        lwd  = coords$linewidth * .pt,
        lty  = coords$linetype,
        linejoin = linejoin,
        lineend  = lineend
      ),
      stroke_col = coords$stroke_colour,
      stroke_lwd = coords$stroke_linewidth * .pt
    )
  },

  draw_key = draw_key_rect_outline
)

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTileOutline <- ggproto(
  "GeomTileOutline", GeomRectOutline,

  extra_params = c("na.rm"),

  setup_data = function(data, params) {

    data$width  <- data$width  %||% params$width  %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(
      data,
      xmin = x - width  / 2, xmax = x + width  / 2, width  = NULL,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = aes(
    fill   = "grey20",
    colour = NA,
    linewidth = 0.1,
    linetype = 1,
    alpha = NA,
    width = NA,
    height = NA,
    stroke_linewidth = 1,
    stroke_colour = "black"
  ),

  required_aes = c("x", "y"),

  non_missing_aes = c("xmin", "xmax", "ymin", "ymax")
)

