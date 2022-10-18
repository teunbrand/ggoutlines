# Constructors ------------------------------------------------------------

#' Outlined polygons
#'
#' Like [`ggplot2::geom_polygon()`], this draws polygons in the plot. However,
#' the polygons have an outline, that can make it easier to discern groups.
#'
#' @inheritParams ggplot2::geom_polygon
#' @param by_group A `logical(1)` for setting groupwise outlines. If `TRUE`,
#'   every polygon gets a separate outline. If `FALSE` (default), the collection
#'   of polygons as a whole gets an outline.
#'
#' @return A `<LayerInstance>` object that can be added to a plot.
#' @export
#' @eval ggplot2:::rd_aesthetics("geom", "polygon_outline")
#'
#' @examples
#' # Setup some data
#' df <- data.frame(
#'   x = c(2, 6, 6, 2, 2.5, 5.5, 5.5, 2.5,
#'         8, 4, 4, 8, 7.5, 4.5, 4.5, 7.5),
#'   y = c(2, 2, 6, 6, 2.5, 2.5, 5.5, 5.5,
#'         8, 8, 4, 4, 7.5, 7.5, 4.5, 4.5),
#'   sub_id = rep(1:4, each = 4),
#'   id = rep(1:2, each = 8)
#' )
#'
#' # Setup plot
#' p <- ggplot(df, aes(x, y, group = id, subgroup = sub_id, fill = factor(id)))
#'
#' # Plot with polygon containing holes, sharing outlines
#' p + geom_polygon_outline()
#'
#' # Outline for every polygon
#' p + geom_polygon_outline(by_group = TRUE)
geom_polygon_outline <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  rule     = "evenodd",
  by_group = FALSE,
  ...,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomPolygonOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list2(
      na.rm     = na.rm,
      rule      = rule,
      by_group  = by_group,
      ...
    )
  )
}


# Classes -----------------------------------------------------------------

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPolygonOutline <- ggproto(
  "GeomPolygonOutline", GeomPolygon,

  default_aes = aes(
    colour    = NA,
    fill      = "grey20",
    linewidth = 0.5,
    linetype  = 1,
    alpha     = 1,
    subgruop  = NULL,
    stroke_colour = "black",
    stroke_linewidth = 1
  ),

  draw_panel = function(data, panel_params, coord,
                        rule = "evenodd", by_group = FALSE) {

    n <- nrow(data)
    if (n == 1) {
      return(zeroGrob())
    }
    munched <- coord_munch(coord, data, panel_params)
    if (is.null(munched$subgroup)) {
      munched <- munched[order(munched$group), ]
      first_idx <- !duplicated(munched$group)
      first_row <- munched[first_idx, ]

      group <- if (by_group) munched$group else rep(1, n)

      grob_outline_polygon(
        x = munched$x, y = munched$y,
        default.units = "native",
        id = munched$group,
        super_id = group,
        gp = gpar(
          col  = first_row$colour,
          fill = alpha(first_row$fill, first_row$alpha),
          lwd  = first_row$linewidth * .pt,
          lty  = first_row$linetype
        ),
        stroke_col = first_row$stroke_colour,
        stroke_lwd = first_row$stroke_linewidth * .pt
      )
    } else {

      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique(munched$subgroup))
      first_idx <- !duplicated(munched$group)
      first_row <- munched[first_idx, ]

      group <- if (by_group) munched$group else rep(1, n)
      grob_outline_polygon(
        x = munched$x, y = munched$y,
        default.units = "native",
        id = id,
        pathId = munched$group,
        super_id = group,
        rule = rule,
        gp = gpar(
          col  = first_row$colour,
          fill = alpha(first_row$fill, first_row$alpha),
          lwd  = first_row$linewidth * .pt,
          lty  = first_row$linetype
        ),
        stroke_col = first_row$stroke_colour,
        stroke_lwd = first_row$stroke_linewidth * .pt
      )
    }
  },

  draw_key = draw_key_rect_outline
)
