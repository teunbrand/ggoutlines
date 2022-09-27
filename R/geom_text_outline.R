# Constructor -------------------------------------------------------------

#' Outlined text
#'
#' Like [`ggplot2::geom_text()`], this draws text in the plot. However, the
#' text has an outline, that can make it easier to discern when the text and
#' background colours are similar.
#'
#' @inheritParams ggplot2::geom_text
#' @param by_group A `logical(1)` for setting groupwise outlines. If `TRUE`,
#'   will draw outlines for groups of labels. If `FALSE` (default), individual
#'   labels get individual outlines.
#'
#' @return A `<LayerInstance>` object that can be added to a plot.
#' @export
#'
#' @eval ggplot2:::rd_aesthetics("geom", "text_outline")
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))
#'
#' # The text fill corresponds to the 'colour' aesthetic
#' p + geom_text_outline(aes(colour = factor(cyl)))
#'
#' # The text outline corresponds to the 'stroke_colour' aesthetic.
#' # Using this with a scale requires the scale to know about the aesthetic.
#' p + geom_text_outline(aes(stroke_colour = factor(cyl))) +
#'   scale_colour_hue(aesthetics = "stroke_colour")
#'
#' # The size of the outline is determined by the 'stroke_linewidth' aesthetic.
#' # There are no default scales for linewidth
#' p + geom_text_outline(aes(stroke_linewidth = cyl)) +
#'   continuous_scale("stroke_linewidth", "linewidth",
#'                    pal = scales::rescale_pal(c(0.5, 3)),
#'                    limits = c(0, NA))
geom_text_outline <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  parse    = FALSE,
  by_group = FALSE,
  nudge_x  = 0,
  nudge_y  = 0,
  na.rm    = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(paste0(
        "You must specify either {.arg position} or ",
        "{.arg nudge_x}/{.arg nudge_y}"
      ))
      position <- position_nudge(nudge_x, nudge_y)
    }
  }
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomTextOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse    = parse,
      na.rm    = na.rm,
      by_group = by_group,
      ...
    )
  )
}

# Key ---------------------------------------------------------------------

#' @export
#' @rdname ggoutlines-keys
draw_key_text_outline <- function(data, params, size) {
  if (is.null(data$label)) {
    data$label <- "a"
  }
  grob_outline_text(
    label = data$label,
    x = 0.5, y = 0.5,
    rot = data$angle %||% 0,
    gp = gpar(
      col = alpha(data$colour  %||% data$fill %||% "white", data$alpha),
      fontfamily = data$family %||% "",
      fontface   = data$fontface %||% 1,
      fontsize   = (data$size %||% 3.88) * .pt
    ),
    stroke_col = data$stroke_colour %||% "black",
    stroke_lwd = data$stroke_linewidth %||% 1
  )
}

# Classes -----------------------------------------------------------------

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextOutline <- ggproto(
  "GeomTextOutline", GeomText,

  default_aes = aes(
    colour   = "white",
    size     = 3.88,
    angle    = 0,
    hjust    = 0.5,
    vjust    = 0.5,
    alpha    = NA,
    family   = NA,
    fontface = 1,
    lineheight = 1.2,
    stroke_colour = "black",
    stroke_linewidth = 1
  ),

  draw_panel = function(
    data, panel_params, coord,
    parse = FALSE, na.rm = FALSE, by_group = TRUE
  ) {
    lab <- data$label
    if (parse) {
      lab <- ggplot2:::parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- ggplot2:::compute_just(data$vjust, data$y, data$x, data$angle)
    }
    if (is.character(data$hjust)) {
      data$hjust <- ggplot2:::compute_just(data$hjust, data$x, data$y, data$angle)
    }
    super_id <- if (by_group) match(data$group, unique0(data$group)) else NULL

    grob_outline_text(
      label = lab,
      x = data$x,
      y = data$y,
      default.units = "native",
      hjust = data$hjust,
      vjust = data$vjust,
      rot   = data$angle,
      gp    = gpar(
        col        = alpha(data$colour, data$alpha),
        fontsize   = data$size * .pt,
        fontfamliy = data$family,
        fontface   = data$fontface,
        lineheight = data$lineheight
      ),
      super_id   = super_id,
      stroke_col = data$stroke_colour,
      stroke_lwd = data$stroke_linewidth
    )
  },

  draw_key = draw_key_text_outline
)
