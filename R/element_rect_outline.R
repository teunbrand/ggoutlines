
#' Outlined rectangle theme element
#'
#' The element for drawing outlined rectangles. Can be given as some arguments
#' in the [`ggplot2::theme()`] function.
#'
#' @inheritParams ggplot2::element_rect
#' @param stroke_colour,stroke_color Outline colour. `stroke_color` is an alias
#'   for `stroke_colour`.
#' @param stroke_linewidth Outline size in millimetres.
#'
#' @return An `<element_rect_outline>` object.
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#'   geom_point() +
#'   theme(
#'     legend.key = element_rect_outline(
#'       colour = "green", stroke_color = "blue"
#'     )
#'   )
element_rect_outline <- function(
  fill      = NULL,
  colour    = NULL,
  linewidth = NULL,
  linetype  = NULL,
  color     = NULL,
  stroke_colour = NULL,
  stroke_color  = NULL,
  stroke_linewidth = NULL,
  inherit.blank = FALSE
) {
  colour <- colour %||% color
  arrow  <- arrow  %||% FALSE
  stroke_Colour <- stroke_colour %||% stroke_color

  structure(list(
    fill   = fill,
    colour = colour,
    linewidth = linewidth,
    linetype  = linetype,
    inherit.blank = inherit.blank
  ), class = c("element_rect_outline", "element_rect", "element"))
}

#' @export
element_grob.element_rect_outline <- function(
  element,
  x = 0.5,
  y = 0.5,
  width = 1,
  height = 1,
  fill = NULL,
  colour = NULL,
  linewidth = NULL,
  linetype = NULL,
  stroke_colour = NULL,
  stroke_linewidth = NULL,
  ...
) {
  gp <- gpar(
    lwd = protect0(linewidth * .pt), col = colour,
    fill = fill, lty = linetype
  )
  element_gp <- gpar(
    lwd = protect0(element$linewidth * .pt),
    col = element$colour, fill = element$fill,
    lty = element$linetype
  )
  gp <- modify_list(element_gp, gp)

  st_lwd <- stroke_linewidth %||% element$stroke_linewidth %||% (gp$lwd / .pt)
  st_col <- stroke_colour    %||% element$stroke_colour    %||% "black"

  grob_outline_rect(
    x = x, y = y,
    width = width, height = height,
    gp = gp,
    stroke_col = st_col,
    stroke_lwd = protect0(st_lwd * .pt)
  )
}
