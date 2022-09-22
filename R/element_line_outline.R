
#' Outlined line theme element
#'
#' Theme element for drawing outlined lines. Can be given as some arguments in
#' the [`ggplot2::theme()`] function.
#'
#' @inheritParams ggplot2::element_line
#' @param stroke_colour,stroke_color Outline colour. `stroke_color` is an alias
#'   for `stroke_colour`.
#' @param stroke_linewidth Outline size in millimetres.
#'
#' @return An <element_line_outline> object.
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(
#'     axis.line.x = element_line_outline(
#'       "white", linewidth = 1,
#'       stroke_colour = "black", stroke_linewidth = 0.5
#'     )
#'   )
element_line_outline <- function(
  colour    = NULL,
  linewidth = NULL,
  linetype  = NULL,
  lineend   = NULL,
  color     = NULL,
  arrow     = NULL,
  inherit.blank = FALSE,
  stroke_colour = NULL,
  stroke_color  = NULL,
  stroke_linewidth = NULL
) {
  colour <- colour %||% color
  arrow  <- arrow  %||% FALSE
  stroke_colour <- stroke_colour %||% stroke_color

  structure(list(
    colour    = colour,
    linewidth = linewidth,
    linetype  = linetype,
    lineend   = lineend,
    arrow     = arrow,
    stroke_colour = stroke_colour,
    stroke_linewidth = stroke_linewidth,
    inherit.blank = inherit.blank
  ), class = c("element_line_outline", "element_line", "element"))
}

#' @export
element_grob.element_line_outline <- function(
  element,
  x = 0:1,
  y = 0:1,
  colour    = NULL,
  linewidth = NULL,
  linetype  = NULL,
  lineend   = NULL,
  default.units = "npc",
  id.lengths = NULL,
  stroke_colour = NULL,
  stroke_linewidth = NULL,
  ...
) {
  gp <- gpar(
    col = colour, fill = colour, lwd = protect0(linewidth * .pt),
    lty = linetype, lineend = lineend
  )
  element_gp <- gpar(
    col = element$colour %||% "white", fill = element$colour %||% "White",
    lwd = protect0((element$linewidth %||% element$size) * .pt),
    lty = element$linetype, lineend = element$lineend
  )
  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  } else {
    element$arrow
  }
  gp <- modify_list(element_gp, gp)

  st_lwd <- stroke_linewidth %||% element$stroke_linewidth %||% (gp$lwd / .pt)
  grob_outline_line(
    x = x, y = y,
    default.units = default.units,
    gp = gp,
    id.lengths = id.lengths,
    arrow = arrow,
    stroke_lwd = protect0(st_lwd * .pt),
    stroke_col = stroke_colour %||% element$stroke_colour
  )
}
