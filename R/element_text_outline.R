
#' Outlined text theme element
#'
#' The element for drawing outlined text. Can be given as some arguments in the
#' [`ggplot2::theme()`] function.
#'
#' @inheritParams ggplot2::element_text
#' @param stroke_colour,stroke_color Outline colour. `stroke_color` is an alias
#'   for `stroke_colour`.
#' @param stroke_linewidth Outline size in millimetres.
#'
#' @return An <element_text_outline> object.
#' @export
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(
#'     axis.title.x = element_text_outline(
#'       colour = "white",
#'       stroke_linewidth = 0.5,
#'       stroke_colour = "red"
#'     )
#'   )
element_text_outline <- function(
  family = NULL,
  face   = NULL,
  colour = NULL,
  size   = NULL,
  hjust  = NULL,
  vjust  = NULL,
  angle  = NULL,
  lineheight = NULL,
  color  = NULL,
  margin = NULL,
  inherit.blank = FALSE,
  stroke_colour = NULL,
  stroke_color  = NULL,
  stroke_linewidth = NULL
) {
  colour <- colour %||% color
  stroke_colour <- stroke_colour %||% stroke_color

  n <- max(length(family), length(face), length(colour), length(size),
           length(hjust), length(vjust), length(angle), length(lineheight),
           length(stroke_colour), length(stroke_linewidth))
  if (n > 1) {
    cli::cli_warn(paste0(
      "Vectorised input to {.fn element_text_outline} is not officially ",
      "supported. Results may be unexpected or subject to change in future ",
      "versions of {.pkg ggoutline}/{.pkg ggplot2}."
    ))
  }

  structure(list(
    family = family,
    face   = face,
    colour = colour,
    size   = size,
    hjust  = hjust,
    vjust  = hjust,
    angle  = angle,
    lineheight = lineheight,
    margin = margin,
    debug  = debug,
    inherit.blank = inherit.blank,
    stroke_colour = stroke_colour,
    stroke_linewidth = stroke_linewidth
  ), class = c("element_text_outline", "element_text", "element"))
}

#' @export
element_grob.element_text_outline <- function(
  element,
  label = "",
  x = NULL,
  y = NULL,
  family = NULL,
  face   = NULL,
  colour = NULL,
  size   = NULL,
  hjust  = NULL,
  vjust  = NULL,
  angle  = NULL,
  lineheight = NULL,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  stroke_colour = NULL,
  stroke_linewidth = NULL,
  ...
) {
  if (is.null(label))
    return(zeroGrob())
  vjust  <- vjust  %||% element$vjust
  hjust  <- hjust  %||% element$hjust
  margin <- margin %||% element$margin
  angle  <- angle  %||% element$angle %||% 0
  gp <- gpar(
    fontisze   = size,
    col        = colour,
    fontfamily = family,
    fontface   = face,
    lineheight = lineheight
  )
  element_gp <- gpar(
    fontsize   = element$size,
    col        = element$colour,
    fontfamily = element$family,
    fontface   = element$face,
    lineheight = element$lineheight
  )
  gp <- modify_list(element_gp, gp)

  just <- ggplot2:::rotate_just(angle, hjust, vjust)
  n    <- max(length(x), length(y), 1)
  x    <- x %||% unit(rep(just$hjust, n), "npc")
  y    <- y %||% unit(rep(just$vjust, n), "npc")

  st_lwd <- stroke_linewidth %||% element$stroke_linewidth %||% (1 / .pt)
  st_lwd <- protect0(st_lwd * .pt)
  st_col <- stroke_colour    %||% element$stroke_colour    %||% "black"

  text_grob <- grob_outline_text(
    label = label, x = x, y = y, hjust = hjust, vjust = vjust,
    rot = angle, gp = gp,
    stroke_lwd = st_lwd, stroke_col = st_col
  )

  descent <- ggplot2:::font_descent(gp$fontfamily, gp$fontface, gp$fontsize,
                                    gp$cex)

  text_height <- unit(1, "grobheight", text_grob) +
    abs(cos(angle[1] / 180) * pi) * descent + unit(2 * st_lwd, "pt")

  text_width <- unit(1, "grobwidth", text_grob) +
    abs(sin(angle[1] / 180) * pi) * descent + unit(2 * st_lwd, "pt")

  ggplot2:::add_margins(
    grob = gList(text_grob),
    height = text_height,
    width  = text_width,
    gp = gp, margin = margin,
    margin_x = margin_x, margin_y = margin_y
  )
}
