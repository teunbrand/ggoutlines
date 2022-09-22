#' Outlined line grob
#'
#' @inheritParams grid::polylineGrob
#' @param stroke_lwd Line width of the outline.
#' @param stroke_col Colour of the
#'
#' @return A [grid::grobTree] object
#' @export
#'
#' @examples
#' require(grid)
#' grid.newpage()
#' grid.draw(grob_outline_line(
#'   gp = gpar(col = "limegreen", lwd = 5),
#'   stroke_col = "forestgreen",
#'   stroke_lwd = 5
#' ))
grob_outline_line <- function(
  x = unit(c(0, 1), "npc"),
  y = unit(c(0, 1), "npc"),
  stroke_lwd = 1,
  stroke_col = "black",
  id = NULL,
  id.lengths = NULL,
  default.units = "npc",
  arrow = NULL,
  name = NULL,
  gp = gpar(),
  vp = NULL
) {
  # Make foreground
  fg <- polylineGrob(
    x = x, y = y, id = id, id.lengths = id.lengths,
    default.units = default.units, arrow = arrow, name = name,
    gp = gp
  )

  stroke_lwd <- stroke_lwd %||% 1
  stroke_col <- stroke_col %||% "black"

  # Early exit when no stroke needs to be drawn
  if (all(is.na(stroke_col) | stroke_col == 'transparent') ||
      all(stroke_lwd <= 0)) {
    return(fg)
  }

  # Make background
  bg <- polylineGrob(
    x = x, y = y, id = id, id.lengths = id.lengths,
    default.units = default.units, arrow = arrow, name = name,
    gp = update_gpar(
      gp,
      col = stroke_col,
      lwd = (gp$lwd %||% 1) + stroke_lwd * 2
    )
  )
  grobTree(bg, fg, vp = vp, name = name)
}
