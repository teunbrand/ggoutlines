#' Outlined points grob
#'
#' Creates a graphical object consisting of outlined points.
#'
#' @inheritParams grid::pointsGrob
#' @param id Optional grouping identifier.
#' @param stroke_col Colour for the outline.
#' @param stroke_lwd Line width for the outline.
#'
#' @return A [grid::grobTree] object.
#' @export
#'
#' @examples
#' require(grid)
#' grid.newpage()
#' grid.draw(grob_outline_points(
#'   x = rep(seq(0.1, 0.9, length.out = 5), 5),
#'   y = rep(seq(0.9, 0.1, length.out = 5), each = 5),
#'   gp = gpar(col = "red", fill = "grey50"),
#'   default.units = "npc",
#'   pch = 1:25,
#'   stroke_lwd = 1,
#'   stroke_col = "blue"
#' ))
grob_outline_points <- function(
  x = stats::runif(10),
  y = stats::runif(10),
  pch  = 1,
  id = NULL,
  stroke_col = "black",
  stroke_lwd = 1,
  default.units = "native",
  name = NULL,
  gp = gpar(),
  vp = NULL
) {

  n <- length(x)
  pch <- rep_len(pch, n)
  is_solid <- pch > 14
  has_fill <- pch > 20

  lwd_orig <- rep_len(gp$lwd %||% 1, n)

  stroke_lwd <- rep_len(stroke_lwd %||% 1, n)
  stroke_col <- rep_len(stroke_col %||% "black", n)

  no_stroke <- all(is.na(stroke_col) | stroke_col == "transparent") ||
    all(stroke_lwd <= 0)
  single <- length(unique(id)) <= 2

  if (single || no_stroke) {
    fg <- pointsGrob(
      x = x, y = y,
      pch = pch,
      gp = update_gpar(gp, lwd = lwd_orig),
      default.units = default.units
    )
    # Try early exit when there is no stroke
    if (no_stroke) {
      fg <- editGrob(fg, name = name, vp = vp)
      return(fg)
    }
  } else {
    # Instead make a list of points
    sgp <- split_gp(gp, id)
    sgp <- Map(update_gpar, gp = sgp, lwd = split(lwd_orig, id))
    fg <- Map(
      pointsGrob,
      x   = split(x, id),
      y   = split(y, id),
      pch = split(pch, id),
      default.units = default.units,
      gp  = sgp
    )
  }

  # Set outline graphical parameters
  fontsize <- rep_len(gp$fontsize %||% 12, n)
  fontsize <- fontsize + stroke_lwd * .pch_size_mult[pch]
  stroke_lwd <- ifelse(pch > 18, ifelse(has_fill, 1, 0), 2) * stroke_lwd

  # Early exit when no grouping needs to be applied
  if (single) {
    bg <- editGrob(
      fg, name = paste0(fg$name, "_bg"),
      gp = update_gpar(
        gp  = gp,
        lwd = lwd_orig + stroke_lwd,
        col = stroke_col,
        fontsize = fontsize
      )
    )
    bg  <- clear_grob(bg, fg, gp$col, gp$fill)
    out <- grobTree(bg, fg, vp = vp, name = name)
    return(out)
  }

  # Make list of strokes
  names <- paste0(vapply(fg, `[[`, "", "name"), "_bg")
  bg_gp <- Map(
    update_gpar,
    gp = sgp,
    lwd = split(lwd_orig + stroke_lwd, id),
    col = split(stroke_col, id),
    fontsize = split(fontsize, id)
  )
  bg <- Map(editGrob, grob = fg, gp = bg_gp, name = names)
  bg <- clear_grob(bg, fg, gp$col, gp$fill)

  grob <- vec_interleave(bg, fg)
  gTree(children = do.call(gList, grob), vp = vp, name = name)
}

.pch_size_mult <- c(
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 2,
  2, 2.574, 2 * sqrt(2), 2, 3,
  1, 1, 1, 1, 1
)

.pch_lwd_mult <- c(
  2, 2, 2, 2, 2,
  2, 2, 2, 2, 2,
  2, 2, 2, 2, 2,
  2, 2, 2, 0, 0,
  1, 1, 1, 1, 1
)
