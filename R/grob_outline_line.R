#' Outlined line grob
#'
#' Creates a graphical object consisting of outlined lines.
#'
#' @inheritParams grid::polylineGrob
#' @param stroke_lwd Line width of the outline.
#' @param stroke_col Colour of the outline.
#' @param super_id Identifiers of super-groups that share an outline.
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
  super_id = NULL,
  default.units = "npc",
  arrow = NULL,
  name = NULL,
  gp = gpar(),
  vp = NULL
) {

  id <- standardise_id(id, id.lengths, length(x))
  uid <- unique(id)
  suid <- unique(super_id)
  single <- length(uid) == 1 || length(suid) == 1

  stroke_lwd <- stroke_lwd %||% 1
  stroke_col <- stroke_col %||% "black"

  no_stroke <- all(is.na(stroke_col) | stroke_col == "transparent") ||
    all(stroke_lwd <= 0)

  # Make foreground
  if (single || no_stroke) {
    fg <- polylineGrob(
      x = x, y = y, id = id,
      default.units = default.units,
      arrow = arrow, gp = gp
    )
    # Early exit when no stroke needs to be drawn
    if (no_stroke) {
      return(fg)
    }
  } else {
    # Instead, make list of line grobs
    split_var <- if (is.null(super_id)) id else super_id
    split_var <- split_var[!duplicated(id)]
    fg <- Map(
      polylineGrob,
      x = split(x, super_id %||% id), y = split(y, super_id %||% id),
      id = split(id, super_id %||% id),
      default.units = default.units,
      arrow = split_arrow(arrow, suid %||% uid),
      gp    = split_gp(gp, split_var)
    )
  }

  # Build stroke gpar
  stroke_gp <- gpar(
    col = stroke_col,
    lwd = (gp$lwd %||% 1) + stroke_lwd * 2
  )

  # Make background
  if (single) {
    bg  <- editGrob(fg, gp = stroke_gp, name = paste0(fg$name, "_bg"))
    if (has_alpha(gp$col)) {
      bg <- groupGrob(fg, op = "clear", bg)
    }
    out <- grobTree(bg, fg, vp = vp, name = name)
    return(out)
  }
  names <- paste0(vapply(fg, `[[`, "", "name"), "_bg")

  bg <- Map(editGrob, grob = fg, gp = split_gp(stroke_gp, split_var),
            name = names)

  if (has_alpha(gp$col)) {
    bg <- Map(groupGrob, src = fg, op = "clear", dst = bg)
  }

  grob <- vec_interleave(bg, fg)

  gTree(children = do.call(gList, grob), vp = vp, name = name)
}
