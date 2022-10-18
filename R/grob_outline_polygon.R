#' Outlined polygon grob
#'
#' Creates a graphical object consisting of outlined polygons.
#'
#' @inheritParams grid::pathGrob
#' @param super_id Identifiers of super-groups that share an outline
#' @param stroke_col Colour of the outline
#' @param stroke_lwd Line width of the outline
#'
#' @return A [grid::grobTree] object
#' @export
#'
#' @examples
#' require(grid)
#' grid.newpage()
#' grid.draw(grob_outline_polygon(
#'   x = c(0.2, 0.6, 0.6, 0.2, 0.25, 0.55, 0.55, 0.25,
#'         0.8, 0.4, 0.4, 0.8, 0.75, 0.45, 0.45, 0.75),
#'   y = c(0.2, 0.2, 0.6, 0.6, 0.25, 0.25, 0.55, 0.55,
#'         0.8, 0.8, 0.4, 0.4, 0.75, 0.75, 0.45, 0.45),
#'   id = rep(1:4, each = 4),
#'   pathId = rep(1:2, each = 8),
#'   gp = gpar(fill = c("dodgerblue", "tomato"), lwd = 2, col = "black"),
#'   stroke_col = "white",
#'   stroke_lwd = 5,
#'   rule = "evenodd"
#' ))
grob_outline_polygon <- function(
  x,
  y,
  id = NULL,
  id.lengths = NULL,
  pathId = NULL,
  pathId.lengths = NULL,
  super_id = NULL,
  rule = "winding",
  stroke_col = "black",
  stroke_lwd = 1,
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  vp = NULL
) {

  n  <- length(x)
  id <- standardise_id(id, id.lengths, n)
  pathId <- standardise_id(pathId, pathId.lengths, n)

  uid <- unique(id)
  suid <- unique(super_id)
  single <- length(uid) == 1 || length(suid) == 1

  stroke_lwd <- stroke_lwd %||% 1
  stroke_col <- stroke_col %||% "black"

  no_stroke <- all(is.na(stroke_col) | stroke_col == "transparent") ||
    all(stroke_lwd <= 0)

  # Make foreground
  if (single || no_stroke) {
    fg <- pathGrob(
      x = x, y = y, id = id, pathId = pathId,
      rule = rule, default.units = default.units,
      gp = gp
    )
    if (no_stroke) {
      fg <- editGrob(fg, vp = vp, name = name)
      return(fg)
    }
  } else {
    split_var <- if (is.null(super_id)) pathId else super_id
    split_var <- split_var[!duplicated(pathId)]
    split_id <- super_id %||% pathId
    fg <- Map(
      pathGrob,
      x  = split(x, split_id),
      y  = split(y, split_id),
      id = split(id, split_id),
      pathId = split(pathId, split_id),
      default.units = default.units,
      rule = rule,
      gp = split_gp(gp, split_var)
    )
  }

  stroke_gp <- gpar(
    col = stroke_col,
    lwd = (gp$lwd %||% 1) + stroke_lwd * 2
  )

  if (single) {
    bg <- editGrob(fg, gp = stroke_gp, name = paste0(fg$name, "_bg"))
    bg <- clear_grob(bg = bg, fg = fg, col = gp$col, fill = gp$fill)
    out <- grobTree(bg, fg, vp = vp, name = name)
    return(out)
  }
  names <- paste0(vapply(fg, `[[`, "", "name"), "_bg")

  bg <- Map(editGrob, grob = fg, gp = split_gp(stroke_gp, split_var),
            name = names)
  bg <- clear_grob(bg = bg, fg = fg, col = gp$col, fill = gp$fill)

  grob <- vec_interleave(bg, fg)

  gTree(children = do.call(gList, grob), vp = vp, name = name)
}
