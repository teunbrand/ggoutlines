#' Outlined text grob
#'
#' Creates a graphical object consisting of outlined text.
#'
#' @inheritParams grid::textGrob
#' @param stroke_lwd Line width for the outline.
#' @param stroke_col Colour for the outline.
#' @param stroke_linejoin Line join style for the outline.
#' @param stroke_lty Line type for the outline.
#'
#' @return A [grid::gTree] object.
#' @export
#'
#' @examples
#' require(grid)
#' grid.newpage()
#' grid.draw(grob_outline_text(
#'   label = c("Hello there,", "General Kenobi"),
#'   y = c(0.66, 0.33),
#'   gp = gpar(col = c("white", "red"), fontsize = 32),
#'   stroke_col = c("grey50", "black"), stroke_lwd = 3
#' ))
grob_outline_text <- function(
  label,
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  stroke_lwd = 1,
  stroke_col = "black",
  stroke_linejoin = "round",
  stroke_lty = 1,
  super_id = NULL,
  just  = "centre",
  hjust = NULL,
  vjust = NULL,
  rot   = 0,
  check.overlap = FALSE,
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  vp = NULL
) {
  single <- length(label) == 1

  hjust <- resolveHJust(just, hjust)
  vjust <- resolveVJust(just, vjust)

  stroke_lwd <- stroke_lwd %||% 1
  stroke_col <- stroke_col %||% "black"

  no_stroke <- all(is.na(stroke_col) | stroke_col == "transparent") ||
    all(stroke_lwd <= 0)

  if (single || no_stroke) {
    fg <- textGrob(
      label = label, x = x, y = y, hjust = hjust, vjust = vjust,
      rot = rot, check.overlap = check.overlap, default.units = default.units,
      name = NULL, gp = gp, vp = NULL
    )
    # Try early exit when there is no stroke
    if (no_stroke) {
      return(fg)
    }
  } else {
    # Instead, make list of text grobs
    fg <- Map(textGrob, label = label, x = x, y = y, just = just, hjust = hjust,
              vjust = vjust, rot = rot, check.overlap = check.overlap,
              default.units = default.units,
              gp = split_gp(gp, seq_along(label)))
  }

  # Build stroke gpar
  stroke_gp <- gpar(
    col = stroke_col,
    lwd = stroke_lwd * 2,
    lty = stroke_lty %||% 1,
    linejoin = stroke_linejoin %||% "round"
  )

  # Early exit when only 1 text object is stroked
  if (single) {
    bg  <- strokeGrob(fg, gp = stroke_gp)
    if (any(get_alpha(gp$col) != 1)) {
      bg <- groupGrob(fg, op = "clear", bg)
    }
    out <- grobTree(bg, fg, vp = vp, name = name)
    return(out)
  }
  # Make list of strokes
  bg <- Map(strokeGrob, x = fg, gp = split_gp(stroke_gp, seq_along(fg)))

  if (any(get_alpha(gp$col) != 1)) {
    bg <- Map(groupGrob, src = fg, op = "clear", dst = bg)
  }

  # Alternate text and stroked text
  if (!is.null(super_id)) {
    grob <- unlist(vec_interleave(
      split(bg, super_id),
      split(fg, super_id)
    ), recursive = FALSE, FALSE)
  } else {
    grob <- vec_interleave(bg, fg)
  }

  gTree(children = do.call(gList, grob), vp = vp, name = name)
}


