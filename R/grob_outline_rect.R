#' Outlined rectangle grob
#'
#' @inheritParams grid::rectGrob
#' @param id Optional grouping identifier.
#' @param stroke_col Colour for the outline.
#' @param stroke_lwd Line width for the outline.
#' @param r The radius of optional rounded corners.
#'
#' @return A [grid::gTree] object.
#' @export
#'
#' @examples
#' grid.newpage()
#' grid.draw(grob_outline_rect(
#'   x = c(0.33, 0.66),
#'   y = c(0.33, 0.66),
#'   width = 0.5, height = 0.5,
#'   stroke_lwd = 5,
#'   stroke_col = 3:2
#' ))
grob_outline_rect <- function(
  x = unit(0.5, "npc"),
  y = unit(0.5, "npc"),
  width = unit(1, "npc"),
  height = unit(1, "npc"),
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  id = NULL,
  r = NULL,
  stroke_col = "black",
  stroke_lwd = 1,
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  vp = NULL
) {

  n <- length(x)

  lwd_orig   <- rep_len(gp$lwd %||% 1, n)
  stroke_lwd <- rep_len(stroke_lwd %||% 1, n)
  stroke_col <- rep_len(stroke_col %||% "black", n)

  no_stroke <- all(is.na(stroke_col) | stroke_col == "transparent") ||
    all(stroke_lwd <= 0)
  single <- length(unique(id)) < 2

  if ((single || no_stroke) && is.null(r)) {
    fg <- rectGrob(
      x = x, y = y,
      width = width, height = height,
      just = just, hjust = hjust, vjust = vjust,
      default.units = default.units,
      gp = gp
    )
    # Try early exit when there is no stroke
    if (no_stroke) {
      fg <- editGrob(fg, name = name, vp = vp)
      return(fg)
    }
  } else {
    hjust <- resolveHJust(just, hjust)
    vjust <- resolveVJust(just, vjust)
    id <- id %||% rep(1, n)
    if (is.null(r)) {
      sgp <- split_gp(gp, id)
      fg <- Map(
        rectGrob,
        x = split(x, id), y = split(y, id),
        width  = split(rep_len(width,  n), id),
        height = split(rep_len(height, n), id),
        hjust  = split(rep_len(hjust,  n), id),
        vjust  = split(rep_len(vjust,  n), id),
        default.units = default.units,
        gp = sgp
      )
    } else {
      just  <- c(rep_len(hjust, n), rep_len(vjust, n))
      nseq  <- seq_len(n)
      fg <- Map(
        roundrectGrob,
        x = split(x, nseq), y = split(y, nseq),
        width  = split(rep_len(width, n), nseq),
        height = split(rep_len(height, n), nseq),
        just   = if (!is.null(just))  split(just, c(nseq, nseq)),
        r      = r,
        default.units = default.units,
        gp = split_gp(gp, nseq)
      )
      fg <- split(fg, id)
      fg <- lapply(fg, function(x) do.call(grobTree, x))
    }
    if (no_stroke) {
      ans <- gTree(children = do.call(gList, fg), vp = vp, name = name)
      return(ans)
    }
  }

  # Set outline graphical parameters
  stroke_gp <- update_gpar(
    gp,
    col = stroke_col,
    lwd = lwd_orig + stroke_lwd * 2,
    fill = NULL
  )

  # Make background
  if (single && is.null(r)) {
    bg <- editGrob(fg, gp = stroke_gp, name = paste0(fg$name, "_bg"))
    bg <- clear_grob(bg = bg, fg = fg, col = gp$col, fill = gp$fill)
    out <- grobTree(bg, fg, vp = vp, name = name)
    return(out)
  }
  names <- paste0(vapply(fg, `[[`, "", "name"), "_bg")

  if (is.null(r)) {
    bg <- Map(editGrob, grob = fg, gp = split_gp(stroke_gp, id), name = names)
  } else {
    bg <- Map(
      function(ff, gp, nm) {
        children <- Map(editGrob, grob = ff$children, gp = gp)
        editGrob(do.call(grobTree, children), name = nm)
      },
      ff = fg,
      gp = lapply(split_gp(stroke_gp, id), split_gp, nseq),
      nm = names
    )
  }

  bg <- clear_grob(bg = bg, fg = fg, col = gp$col, fill = gp$fill)

  grob <- vec_interleave(bg, fg)
  gTree(children = do.call(gList, grob), vp = vp, name = name)
}
