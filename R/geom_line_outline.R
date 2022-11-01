# Constructors ------------------------------------------------------------

#' Outlined lines
#'
#' Like [`ggplot2::geom_path()`], this draws lines in the plot. However, the
#' lines have an outline, that can make it easier to discern overlapping lines.
#'
#' @inheritParams ggplot2::geom_path
#' @param by_group A `logical(1)` for setting groupwise outlines. If `TRUE`,
#'   (default) will draw outlines for individual lines. If `FALSE`, the
#'   collection of lines as a whole gets an outline.
#'
#' @return A `<LayerInstance>` object that can be added to a plot.
#' @export
#' @eval ggplot2:::rd_aesthetics("geom", "path_outline")
#'
#' @examples
#' # A standard plot
#' p <- ggplot(ChickWeight) +
#'   aes(Time, weight, group = Chick, colour = Diet)
#'
#' # Showing outlines makes it easier to distinguish separate trends
#' p + geom_line_outline()
#'
#' # Adapt the stroke to plot background to show 'interrupted' lines
#' p + geom_line_outline(stroke_colour = "grey92")
#'
#' # Alternatively, have all lines as foreground and strokes as background
#' p + geom_line_outline(by_group = FALSE)
geom_path_outline <- function(
  mapping   = NULL,
  data      = NULL,
  stat      = "identity",
  position  = "identity",
  ...,
  lineend   = "butt",
  linejoin  = "round",
  linemitre = 10,
  by_group  = TRUE,
  arrow = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomPathOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      by_group  = by_group,
      arrow     = arrow,
      na.rm     = na.rm,
      ...
    )
  )
}

#' @export
#' @rdname geom_path_outline
geom_line_outline <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  na.rm    = FALSE,
  by_group = TRUE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomLineOutline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params   = list2(
      na.rm  = na.rm,
      orientation = orientation,
      by_group = by_group,
      ...
    )
  )
}

# Key ---------------------------------------------------------------------

#' @export
#' @rdname ggoutlines-keys
draw_key_path_outline <- function(data, params, size) {
  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }
  grob_outline_line(
    x = c(0.1, 0.9), y = c(0.5, 0.5),
    gp = gpar(
      col  = alpha(data$colour %||% data$fill %||% "white", data$alpha),
      fill = alpha(params$arrow.fill %||% data$colour %||% data$fill %||%
                     "white", data$alpha),
      lwd = (data$linewidth %||% 0.5) * .pt,
      lty = data$linetype %||% 1,
      lineend = params$lineend %||% "butt"
    ),
    arrow = params$arrow,
    stroke_col = data$stroke_colour %||% "black",
    stroke_lwd = data$stroke_linewidth %||% 1
  )
}

# Classes -----------------------------------------------------------------

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPathOutline <- ggproto(
  "GeomPathOutline", GeomPath,

  default_aes = aes(
    colour    = "white",
    linewidth = 0.5,
    linetype  = 1,
    alpha     = NA,
    stroke_colour = "black",
    stroke_linewidth = 1
  ),

  draw_panel = function(self, data, panel_params, coord,
                        arrow = NULL, lineend = "butt", linejoin = "round",
                        linemitre = 10, na.rm = FALSE, by_group = TRUE) {

    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(paste0(
        "{.fn {snake_class(self)}}: Each group consists of only one ",
        "observation."
      ), i = "Do you need to adjust the {.field group} aesthetic?"))
    }

    # Order by group
    data    <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- ave(seq_len(nrow(munched)), munched$group, FUN = length)
    muched <- munched[rows >= 2, , drop = FALSE]
    if (nrow(munched) < 2) {
      return(zeroGrob())
    }

    ccols <- c("alpha", "colour", "linewidth", "linetype",
               "stroke_colour", "stroke_linewidth")
    attr <- ggplot2:::dapply(muched, "group", function(df) {
      linetype <- unique0(df$linetype)

      data_frame0(
        solid    = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique0(df[, ccols])) == 1,
        .size    = 1L
      )
    })
    solid_lines <- all(attr$solid)
    constant    <- all(attr$constant)
    if (!solid_lines && !constant) {
      cli::cli_abort(paste0(
        "{.fn {snake_class(self)}} can't have varying {.field colour}, ",
        "{.field linewidth}, {.field stroke_colour}, {.field stroke_linewidth}",
        " and/or {.field alpha} along the line when {.field linetype} isn't ",
        "solid."
      ))
    }

    n <- nrow(munched)
    group_diff <- muched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end   <- c(group_diff, TRUE)

    if (!constant) {
      super_id <- if (by_group) {
        vec_interleave(munched$group[!end], munched$group[!start])
      } else {
        rep(1L, sum(!end) + sum(!start))
      }

      grob_outline_line(
        x = vec_interleave(munched$x[!end], munched$x[!start]),
        y = vec_interleave(munched$y[!end], munched$y[!start]),
        id.lengths = rep(2, sum(!end)),
        super_id = super_id,
        gp = gpar(
          col  = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$colour, munched$alpha)[!end],
          lwd  = munched$linewidth[!end] * .pt,
          lty  = munched$linetype[!end],
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        ),
        stroke_col = munched$stroke_colour[!end],
        stroke_lwd = munched$stroke_linewidth[!end]
      )
    } else {
      super_id <- if (by_group) NULL else rep(1, length(start))
      id <- match(muched$group, unique0(muched$group))
      grob_outline_line(
        x = munched$x, y = muched$y, id = id,
        default.units = "native", arrow = arrow,
        super_id = super_id,
        gp = gpar(
          col  = alpha(munched$colour, munched$alpha)[start],
          fill = alpha(munched$colour, munched$alpha)[start],
          lwd  = munched$linewidth[start] * .pt,
          lty  = munched$linetype[start],
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre
        ),
        stroke_col = munched$stroke_colour[start],
        stroke_lwd = munched$stroke_linewidth[start]
      )
    }

  },

  draw_key = draw_key_path_outline
)

#' @rdname ggoutlines-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLineOutline <- ggproto(
  "GeomLineOutline", GeomPathOutline,

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)
