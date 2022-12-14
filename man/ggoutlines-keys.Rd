% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_rect_outline.R, R/geom_line_outline.R,
%   R/geom_points_outline.R, R/geom_text_outline.R, R/ggoutlines-package.R
\name{draw_key_rect_outline}
\alias{draw_key_rect_outline}
\alias{draw_key_path_outline}
\alias{draw_key_point_outline}
\alias{draw_key_text_outline}
\alias{ggoutlines-keys}
\title{Legend keys in ggoutlines}
\usage{
draw_key_rect_outline(data, params, size)

draw_key_path_outline(data, params, size)

draw_key_point_outline(data, params, size)

draw_key_text_outline(data, params, size)
}
\description{
These are legend key drawing function in the ggoutlines package. They are
not intended to be used directly, but they can be passed to layers as the
\code{key_glyph} argument to use them to draw legend keys.
}
\examples{
ggplot(economics, aes(date, unemploy, colour = "Unemployment")) +
  geom_path(key_glyph = draw_key_path_outline)
}
