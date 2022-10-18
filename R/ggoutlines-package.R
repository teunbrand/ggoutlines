#' @keywords internal
"_PACKAGE"

#' ggproto objects in ggoutlines
#'
#' The ggoutlines packages makes use of the ggproto class extension mechanism
#' of ggplot2. They ordinarily shouldn't be of any concern to the regular user,
#' as their access is implemented through the `geom_*()` family of functions.
#' For developers, you can build your own ggproto objects inheriting from
#' ggoutlines' ggproto objects as usual, and as indicated in the examples.
#' @name ggoutlines-ggproto
#' @rdname ggoutlines-ggproto
#' @examples
#' # Inheriting from ggoutlines' classes
#' MyOutlinePath <- ggproto(
#'   "MyOutlinePath", GeomPathOutline #, ...
#' )
NULL

#' Legend keys in ggoutlines
#'
#' These are legend key drawing function in the ggoutlines package. They are
#' not intended to be used directly, but they can be passed to layers as the
#' `key_glyph` argument to use them to draw legend keys.
#'
#' @name ggoutlines-keys
#' @rdname ggoutlines-keys
#' @examples
#' ggplot(economics, aes(date, unemploy, colour = "Unemployment")) +
#'   geom_path(key_glyph = draw_key_path_outline)

## usethis namespace: start
#' @importFrom farver decode_colour
#' @importFrom grid editGrob
#' @importFrom grid gList
#' @importFrom grid gpar
#' @importFrom grid grobTree
#' @importFrom grid groupGrob
#' @importFrom grid gTree
#' @importFrom grid pathGrob
#' @importFrom grid pointsGrob
#' @importFrom grid polylineGrob
#' @importFrom grid rectGrob
#' @importFrom grid resolveHJust
#' @importFrom grid resolveVJust
#' @importFrom grid roundrectGrob
#' @importFrom grid strokeGrob
#' @importFrom grid textGrob
#' @importFrom rlang %||%
#' @importFrom rlang list2
#' @importFrom stats ave
#' @importFrom vctrs data_frame
#' @importFrom vctrs vec_chop
#' @importFrom vctrs vec_group_loc
#' @importFrom vctrs vec_interleave
#' @importFrom vctrs vec_unique
#' @importFrom vctrs vec_unique_count
## usethis namespace: end
NULL
