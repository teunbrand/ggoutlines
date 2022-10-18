
#' Tetris
#'
#' A made up dataset of tetris-blocks (tetronimos) arranged in a grid.
#'
#' @format ## `tetris`
#' A data.frame with 28 rows and 4 columns:
#' \describe{
#'   \item{x}{The x-position of a single block.}
#'   \item{y}{The y-position of a single block.}
#'   \item{id}{A grouping identifier telling which blocks belong to the same
#'     shape.}
#'   \item{type}{The type of shape that the groups in the `id` column describe}
#' }
"tetris"
