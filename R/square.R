#' Square or Cube a vector
#'
#' That's it -- these functions just square or cube a vector.
#'
#' @param x The vector to be squared or cubed.
#'
#' @return A vector that is the square (for \code{square}) or cube (for \code{cube}) of \code{x}.
#'
#' @details
#' This function isn't complicated.
#'
#' And it almost certainly doesn't need two paragraphs in the "Details"
#' section!
#'
#' Here are some reasons why putting a list in this section is excessive:
#' \itemize{
#'      \item These functions are quite simple.
#'      \item There's nothing else to say about these functions.
#' }
#'
#' If you really want to see more, check out the internal \code{\link{pow}}
#' function that these functions depend on.
#' @examples
#' cube(1:10)
#' square(-5)
#' @rdname square
#' @export
square <- function(x) pow(x, a=2)

#' @rdname square
#' @export
cube <- function(x) pow(x, a=3)
