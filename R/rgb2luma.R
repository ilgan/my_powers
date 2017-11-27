#' Convert rgb image in png format into luma image
#' Needs library(png)
#'
#'
#' @param img_png The name of the png impage file in the directory to be converted
#'
#' @return A matrix that is the sum of three channeles of the png file (R, G, and B) and normalization (for \code{rgb2luma}).
#'
#' @details
#' This function isn't complicated.
#'
#' Here are some reasons why putting a list in this section is excessive:
#' \itemize{
#'      \item These functions are quite simple.
#'      \item There's nothing else to say about these functions.
#' }
#' function that these functions depend on.
#' @examples
#' rgb2luma()
#' @rdname rgb2luma
#' @export
#'

rgb2luma <- function(img_png=TRUE){
	if (img_png == TRUE){
		return(FALSE)
	}
	else{
		rgbimg <- img_png
		#That's an array n by m by 3 . Now reduce to grey
		greyimg <- (0.2126*rgbimg[,,1] + 0.7152*rgbimg[,,2] + 0.0722*rgbimg[,,3])
		#Normalize
		rgb2grey <- greyimg / max(greyimg)
		return(img_png)
	}
}
