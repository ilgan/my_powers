#' Convert rgb image in png format into chroma2 image
#' Needs library(png)
#'
#'
#' @param img_png The name of the png impage file in the directory to be converted
#'
#' @return A matrix that is the sum of three channeles of the png file (R, G, and B) with the chroma 2 coefficiencies and normalization (for \code{rgb2chroma2}).
#'
#' @details
#' This function isn't complicated.
#'
#' Here are some reasons why putting a list in this section is excessive:
#' \itemize{
#'      \item These functions are quite simple.
#'      \item There's nothing else to say about these functions.
#' }
#'
#' function that these functions depend on.
#' @examples
#' rgb2chroma2()
#' @rdname rgb2chroma2
#' @export
#'

rgb2chroma2 <- function(img_png=TRUE){
	if (img_png == TRUE){
		return(FALSE)
	}
	else{
		rgbimg <- img_png
		#That's an array n by m by 3 . Now reduce to grey
		greyimg <- (0.615*rgbimg[,,1] + (-0.55861*rgbimg[,,2]) + (-0.05639*rgbimg[,,3]))
		#Normalize
		rgb2grey <- greyimg / max(greyimg)
		return(img_png)
	}
}
