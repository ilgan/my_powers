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
#' rgb2chroma2("img.png")
#' @rdname rgb2chroma2
#' @export
#'

rgb2chroma2 <- function(img_png){
	if (!grepl(typeof(img_png), "character", fixed=TRUE)){
		"Input should be a character 'img.png'"
	}
	rgbimg <- png::readPNG(img_png)

	#That's an array n by m by 3 . Now reduce to grey
	greyimg <- (0.615*rgbimg[,,1] + (-0.55861*rgbimg[,,2]) + (-0.05639*rgbimg[,,3]))

	#Normalize
	rgb2grey <- greyimg / max(greyimg)
	png('chroma2.png')
	plot(c(0,1),c(0,1),t='n')
	rasterImage(bar, 0,0,1,1)
	dev.off()
}
