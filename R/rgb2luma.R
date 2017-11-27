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
#'
#' function that these functions depend on.
#' @examples
#' rgb2luma("img.png")
#' @rdname rgb2luma
#' @export
#'

rgb2luma <- function(img_png){
	if (!grepl(typeof(img_png), "character", fixed=TRUE)){
		"Input should be a character 'img.png'"
	}
	rgbimg <- png::readPNG(img_png)

	#That's an array n by m by 3 . Now reduce to grey
	greyimg <- (0.2126*rgbimg[,,1] + 0.7152*rgbimg[,,2] + 0.0722*rgbimg[,,3])

	#Normalize
	rgb2grey <- greyimg / max(greyimg)
	png('luma.png')
	plot(c(0,1),c(0,1),t='n')
	rasterImage(bar, 0,0,1,1)
	dev.off()
}
