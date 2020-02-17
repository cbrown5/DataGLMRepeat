#' Copy coordinates from a plot to clipboard
#'
#' @Usage loccopy(n, digits = 2)
#'
#' @param n number of points, passed to \code{locator()}
#' @param digits digits to round to
#'
#' @return Sends coordinates from clicking on a plot to your clipboard
#' @Details
#' @author Christopher J. Brown
#' @rdname loccopy
#' @export

loccopy <- function(n, digits = 2){
	thiseol <- ifelse(n>1, "\n", "")
	data <- locator(n)
	data <- paste0(round(c(data$x, data$y), digits), collapse = ",")
	#for osx: 
	# clip <- pipe("clipboard", "w")
	# write.table(data, file = clip, quote = FALSE, col.names = F, row.names = F)
	# close(clip)
	writeClipboard(data)
}
