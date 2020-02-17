#' Add polygon shading for error bars to a figure 
#'
#' @Usage addpoly(x, y,y0=NULL, col = 'grey', border = 'NA',alpha = 0.5){
#'
#' @param x numeric of x values
#' @param y numeric of upper y values
#' @param y0 numeric of lower y values. Default is zero. 
#' @param border character with colour name or hex code
#' @param col colour of border
#' @param alpha transparency {0,1} of polygon shading
#'
#' @return Nothing
#' @Details 
#' @author Christopher J. Brown
#' @rdname addpoly
#' @export 

addpoly <- function(x, y,y0=NULL, col = 'grey', border = 'NA',alpha = 0.5){
	col <- hexalpha(col, alpha = alpha)
	y0 <- rev(y0)
	if (is.null(y0)){y0 <- rep(0, length(y)) }
	
	polygon(c(x, rev(x)), c(y, y0), col = col, border = border)
	}