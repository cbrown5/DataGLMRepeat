#' Scale a variable 
#'
#' @Usage scale2(x, ...)
#'
#' @param x a numeric variable
#' @param ... arguments to pass to mean() and sd()
#' @return Numeric
#' @Details (x - mean(x))/sd(x). R's base function 'scale' returns
#' annoying extra attributes. This is a clean version. 
#' @author Christopher J. Brown
#' @rdname scale2
#' @export


scale2 <- function(x, ...){
  (x-mean(x, ...))/sd(x, ...)
}

