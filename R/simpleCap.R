#' Change first letter of each word to upper case 
#'
#' @Usage simpleCap(x)
#'
#' @param x a character
#'
#' @return x with first letter of each word in upper case
#' @Details 
#' @author Christopher J. Brown, from \code{?toupper} 
#' @rdname simpleCap
#' @export 

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}
