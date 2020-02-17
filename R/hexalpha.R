#' Makes a colour value transparent
#'
#' @Usage hexalpha(col, alpha=1)
#'
#' @param col is colour name, hex value or code
#' @param alpha is transparency {0,1}
#'
#' @return A hex colour code
#' @Details Values of alpha = 1 are solid, alpha = 0 is invisible.  
#' @author Christopher J. Brown
#' @rdname hexalpha
#' @export

hexalpha <- function(col, alpha=1){
  apply(
    sapply(col, col2rgb)/255, 2,
    function(x)
      rgb(x[1], x[2], x[3], alpha=alpha))
}
