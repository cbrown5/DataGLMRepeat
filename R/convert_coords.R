#' Create a function that converts between two coordinate spaces
#'
#' This function is a closure that is used to create a function
#' can translate between two coordinate spaces.  Such a function
#' is useful for digitising graphs and for creating inset
#' plots.
#' @Usage convert_coords(xyfrom, xyto)
#'
#' @param xyfrom vector of the form \code{c(xmin, xmax, ymin, ymax)}
#' giving coordinates to translate from.
#' @param xyto vector of the form \code{c(xmin, xmax, ymin, ymax)}
#' giving coordinates to translate to
#'
#' @return A function that coverts to a new coordinate space
#' @Details This is a closure, so returns a function 
#' @examples
#' \dontrun{
#' # To create a function that plots a new plot ontop another
#' # using NDC:
#' posplot <- covert_coords(par('usr'), par('plt'))
#' }
#' @author Christopher J. Brown
#' @rdname convert_coords
#' @export

convert_coords <- function(xyfrom, xyto){
  xrise <- xyto[2] - xyto[1]
  xrun <- xyfrom[2] - xyfrom[1]
  xa <- xrise/xrun
  xb <- xyto[1]-(xa * xyfrom[1])

  #Ypts
  yrise <- xyto[4] - xyto[3]
  yrun <- xyfrom[4] - xyfrom[3]
  ya <- yrise/yrun
  yb <- xyto[3]-(ya * xyfrom[3])

    function(xy){
      n <- nrow(xy)
      if(is.null(n)) { n <- 1}
      mat <- matrix(NA, nrow = n, ncol = 2)
      mat[,1] <- xy[,1]*xa + xb
      mat[,2] <- xy[,2]*ya + yb
      return(mat)
    }
}
