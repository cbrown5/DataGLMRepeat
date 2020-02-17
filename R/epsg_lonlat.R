#' Just returns the EPSG code for WGS84 Long-Lat cooridnates
#'
#' @Usage epsg_lonlat()
#'
#' @param none
#'
#' @return A \code{character} object giving "+init=epsg:4326"
#' @Details 
#' @author Christopher J. Brown
#' @rdname epsg_lonlat
#' @export 

epsg_lonlat <- function(as_numeric = FALSE){
  if (as_numeric){
    4326
  } else {
  "+init=epsg:4326"
    }
}
