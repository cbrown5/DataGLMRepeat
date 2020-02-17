#' Paste with a dash 
#'
#' @Usage paste_(...)
#'
#' @param ... one or more R objects to convert to characters
#'
#' @return A string 
#' @Details shortcut for paste("a", "b", sep ="_")
#'  
#' @author Christopher J. Brown
#' @rdname paste_
#' @export

paste_ <- function(...){
  paste(..., sep = "_")
}
