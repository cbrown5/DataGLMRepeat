#' Add space between text for a label
#'
#' @Usage textspace(x,y, labnam, nspaces = 1, ...)
#'
#' @param x numeric for x coordinate
#' @param y numeric for y coordinate
#' @param labnam character giving label
#' @param nspaces numeric giving number of spaces between letters
#' @param ... other arguments to \code{text}
#'
#' @return Nothing
#' @details Add spaces between text, useful for maps
#' @author Christopher J. Brown
#' @rdname textspace
#' @export

textspace <- function(x,y, labnam, nspaces = 1, ...){
    treplace <- paste0("\\1", paste0(rep("", nspaces), collapse = " "))
    t2 <- gsub("(.)", treplace, labnam, ignore.case = T)
    text(x, y, t2, ...)
}
