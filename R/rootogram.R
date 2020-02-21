#' Create a rootogram from observed and predicted counts.
#'
#' @Usage rootogram(yobs, ypred, nbrks = 10, hanging = FALSE, ...)
#'
#' @param yobs numeric of observed counts
#' @param ypred numeric of predicted counts
#' @param brks x-axis break points 
#' @param hanging logical indicating whether to do hanging bars or not
#' @param ... other arguments passed to plot.
#'
#' @return A rootogram.
#' @Details Rootograms are used to check model and are particuarly useful for
#' count models (e.g. Poisson GLM).
#' The red line shows frequency for sqrt(predictions) and the
#' grey boxes show range of sqrt(predictions) to
#' sqrt(predictions) - sqrt(observations) frequencies.
#' If the fit is good the grey boxes will range from zero to the red line. 
#' See Kleiber and Zeileis Visualizing Count Data
#' Regression using Rootograms
#' \url{{https://arxiv.org/pdf/1605.01311v1.pdf}
#' Works best if break points are offset by 0.5 from integer values
#'
#'@examples 
#' data(InsectSprays)
#' m1 <- glm(count ~ spray, family = "poisson", data = InsectSprays)
#' InsectSprays$fit <- predict(m1, type = "response")
#' with(InsectSprays, 
#'   rootogram(count, fit, nbrks = 12))
#'  #A very bad fit!
#'  
#' library(MASS)
#' m2 <- glm.nb(count ~ spray, data = InsectSprays)
#' InsectSprays$fit2 <- predict(m2, type = "response")
#' with(InsectSprays, 
#'   rootogram(count, fit2, nbrks = 12))
#'  #Still bad!
#' 
#' @author Christopher J. Brown
#' @rdname rootogram
#' @export

rootogram <- function(yobs, ypred, brks = NA, hanging = FALSE, ...){
  
  if (is.na(brks)){
    brks <- seq(0-0.5, ceiling(max(c(ypred, yobs)))+0.5, by = 1)
  } 
  xwd <- diff(brks)[1]/2.1
  xy <- hist(ypred, breaks = brks, plot = F)
  xy2 <- hist(yobs, breaks = brks, plot = F)
  ylwr <-  sqrt(xy$counts) - sqrt(xy2$counts)
  ylim <- c(min(ylwr), sqrt(max(xy$counts)))
  xlim <- c(min(brks), max(brks))
  plot(xy$mids, sqrt(xy$counts), lty = 3, col = 'red', type = 'b', ylim = ylim,
       xlim = xlim, xlab ='Count', ylab = 'sqrt(Frequency)', ...)
  
  if (hanging){
    rect(xy$mids - xwd, sqrt(xy$counts), xy$mids + xwd,ylwr, col = 'grey')
    }else{
      rect(xy$mids - xwd, sqrt(xy2$counts), xy$mids + xwd,0, col = 'grey')
    }
    
  lines(xy$mids, sqrt(xy$counts), lty = 3, type = 'b', pch = 16, col = 'red')
  abline(h = 0)
}
