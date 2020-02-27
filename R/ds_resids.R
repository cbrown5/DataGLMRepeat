#' Plot Dunn-Smyth residuals
#'
#' @Usage ds_resids(yobs, ypred, plotds = TRUE, 
#' family = "poisson", phis = NULL)
#'
#' @param yobs numeric of observations
#' @param ypred numeric of predictions
#' @param plotds logical, should the residuals be plotted?
#' @param family character for family (poisson, binomial, negbinomial or
#'  nbinom)
#' @param phis scale parameter for nbinom (= 1/shape)
#'
#' @return A Dunn-Smyth residuals and optionally, a plot of them.
#' @Details Dunn-Smyth residuals are useful when checking for overdisperion or
#' hetergenous variance in count models.  As for normal residual plots, you
#' should look for fanning or evidence of non-linearities.
#' Code is based on that from the boral package (boral::ds.residuals)
#' This function is set-up for Poisson, Binomial and Negative binomial models. 
#' NB: the phi parameter for brms models lives in: summary(m1)$spec_pars
#'
#' @examples 
#' data(InsectSprays)
#' m1 <- glm(count ~ spray, family = "poisson", data = InsectSprays)
#' InsectSprays$fit <- predict(m1, type = "response")
#' with(InsectSprays, 
#'   ds_resids(count, fit, family = "poisson"))
#' #Looks good 
#' 
#' library(MASS)
#' m2 <- glm.nb(count ~ spray, data = InsectSprays)
#' InsectSprays$fit2 <- predict(m2, type = "response")
#' with(InsectSprays, 
#'   ds_resids(count, fit2, family = "nbinom", phis = 1/m2$theta))
#'  #Ok good, note we need 1/theta to get phi 
#'  
#' @author Christopher J. Brown
#' @rdname ds_resids
#' @export

ds_resids <- function(yobs, ypred, plotds = TRUE, 
                          family = "poisson", 
                          phis = NULL, trial.size = 1){
  if (family == "poisson"){
    a <- ppois(yobs - 1, ypred)
    b <- ppois(yobs, ypred) 
  }
  if (family %in% c("nbinom", "negbinomial")){
    a <- pnbinom(yobs - 1, mu = ypred, size = 1/phis)
    b <- pnbinom(yobs, mu = ypred, size = 1/phis) 
  }
  if (family == "binomial") {
    a <- pbinom(yobs - 1, 
                trial.size, prob = ypred)
    b <- pbinom(yobs, trial.size, 
                prob = ypred)
  }
  
	u <- runif(n = length(yobs), min = a, max = b)
	dsres <- qnorm(u)
	#check this
	if (plotds){
		par(mfrow = c(1,2))
		plot(ypred, dsres, xlab = "Prediction", ylab = "Dunn-Smyth residual")
		abline(h=0)
		qqnorm(dsres)
		abline(0,1)
	}
	return(dsres)
	}
