#' Calculate semivariance given a distance matrix 
#'
#' @Usage semivariance(xdists, yresp, ncats = NA){
#'
#' @param xdists a distance matrix
#' @param yresp response variable
#' @param ncats number of categories for x-axis of semivariance plot
#'
#' @return A data.frame of distances and autocorrelation statistics
#' @Details Calculates semivariance function at discrete distance
#' intervals. It also calculates the Moran's I statistic. 
#' order of yresp should be the same as rows/cols of xdists. 
#' #ncats is the resolution. Setting ncats = NA (default)
#' then the function will use sturges rule for
#'  histogram classes. See Legendre and Legendre 'Numerical Ecology'
#'  For details of the math. 
#'
#' @examples 
#' xy <- cbind(runif(50), runif(50))
#' y <- rnorm(50)
#' dmat <- as.matrix(dist(xy))
#' sv <- semivariance(dmat, y)
#' with(sv, plot(distances, semivar))
#' 
#' @author Christopher J. Brown
#' @rdname semivariance
#' @export

 semivariance <- function(xdists, yresp, ncats = NA){
	nsites <- nrow(xdists)
	stopifnot(nsites == length(yresp))
	y_mean <- mean(yresp)
	
	#Set number of classes if not inputted
	if (is.na(ncats)) 
	  ncats <- round(1 + (3.3* log10(((nsites^2)/2)-(nsites/2))))

	#
	# DIVIDE SITES INTO DISTANCE CATEGORIES
	#
	
	#Get rid of symmetrical values, and don't allow comparisons of a site to itself
	distdiag <- xdists
	distdiag[!lower.tri(xdists, diag=F)] = NA 
	
	#Group sites
	distcatsm <- cut(as.vector(distdiag), breaks = ncats, labels = F)
	#Get midpoints of cuts
	cutlabs <- levels(cut(as.vector(distdiag), breaks = ncats, dig.lab=4))
	distints <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", cutlabs) ),
      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", cutlabs) ))
     distmids <- apply(distints, 1, median)
      
	#
	# SEMIVARIANCE CALCULATION
	#
	#Wd is the number of site pairs for a distance class ie Wd = sum(wd)
	#w[h,i] = 1 if the pair is this distance class and 0 if not
	#y[i] is the value at a site

	semivar <- rep(NA, ncats)
	moransI <- rep(NA, ncats)
	gearysc <- rep(NA, ncats)
	idistcats <- 1:ncats
	
	#
	#Weights matrix for each distance
	#
	wd.mat <- matrix(0, nrow = nsites^2, ncol = ncats)
	for (d in 1:ncats){
		wd.mat[distcatsm == d,d] <- 1
		}
		Wd <- colSums(wd.mat)
	#NB: Can I vectorise this? 
	#
	#Calculate semivariance
	#
	#Compare every value to every other value
	#squared difference
	sqrdiff <- (rep(yresp,nsites) - rep(yresp, each = nsites) ) ^2
	#NB can also be calculated transpose(Y) %*% Y where Y is a matrix of centred values
	yresp.comp <- matrix(sqrdiff, nrow = nsites^2, ncol = ncats)
	covar <- colSums(wd.mat * yresp.comp)
	semivar <- (1/(2*Wd)) * covar
	
	#
	# Calculate Morans I
	#	
	#squared difference
	sqrdiff <- (rep(yresp,nsites) - y_mean) * (rep(yresp, each = nsites) - y_mean)
	yresp.comp <- matrix(sqrdiff, nrow = nsites^2, ncol = ncats)
	sqrdiff.mean <- (yresp - y_mean)^2
	morans.covar <- colSums(wd.mat * yresp.comp)
	moransI <- (1/Wd) * morans.covar / ((1/nsites) * sum(sqrdiff.mean))
	 
	 return(data.frame(distances = distmids, 
	                   semivar = semivar, moransI = moransI))
	
	 }
