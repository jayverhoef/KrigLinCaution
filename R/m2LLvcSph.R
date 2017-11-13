#' minus 2 times log-likelihood for variance component model
#'
#' minus 2 times log-likelihood for variance component model with two spherical 
#' models, one using the reduced rank model for linear distance, and the other 
#' using Euclidean distance
#'
#' @param theta vector of estimated covariance parameters, where theta[1] is the 
#' partial sill for linear distance, theta[2] is the range for linear distance,
#' theta[3] is the partial sill Euclidean distance, theta[4] is the range for 
#' knots when using linear distance, theta[5] is the nugget effect, and theta[6]
#' is the range for Euclidean distance
#' @param z vector of data
#' @param rrDist linear network distance matrix between knots and observed data
#' @param knDist Euclidean distance matrix among knots
#' @param EdisMat matrix of Euclidean distances
#' @param estMeth estimation method.  Default is "REML" for restricted maximum 
#' likelihood.  Other options are "ML" for maximum likelihood
#'
#' @return minus 2 times the loglikelihood
#'
#' @author Jay Ver Hoef
m2LLvcSph <- function(theta, z, rrDist, knDist, EdisMat, estMeth = 'REML')
{
	nugg = exp(theta[5])
  psrr = exp(theta[1])
  pseu = exp(theta[3])
  rarr = exp(theta[2])
  rakn = exp(theta[4])
  raeu = exp(theta[6])
	if(psrr > 10) return(1e+30)
	if(rarr > 60000) return(1e+30)
  if(pseu > 10) return(1e+30)
	if(raeu > 60000) return(1e+30)
	n <- length(z)
	p = 1
	X = matrix(rep(1, times = n), ncol = 1)
  covMat = diag(rep(nugg, times = n)) + 
    psrr*SphMod(rrDist,rarr)%*%solve(SphMod(knDist,rakn))%*%
    t(SphMod(rrDist,rarr)) + pseu*SphMod(EdisMat,raeu)

	logDetV <- as.numeric(determinant(covMat, logarithm = TRUE)$modulus)
	Vi <- solve(covMat)
	XViX <- t(X)%*%Vi%*%X
	logDetXViX <- as.numeric(determinant(XViX, logarithm = TRUE)$modulus)
	covBetaHat <- solve(XViX)
	betaHat <- covBetaHat %*% t(X)%*% Vi%*% matrix(z, ncol = 1)
	r <- matrix(z - X %*% betaHat, ncol = 1)
	rVir <- t(r) %*% Vi %*% r
	minus2LL <- logDetV + rVir + n*log(2*pi)
	if(estMeth == "REML") minus2LL <- minus2LL + logDetXViX - p*log(2*pi)
	return( as.numeric(minus2LL) )
}


