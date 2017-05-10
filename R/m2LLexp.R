#' minus 2 times log-likelihood for exponential spatial covariance model
#'
#' minus 2 times log-likelihood for exponential spatial covariance model
#'
#' @param theta vector of estimated covariance parameters, where theta[1] is the partial sill, theta[2] is the range, 
#' and theta[3] is the nugget effect
#' @param z vector of data 
#' @param EucDist matrix of Euclidean distances
#' @param estMeth estimation method.  Default is "REML" for restricted maximum likelihood.  Other options are "ML" 
#' for maximum likelihood
#'
#' @return minus 2 times the loglikelihood
#'
#' @author Jay Ver Hoef
m2LLexp <- function(theta, z, EucDist, estMeth = 'REML')
{

	sigmap = exp(theta[1])
  alpha = exp(theta[2])
	sigma0 = exp(theta[3])
	if(sigmap > 10) return(1e+30)
	if(alpha > 28200) return(1e+30)
	n <- length(z)
	p = 1
	X = matrix(rep(1, times = n), ncol = 1)
  covMat = sigmap^2*exp(-EucDist/alpha) + 
		diag(rep(sigma0^2, times = length(rrDist[,1])))

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


