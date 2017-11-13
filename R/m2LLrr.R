#' minus 2 times log-likelihood for reduced rank models using Sherman-Morrison-Woodbury
#'
#' minus 2 times loglikelihood for reduced rank models using Sherman-Morrison-Woodbury
#'
#' @param theta vector of estimated covariance parameters, with theta[1] the partial sill, 
#' theta[2] the range parameter of the reduced rank matrix, theta[3] the range parameter for the knots, and 
#' theta[4] the nugget effect.
#' @param z vector of data
#' @param rrDist distance matrix with any kind of distance metric. The reduced rank distance matrix.
#' @param knDist Euclidean distance matrix among knot locations 
#' @param corMod one of the autocorrelation models.  Valid options are 'exp', 'sph', 'gau', and 'cau', for 
#' exponential, spherical, Gaussian, and Cauchy, respectively
#' @param estMeth estimation method.  Default is "REML" for restricted maximum likelihood.  Other options are "ML" for maximum likelihood
#'
#' @return minus 2 times the (restricted) loglikelihood
#'
#' @author Jay Ver Hoef
m2LLrr <- function(theta, z, rrDist, knDist, corMod = 'exp', estMeth = 'REML')
{

	sigmap = exp(theta[1])
  alpha = exp(theta[2])
	rho = exp(theta[3])
	sigma0 = exp(theta[4])
	if(alpha > 99999) return(1e+30)
	if(rho > 99999) return(1e+30)
	n <- length(z)
	p = 1

	if(corMod == 'exp')
	  SMWXyout = SMWXy(acor.exp(knDist,rho), 
		  -as.numeric(determinant(acor.exp(knDist,rho), logarithm = TRUE)$modulus),
		  sigmap*acor.exp(rrDist,alpha)/sigma0^2,
		  length(rrDist[,1])*log(sigma0^2),
      sigmap*acor.exp(rrDist,alpha),
		  matrix(1/sigma0^2, nrow = length(rrDist[,1])),
		  matrix(z/sigma0^2, nrow = length(rrDist[,1])))
	if(corMod == 'sph')
	  SMWXyout = SMWXy(acor.sph(knDist,rho), 
		  -as.numeric(determinant(acor.sph(knDist,rho), logarithm = TRUE)$modulus),
		  sigmap*acor.sph(rrDist,alpha)/sigma0^2,
		  length(rrDist[,1])*log(sigma0^2),
      sigmap*acor.sph(rrDist,alpha),
		  matrix(1/sigma0^2, nrow = length(rrDist[,1])),
		  matrix(z/sigma0^2, nrow = length(rrDist[,1])))
	if(corMod == 'gau')
	  SMWXyout = SMWXy(acor.gau(knDist,rho), 
		  -as.numeric(determinant(acor.gau(knDist,rho), logarithm = TRUE)$modulus),
		  sigmap*acor.gau(rrDist,alpha)/sigma0^2,
		  length(rrDist[,1])*log(sigma0^2),
      sigmap*acor.gau(rrDist,alpha),
		  matrix(1/sigma0^2, nrow = length(rrDist[,1])),
		  matrix(z/sigma0^2, nrow = length(rrDist[,1])))
	if(corMod == 'cau')
	  SMWXyout = SMWXy(acor.cau(knDist,rho), 
		  -as.numeric(determinant(acor.cau(knDist,rho), logarithm = TRUE)$modulus),
		  sigmap*acor.cau(rrDist,alpha)/sigma0^2,
		  length(rrDist[,1])*log(sigma0^2),
      sigmap*acor.cau(rrDist,alpha),
		  matrix(1/sigma0^2, nrow = length(rrDist[,1])),
		  matrix(z/sigma0^2, nrow = length(rrDist[,1])))
	XViX = sum(SMWXyout[['invX']])
  logDetV = SMWXyout[['logdet']]


	logDetXViX <- log(XViX)
	covBetaHat <- 1/XViX
	betaHat <- covBetaHat*t(SMWXyout[['invX']])%*%as.matrix(z, ncol = 1)
	rVir <- t(SMWXyout[['invy']])%*%as.matrix(z, ncol = 1) -
    sum(SMWXyout[['invy']])^2*covBetaHat
	minus2LL <- logDetV + rVir + n*log(2*pi)
	if(estMeth == "REML") minus2LL <- minus2LL + logDetXViX - p*log(2*pi)
	return( as.numeric(minus2LL) )
}

