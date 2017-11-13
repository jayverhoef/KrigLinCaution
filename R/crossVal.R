#-------------------------------------------------------------------------------
#
#           crossVal
#
#-------------------------------------------------------------------------------

#' A fast version of leave-one-out crossvalidation (LOOCV) needing without any
#' further matrix inverses
#'
#' A fast version of leave-one-out crossvalidation (LOOCV) needing without any
#' further matrix inverses
#'
#' @param z vector of data
#' @param X design matrix
#' @param V covariance matrix
#' @param Vi inverse of covariance matrix
#' @param n number of observations, number of rows in z
#' @param p number of covariates, number of columns in X
#' @param covb covariance matrix of fitted fixed effects
#' @param covbi inverse of covb
#' @param bhat fitted fixed effects
#'
#' @return a data.frame with crossvalidation predictions "cv.pred" and 
#' estimated prediction variances "cv.var"
#'
#' @author Jay Ver Hoef

crossVal <- function(z, X, V, Vi, n, p, covb, covbi, bhat)
{
	cdd.out <- matrix(-999.9, nrow = n, ncol = 2)
	for(i in 1:n) {
		Vi.i <- Vi[(1:n) != i,(1:n) != i] - 
			matrix(Vi[(1:n) != i,i],ncol = 1) %*% 
			matrix(Vi[i,(1:n) != i],nrow = 1)/Vi[i,i]
		c.i <- matrix(V[(1:n) != i,i],ncol = 1)
		xi <- matrix(X[i], ncol = 1)
		X.i <- X[(1:n) != i]
		z.i <- matrix(z[(1:n) != i], ncol = 1)
		xxi <- xi - t(X.i) %*% Vi.i %*% c.i
		hhi <- t(xxi) %*% covb %*% xxi
		zzi <- z[i] - t(z.i) %*% Vi.i %*% c.i
		si <- V[i,i]  - t(c.i) %*% Vi.i %*% c.i
		covb.i <- solve(t(X.i) %*% Vi.i %*% X.i)
		lam <- t(c.i + X.i %*% covb.i %*% xxi) %*% Vi.i
		cdd.out[i,1] <- lam %*% z.i
		cdd.out[i,2] <- (si + t(xxi) %*% covb.i %*% xxi)
	}
	cdd.out <- as.data.frame(cdd.out)
	names(cdd.out) <- c("cv.pred","cv.var")
	cdd.out
}


