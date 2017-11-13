#' Sherman Morrison Woodbury inverse
#' 
#' Uses Sherman Morrison Woodbury on matrix formula inv(Z A t(Z) + B) and returns inv(Z A t(Z) + B) X and inv(Z A t(Z) + B) y  
#' and the log determinant. This keeps from storing the whole inverse for very large data problems.
#'
#' @param Ainv inverse of A matrix
#' @param Alogdet determinant of A matrix
#' @param BinvZ inverse of B matrix times Z
#' @param Blogdet log determinant of B matrix
#' @param Z a matrix
#' @param BinvX inverse of B matrix times X
#' @param Binvy inverse of B matrix times y
#'
#' @return a list with the inverse inv(Z A t(Z) + B) X, inv(Z A t(Z) + B) y and determinant of Z A t(Z) + B 
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
SMWXy <- function(Ainv, Alogdet, BinvZ, Blogdet, Z, BinvX, Binvy) {
	tZBinvZ <- t(Z) %*% BinvZ
  tZBinvX <- t(Z) %*% BinvX
	tZBinvy <- t(Z) %*% Binvy
	solveAZBZ =  solve(Ainv + tZBinvZ)
	list(invX = BinvX - BinvZ %*% solveAZBZ %*% tZBinvX,
		invy = Binvy - BinvZ %*% solveAZBZ %*% tZBinvy,
		logdet = Alogdet + Blogdet + 
		as.numeric(determinant(Ainv + tZBinvZ, logarithm = TRUE)$modulus))
}


