#' minimum eigenvalue from a spatial covariance matrix
#'
#' minimum eigenvalue from a spatial covariance matrix
#'
#' @param fun.cor one of the autocorrelation functions
#' @param Dis distance matrix
#' @param alpha range parameter of autocorrelation function
#'
#' @return minimum eigenvalue
#'
#' @author Jay Ver Hoef
minEigVal = function(fun.acor, Dis, alpha) {
  min(eigen(fun.acor(Dis,alpha), symmetric = TRUE, only.values = TRUE)$values) 
}


