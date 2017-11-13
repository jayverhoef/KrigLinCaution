#-------------------------------------------------------------------------------
#
#           vgrmFitWLS
#
#-------------------------------------------------------------------------------

#' Weighted least squares function for a variogram model
#'
#' Weighted least squares function for a variogram model
#'
#' @param theta a vector of covariance parameters, with theta[1] the partial 
#'   sill, theta[2] the range parameter, and theta[3] the nugget effect, on 
#'   the log scale
#' @param empsvgm a vector of empirical semivariogram values
#' @param dist a vector of distances matched to (the same length as) the 
#'   semivariogram values
#' @param npair a vector of the number of location-pairs matched to (the same 
#'   length as) the semivariogram values
#' @param aCorMod an autocorrelation model that takes a distance matrix and
#'   returns a matrix of autocorrelation values.  The autocorrelation model
#'   should have 2 arguments, with the first the distance matrix, the second
#'   the range parameter.
#' @param useNugget Either TRUE or FALSE on whether to add a nugget effect
#'   to the model.  If TRUE, the theta argument must have a length of 3.
#'   Default is TRUE.
#'
#' @return the weighted sums-of-squares of the difference between the empirical
#'   semivariogram values and the fitted model for the given theta.
#'
#' @author Jay Ver Hoef
vgrmFitWLS = function(theta, aCorMod, empsvgm, dist, npair, useNugget = TRUE) {
  svgmMod = exp(theta[1])*(1 - aCorMod(dist, exp(theta[2])))
  if(useNugget == TRUE) svgmMod = svgmMod + exp(theta[3])
  sum(npair*(empsvgm -  svgmMod )^2)
}
