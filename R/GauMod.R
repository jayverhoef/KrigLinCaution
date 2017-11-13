#-------------------------------------------------------------------------------
#
#           GauMod
#
#-------------------------------------------------------------------------------

#' Gaussian autocorrelation model
#'
#' Gaussian autocorrelation model
#'
#' @param dist a vector or matrix of distance values
#' @param rang the range of the Gaussian model
#'
#' @return a vector or matrix of the Gaussian model values for each distance
#'
#' @author Jay Ver Hoef
GauMod = function(dist,rang){
  exp(-(dist/rang)^2)
}
