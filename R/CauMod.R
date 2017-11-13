#-------------------------------------------------------------------------------
#
#           CauMod
#
#-------------------------------------------------------------------------------

#' Cauchy autocorrelation model
#'
#' Cauchy autocorrelation model
#'
#' @param dist a vector or matrix of distance values
#' @param rang the range of the spherical model
#'
#' @return a vector or matrix of the Cauchy model values for each distance
#'
#' @author Jay Ver Hoef
CauMod = function(dist,rang){
  1/(1 + (dist/rang)^2)
}
