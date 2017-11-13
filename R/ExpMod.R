#-------------------------------------------------------------------------------
#
#           ExpMod
#
#-------------------------------------------------------------------------------

#' Exponential autocorrelation model
#'
#' Exponential autocorrelation model
#'
#' @param dist a vector or matrix of distance values
#' @param rang the range of the exponential model
#'
#' @return a vector or matrix of the exponential model values for each distance
#'
#' @author Jay Ver Hoef
ExpMod = function(dist,rang){
  exp(-dist/rang)
}
