#-------------------------------------------------------------------------------
#
#           HolMod
#
#-------------------------------------------------------------------------------

#' Hole effect autocorrelation model
#'
#' Hole effect autocorrelation model
#'
#' @param dist a vector or matrix of distance values
#' @param rang the range of the hole effect model
#'
#' @return a vector or matrix the hole effect model values for each distance
#'
#' @author Jay Ver Hoef
HolMod = function(dist, rang){
  mat = sin(dist/rang)/(dist/rang)*(dist != 0)
  mat[dist == 0] = 1
  mat
}
