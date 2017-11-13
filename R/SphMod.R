#-------------------------------------------------------------------------------
#
#           SphMod
#
#-------------------------------------------------------------------------------

#' Spherical autocorrelation model
#'
#' spherical autocorrelation model
#'
#' @param dist a vector or matrix of distance values
#' @param rang the range of the spherical model
#'
#' @return a vector or matrix of the spherical model values for each distance
#'
#' @author Jay Ver Hoef
SphMod = function(dist, rang){
  mat = (1 - 3*dist/(2*rang) + dist^3/(2*rang^3))*(dist <= rang)
  mat[dist > rang] == 0
  mat
}
