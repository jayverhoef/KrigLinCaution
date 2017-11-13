#-------------------------------------------------------------------------------
#
#           empsvgm
#
#-------------------------------------------------------------------------------

#' Empirical semivariogram
#'
#' Empirical semivariogram
#'
#' @param z a vector of the observed data
#' @param distMat distance matrix between all pairs of locations in the z vector,
#' in the same order as the z vector
#' @param breaks a vector of breaks to create distance bins
#'
#' @return a data.frame with 3 columes.  The first column is empirical
#' semivariogram for each distance class, the second column is the mean
#' distance in that distance class, and the final column is the number
#' of pairs of points in that distance class
#'
#' @author Jay Ver Hoef
empsvgm = function(z, distMat, breaks)
{
  # get squared differences among all pairs of points
  df2 <- (abs(z%o%rep(1, times = length(z)) - rep(1, times = length(z))%o%z))^2
  # remove duplicates from symmetric difference matrix
  # divide by 2 to get semivariances
  df2 = df2[lower.tri(df2)]/2
  # remove duplicated distances from symmetric distance matrix
  dists = distMat[lower.tri(distMat)]
  # classify distances into distance bins
  cutvec = cut(dists, breaks)
  out = data.frame(
    svgm = aggregate(df2, by = list(cutvec), mean)[,2],
    mndist = aggregate(dists, by = list(cutvec), mean)[,2],
    npair = aggregate(df2, by = list(cutvec), function(x) {length(x)})[,2]
  )
  out
}
