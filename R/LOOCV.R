#-------------------------------------------------------------------------------
#
#           LOOCV
#
#-------------------------------------------------------------------------------

#' Leave-one-out crossvalidation (LOOCV) for a fitted spatial model
#'
#' Leave-one-out crossvalidation (LOOCV) for a fitted spatial model
#'
#' @param SigMat The fitted covariance matrix for the spatial data
#' @param obsvec a vector of the observed data
#'
#' @return a data.frame with the predictions in the first column and the 
#' prediction variances in the second column
#'
#' @author Jay Ver Hoef
LOOCV = function(SigMat, obsVec)
{
  SigMatInv = solve(SigMat)
  covb = sum(SigMatInv)
  covbi = 1/covb
  bhat = covb*sum(SigMatInv%*%obsVec)
  n = length(obsVec)
  LOOCVout = crossVal(
    z = matrix(obsVec, nrow = n, ncol = 1), 
    X = matrix(1, nrow = n, ncol = 1), 
    V = SigMat, 
    Vi = SigMatInv, 
    n = n, 
    p = 1, 
    covb = covb, 
    covbi = covbi, 
    bhat = bhat)
  LOOCVout
}


