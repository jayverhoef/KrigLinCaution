#-------------------------------------------------------------------------------
#
#           CVsummStats
#
#-------------------------------------------------------------------------------

#' Summary statistics from a leave-one-out crossvalidation (LOOCV) from a 
#' fitted spatial model
#'
#' Summary statistics from a leave-one-out crossvalidation (LOOCV) from a 
#' fitted spatial model
#'
#' @param optimFit the optimized likelihood using the optim() function
#' @param obsvec a vector of the observed data
#' @param CVtable an n x 2 table from LOOCV, where the predicted values are in
#'   the first column, and the prediction variances are in the second column.
#'
#' @return a list with [[1]] AIC; [[2]] corr: the correlation between the obseved 
#' values and the LOOCV predictions; [[3]] RMSPE, the root-mean-squared prediction 
#' error from LOOCV; and [[4]] CI90, the proportion of times the LOOCV 90/%
#' prediction interval covers the true value.
#'
#' @author Jay Ver Hoef
CVsummStats = function(optimFit, obsVec, CVtable)
{
  list(
    AIC = if(is.null(optimFit)) NULL else
      optimFit$value + 2*length(optimFit$par) + 2,
    corr = cor(obsVec, CVtable[,1]),
    RMSPE = sqrt(mean((obsVec - CVtable[,1])^2)),
    CI90 = mean(CVtable[,1] - qnorm(.95)*sqrt(CVtable[,2]) < obsVec &  
                  obsVec < CVtable[,1] + qnorm(.95)*sqrt(CVtable[,2]))
  )
}
