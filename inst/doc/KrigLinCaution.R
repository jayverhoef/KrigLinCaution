## ----echo = FALSE, include = FALSE---------------------------------------
################################################################################
################################################################################
# smaller font size for chunks
options(scipen = 8)
################################################################################
################################################################################

## ----loadLibrary, echo = FALSE, include = FALSE--------------------------
  # load the libraries anew each time in case they get updated
  library(KrigLinCaution)
  library(xtable)
  library(knitr)

## ----Fig-autocorrModels, echo=FALSE, include = FALSE, fig.height = 11, fig.width = 11, cache = TRUE----
  source('Rchunks/Fig-autocorrModels.R')

## ----Fig-CautionEx, echo=FALSE, include = FALSE, fig.height = 18.5, fig.width = 12, cache = TRUE----
  source('Rchunks/Fig-CautionEx.R')

## ----Fig-realDatEigVals, echo=FALSE, include = FALSE, fig.height = 11, fig.width = 8, cache = TRUE----
  source('Rchunks/Fig-realDatEigVals.R')

## ----Fig-knotLocs, echo=FALSE, include = FALSE, cache = TRUE-------------
  source('Rchunks/Fig-knotLocs.R')

## ----FitsAndPreds, echo=FALSE, include = FALSE, cache = TRUE-------------
  source('Rchunks/FitsAndPreds.R')

## ----Fig-empSemivar, echo=FALSE, include = FALSE, fig.height = 11, fig.width = 8, cache = TRUE----
  source('Rchunks/Fig-empSemivar.R')

## ----Fig-EucLinScatter, echo=FALSE, include = FALSE, fig.height = 8, fig.width = 8, cache = TRUE----
  source('Rchunks/Fig-EucLinScatter.R')

## ----Tab-CVstats, echo= FALSE, include = FALSE, cache = TRUE-------------
  CVstats1 = as.matrix(rbind(
      c(5.093, 14.245, NA, NA, NA, unlist(CVLadExp)),
      c(psilExpLinWLSnug, rangExpLinWLSnug/1000, NA, nuggExpLinWLSnug, NA, unlist(CrosValExpLinWLSnug)),
      c(psilSphLinWLS, rangSphLinWLS/1000, NA, NA, NA, NA, NA, NA),
      c(psilSphLinWLSnug, rangSphLinWLSnug/1000, NA, nuggSphLinWLSnug, NA, unlist(CrosValSphLinWLSnug)),
      c(psilGauLinWLS, rangGauLinWLS/1000, NA, NA, NA, NA, NA, NA),
      c(psilGauLinWLSnug, rangGauLinWLSnug/1000, NA, nuggGauLinWLSnug, NA, unlist(CrosValGauLinWLSnug)),
      c(psilCauLinWLS, rangCauLinWLS/1000, NA, NA, NA, NA, NA, NA),
      c(psilCauLinWLSnug, rangCauLinWLSnug/1000, NA, nuggCauLinWLSnug, NA, unlist(CrosValCauLinWLSnug)),
      c(psilHolLinWLS, rangHolLinWLS/1000, NA, NA, NA, NA, NA, NA),
      c(psilHolLinWLSnug, rangHolLinWLSnug/1000, NA, nuggHolLinWLSnug, NA, NA, NA, NA),
      
      c(psilExpEucWLSnug, rangExpEucWLSnug/1000, NA, nuggExpEucWLSnug, NA, unlist(CrosValExpEucWLSnug)),
      c(psilSphEucWLSnug, rangSphEucWLSnug/1000, NA, nuggSphEucWLSnug, NA, unlist(CrosValSphEucWLSnug)),
      c(psilGauEucWLSnug, rangGauEucWLSnug/1000, NA, nuggGauEucWLSnug, NA, unlist(CrosValGauEucWLSnug)),
      c(psilCauEucWLSnug, rangCauEucWLSnug/1000, NA, nuggCauEucWLSnug, NA, unlist(CrosValCauEucWLSnug)),
      c(psilHolEucWLSnug, rangHolEucWLSnug/1000, NA, nuggHolEucWLSnug, NA, unlist(CrosValHolEucWLSnug)),
      
      c(psilExpEucREMLnug, rangExpEucREMLnug/1000, NA, nuggExpEucREMLnug, unlist(CrosValExpEucREMLnug)),
      c(psilSphEucREMLnug, rangSphEucREMLnug/1000, NA, nuggSphEucREMLnug, unlist(CrosValSphEucREMLnug)),
      c(psilGauEucREMLnug, rangGauEucREMLnug/1000, NA, nuggGauEucREMLnug, unlist(CrosValGauEucREMLnug)),
      c(psilCauEucREMLnug, rangCauEucREMLnug/1000, NA, nuggCauEucREMLnug, unlist(CrosValCauEucREMLnug)),
      c(psilHolEucREMLnug, rangHolEucREMLnug/1000, NA, nuggHolEucREMLnug, unlist(CrosValHolEucREMLnug)),
      
	    c(sigmapRRexp, alphaRRexp/1000, rhoRRexp/1000, sigma0RRexp, unlist(CVRRexp)),
	    c(sigmapRRsph, alphaRRsph/1000, rhoRRsph/1000, sigma0RRsph, unlist(CVRRsph)),
	    c(sigmapRRgau, alphaRRgau/1000, rhoRRgau/1000, sigma0RRgau, unlist(CVRRgau)),
	    c(sigmapRRcau, alphaRRcau/1000, rhoRRcau/1000, sigma0RRcau, unlist(CVRRcau))
    ) 
  ) 
  Mod = c("Exp", "Exp", "Sph", "Sph", "Gau", "Gau", "Cau", "Cau", "Hol", "Hol",
    "Exp","Sph", "Gau", "Cau","Hol","Exp","Sph", "Gau", "Cau","Hol",
    "Exp", "Sph", "Gau", "Cau")
  pd = NULL
  if(min(eigen(SigLadExp)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigExpLinWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigSphLinWLS)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigSphLinWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigGauLinWLS)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigGauLinWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigCauLinWLS)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigCauLinWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}  
  if(min(eigen(SigHolLinWLS)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigHolLinWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}

  if(min(eigen(SigExpEucWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}  
  if(min(eigen(SigSphEucWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  if(min(eigen(SigGauEucWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  if(min(eigen(SigCauEucWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  if(min(eigen(SigHolEucWLSnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  
  if(min(eigen(SigExpEucREMLnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}  
  if(min(eigen(SigSphEucREMLnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  if(min(eigen(SigGauEucREMLnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  if(min(eigen(SigCauEucREMLnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 
  if(min(eigen(SigHolEucREMLnug)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')} 

  if(min(eigen(SigRRexp)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigRRsph)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigRRgau)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  if(min(eigen(SigRRcau)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  Nnv = c(sum(LOOCV(SigLadExp,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigExpLinWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigSphLinWLS,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigSphLinWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigGauLinWLS,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigGauLinWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigCauLinWLS,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigCauLinWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigHolLinWLS,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigHolLinWLSnug,rd$non_motorised)[,2] < 0),
  
    sum(LOOCV(SigExpEucWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigSphEucWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigGauEucWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigCauEucWLSnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigHolEucWLSnug,rd$non_motorised)[,2] < 0),
  
    sum(LOOCV(SigExpEucREMLnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigSphEucREMLnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigGauEucREMLnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigCauEucREMLnug,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigHolEucREMLnug,rd$non_motorised)[,2] < 0),
  
    sum(LOOCV(SigRRexp,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigRRsph,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigRRgau,rd$non_motorised)[,2] < 0),
    sum(LOOCV(SigRRcau,rd$non_motorised)[,2] < 0))    
#  rownames(CVstats1) = string
  CVstats1 = as.data.frame(CVstats1)
  CVstats1 = cbind(
    Mod,
    c(rep(NA, times = 20), rep('Y', times = 4)),
    c(rep('Lin', times = 10),rep('Euc', times = 10), rep('Lin', times = 4)),
    c(' ', 'CWLS', rep(c('WLS','CWLS'), times = 4), rep('CWLS', times = 5),
      rep('REML', times = 9)),
    CVstats1[,1:4], pd, Nnv, CVstats1[,5:8])

  CVstats2 = as.matrix(rbind(
      c(sigmapVCsphLin, alphaVCsphLin/1000, alphaVCsphKnt/1000, sigma0VCsph, unlist(CVVCsph)),
      c(sigmapVCsphEuc, alphaVCsphEuc/1000, NA, NA, NA, NA, NA, NA)
    )
  )
  Mod = c("Sph", "Sph")
  pd = NULL
  if(min(eigen(SigVCsph)$values) > 0) {pd = c(pd,'Y')} else {pd = c(pd,' ')}
  pd = c(pd, '')
  Nnv = c(sum(LOOCV(SigVCsph,rd$non_motorised)[,2] < 0),'')
  CVstats2 = as.data.frame(CVstats2)
  CVstats2 = cbind(
    Mod,
    c('Y', 'N'),
    c('Lin', 'Euc'),
    c('REML', ' '),
    CVstats2[,1:4], pd, Nnv, CVstats2[,5:8])

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(CVstats1, 
      align = c('l',rep('l', times = length(CVstats1[1,]))),
      digits = c(0,0,0,0,0,1,1,1,1,0,0,2,3,3,3),
      caption = 'Cross-validation statistics',
      label = 'tab:CVstats'
    ),
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(CVstats2, 
      align = c('l',rep('l', times = length(CVstats2[1,]))),
      digits = c(0,0,0,0,0,1,1,1,1,0,0,2,3,3,3)
    ),
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----Fig-App4LocNetwork, echo=FALSE, include = FALSE, cache = TRUE-------
  source('Rchunks/Fig-App4LocNetwork.R')

## ------------------------------------------------------------------------
linDmat = rbind(
  c(0,1,2,2),
  c(1,0,1,1),
  c(2,1,0,2),
  c(2,1,2,0))

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(linDmat, 
      align = c('l',rep('l', times = length(linDmat[1,]))),
      digits = c(1,0,0,0,0)
    ),
    hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ------------------------------------------------------------------------
Sig = exp(-(linDmat/3)^2) + diag(rep(0.01, times = 4))

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(Sig, 
      align = c('l',rep('l', times = length(linDmat[1,]))),
      digits = c(1,3,3,3,3)
    ),
    hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ------------------------------------------------------------------------
Lambda = diag(eigen(Sig)$values)
Q = eigen(Sig)$vectors

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(Lambda, 
      align = c('l',rep('l', times = length(linDmat[1,]))),
      digits = c(1,3,3,3,3)
    ),
    hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(Q, 
      align = c('l',rep('l', times = length(linDmat[1,]))),
      digits = c(1,3,3,3,3)
    ),
    hline.after = NULL,
    size = 'footnotesize',
    include.rownames = FALSE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

## ------------------------------------------------------------------------
t(Q[,1]) %*% Q[,4]
t(Q[,4]) %*% Q[,4]

## ------------------------------------------------------------------------
v4 = Q[,4]
t(v4) %*% Sig %*% v4

## ------------------------------------------------------------------------
cvec = exp(-(c(1.3, 0.3, 1.3, 0.7)/3)^2)
cvec

## ------------------------------------------------------------------------
(1 + 0.01) - t(cvec) %*% solve(Sig) %*% cvec + 
  (1 - (sum(solve(Sig) %*% cvec))^2)/sum(solve(Sig))

