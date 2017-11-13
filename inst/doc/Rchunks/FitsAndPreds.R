# check if EdisMat and locs are in the same order?  Yes.
distDiff = as.matrix(dist(locs[,2:3], diag = TRUE, upper = TRUE)) - EdisMat
# max(distDiff)
# so then ldisMat is also in that order

# get a reduced rank linear distance matrix from knots to all locations
rrDist = ldisMat[,rownames(knots)]
knDist = as.matrix(dist(knots[,2:3], diag = TRUE, upper = TRUE))

data(BLUPS)
respDat = BLUPs
respDat[,'Location'] = as.character(respDat[,1])
# check if observed response data are in same order as distance matrix
cbind(rownames(ldisMat),respDat[,1])
rd = merge(data.frame(Location = rownames(ldisMat)),respDat, by = "Location")
cbind(rownames(ldisMat),as.character(rd[,1]))

#   use non-motorised data from Ladle's paper

# -----------------------------------------------------------------------------
#        Empirical Semivariogram used for some of the fits
# -----------------------------------------------------------------------------

brks = c(0, 5000, 5000 + (1:20)*2500, max(ldisMat)+1)
empsvgm.out = empsvgm(z = rd$non_motorised, distMat = ldisMat, breaks = brks)
empsvgm.out = empsvgm.out[1:(dim(empsvgm.out)[1]-1),]
brks = c(0, 5000, 5000 + (1:14)*2500, max(EdisMat)+1)
empsvgmE.out = empsvgm(z = rd$non_motorised, distMat = EdisMat, breaks = brks)
empsvgmE.out = empsvgmE.out[1:(dim(empsvgmE.out)[1]-1),]

# -----------------------------------------------------------------------------
#               Original Ladle Model
# -----------------------------------------------------------------------------

sigmapLexp = 5.093
alphaLexp = 14245
SigLadExp = sigmapLexp*exp(-ldisMat/alphaLexp)
CVLadExp = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigLadExp,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Exponential Model with Linear Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitExpLinWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = ExpMod, useNugget = useNugget)
psilExpLinWLSnug = exp(fitExpLinWLSnug$par)[1]
rangExpLinWLSnug = exp(fitExpLinWLSnug$par)[2]
if(useNugget == TRUE) {nuggExpLinWLSnug = exp(fitExpLinWLSnug$par)[3]} else
  nuggExpLinWLSnug = 0
SigExpLinWLSnug = psilExpLinWLSnug*ExpMod(ldisMat,rangExpLinWLSnug) +
  diag(rep(nuggExpLinWLSnug, times = dim(ldisMat)[1]))
CrosValExpLinWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigExpLinWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Spherical Model with Linear Distance, No Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = FALSE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitSphLinWLS = optim(theta, vgrmFitWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = SphMod, useNugget = useNugget)
psilSphLinWLS = exp(fitSphLinWLS$par)[1]
rangSphLinWLS = exp(fitSphLinWLS$par)[2]
if(useNugget == TRUE) {nuggSphLinWLS = exp(fitSphLinWLS$par)[3]} else
  nuggSphLinWLS = 0
SigSphLinWLS = psilSphLinWLS*SphMod(ldisMat,rangSphLinWLS) +
  diag(rep(nuggSphLinWLS, times = dim(ldisMat)[1]))
CrosValSphLinWLS = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigSphLinWLS,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Spherical Model with Linear Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitSphLinWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = SphMod, useNugget = useNugget)
psilSphLinWLSnug = exp(fitSphLinWLSnug$par)[1]
rangSphLinWLSnug = exp(fitSphLinWLSnug$par)[2]
if(useNugget == TRUE) {nuggSphLinWLSnug = exp(fitSphLinWLSnug$par)[3]} else
  nuggSphLinWLSnug = 0
SigSphLinWLSnug = psilSphLinWLSnug*SphMod(ldisMat,rangSphLinWLSnug) +
  diag(rep(nuggSphLinWLSnug, times = dim(ldisMat)[1]))
CrosValSphLinWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigSphLinWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Guassian Model with Linear Distance, No Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = FALSE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitGauLinWLS = optim(theta, vgrmFitWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = GauMod, useNugget = useNugget)
psilGauLinWLS = exp(fitGauLinWLS$par)[1]
rangGauLinWLS = exp(fitGauLinWLS$par)[2]
if(useNugget == TRUE) {nuggGauLinWLS = exp(fitGauLinWLS$par)[3]} else
  nuggGauLinWLS = 0
SigGauLinWLS = psilGauLinWLS*GauMod(ldisMat,rangGauLinWLS) +
  diag(rep(nuggGauLinWLS, times = dim(ldisMat)[1]))
CrosValGauLinWLS = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigGauLinWLS,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Guassian Model with Linear Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitGauLinWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = GauMod, useNugget = useNugget)
psilGauLinWLSnug = exp(fitGauLinWLSnug$par)[1]
rangGauLinWLSnug = exp(fitGauLinWLSnug$par)[2]
if(useNugget == TRUE) {nuggGauLinWLSnug = exp(fitGauLinWLSnug$par)[3]} else
  nuggGauLinWLSnug = 0
SigGauLinWLSnug = psilGauLinWLSnug*GauMod(ldisMat,rangGauLinWLSnug) +
  diag(rep(nuggGauLinWLSnug, times = dim(ldisMat)[1]))
CrosValGauLinWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigGauLinWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Cauchy Model with Linear Distance, No Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = FALSE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitCauLinWLS = optim(theta, vgrmFitWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = CauMod, useNugget = useNugget)
psilCauLinWLS = exp(fitCauLinWLS$par)[1]
rangCauLinWLS = exp(fitCauLinWLS$par)[2]
if(useNugget == TRUE) {nuggCauLinWLS = exp(fitCauLinWLS$par)[3]} else
  nuggCauLinWLS = 0
SigCauLinWLS = psilCauLinWLS*CauMod(ldisMat,rangCauLinWLS) +
  diag(rep(nuggCauLinWLS, times = dim(ldisMat)[1]))
CrosValCauLinWLS = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigCauLinWLS,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Cauchy Model with Linear Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitCauLinWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = CauMod, useNugget = useNugget)
psilCauLinWLSnug = exp(fitCauLinWLSnug$par)[1]
rangCauLinWLSnug = exp(fitCauLinWLSnug$par)[2]
if(useNugget == TRUE) {nuggCauLinWLSnug = exp(fitCauLinWLSnug$par)[3]} else
  nuggCauLinWLSnug = 0
SigCauLinWLSnug = psilCauLinWLSnug*CauMod(ldisMat,rangCauLinWLSnug) +
  diag(rep(nuggCauLinWLSnug, times = dim(ldisMat)[1]))
CrosValCauLinWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigCauLinWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Hole Effect Model with Linear Distance, No Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = FALSE
theta = log(c(4,15000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitHolLinWLS = optim(theta, vgrmFitWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = HolMod, useNugget = useNugget)
psilHolLinWLS = exp(fitHolLinWLS$par)[1]
rangHolLinWLS = exp(fitHolLinWLS$par)[2]
if(useNugget == TRUE) {nuggHolLinWLS = exp(fitHolLinWLS$par)[3]} else
  nuggHolLinWLS = 0
SigHolLinWLS = psilHolLinWLS*HolMod(ldisMat,rangHolLinWLS) +
  diag(rep(nuggHolLinWLS, times = dim(ldisMat)[1]))
CrosValHolLinWLS = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigHolLinWLS,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Hole Effect Model with Linear Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitHolLinWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair,
  aCorMod = HolMod, useNugget = useNugget)
psilHolLinWLSnug = exp(fitHolLinWLSnug$par)[1]
rangHolLinWLSnug = exp(fitHolLinWLSnug$par)[2]
if(useNugget == TRUE) {nuggHolLinWLSnug = exp(fitHolLinWLSnug$par)[3]} else
  nuggHolLinWLSnug = 0
SigHolLinWLSnug = psilHolLinWLSnug*HolMod(ldisMat,rangHolLinWLSnug) +
  diag(rep(nuggHolLinWLSnug, times = dim(ldisMat)[1]))
CrosValHolLinWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigHolLinWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Exponential Model with Euclidean Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitExpEucWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgmE.out$svgm,
  dist = empsvgmE.out$mndist, npair = empsvgmE.out$npair,
  aCorMod = ExpMod, useNugget = useNugget)
psilExpEucWLSnug = exp(fitExpEucWLSnug$par)[1]
rangExpEucWLSnug = exp(fitExpEucWLSnug$par)[2]
if(useNugget == TRUE) {nuggExpEucWLSnug = exp(fitExpEucWLSnug$par)[3]} else
  nuggExpEucWLSnug = 0
SigExpEucWLSnug = psilExpEucWLSnug*ExpMod(EdisMat,rangExpEucWLSnug) +
  diag(rep(nuggExpEucWLSnug, times = dim(EdisMat)[1]))
CrosValExpEucWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigExpEucWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Spherical Model with Euclidean Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitSphEucWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgmE.out$svgm,
  dist = empsvgmE.out$mndist, npair = empsvgmE.out$npair,
  aCorMod = SphMod, useNugget = useNugget)
psilSphEucWLSnug = exp(fitSphEucWLSnug$par)[1]
rangSphEucWLSnug = exp(fitSphEucWLSnug$par)[2]
if(useNugget == TRUE) {nuggSphEucWLSnug = exp(fitSphEucWLSnug$par)[3]} else
  nuggSphEucWLSnug = 0
SigSphEucWLSnug = psilSphEucWLSnug*SphMod(EdisMat,rangSphEucWLSnug) +
  diag(rep(nuggSphEucWLSnug, times = dim(EdisMat)[1]))
CrosValSphEucWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigSphEucWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Gaussian Model with Euclidean Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitGauEucWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgmE.out$svgm,
  dist = empsvgmE.out$mndist, npair = empsvgmE.out$npair,
  aCorMod = GauMod, useNugget = useNugget)
psilGauEucWLSnug = exp(fitGauEucWLSnug$par)[1]
rangGauEucWLSnug = exp(fitGauEucWLSnug$par)[2]
if(useNugget == TRUE) {nuggGauEucWLSnug = exp(fitGauEucWLSnug$par)[3]} else
  nuggGauEucWLSnug = 0
SigGauEucWLSnug = psilGauEucWLSnug*GauMod(EdisMat,rangGauEucWLSnug) +
  diag(rep(nuggGauEucWLSnug, times = dim(EdisMat)[1]))
CrosValGauEucWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigGauEucWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Cauchy Model with Euclidean Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitCauEucWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgmE.out$svgm,
  dist = empsvgmE.out$mndist, npair = empsvgmE.out$npair,
  aCorMod = CauMod, useNugget = useNugget)
psilCauEucWLSnug = exp(fitCauEucWLSnug$par)[1]
rangCauEucWLSnug = exp(fitCauEucWLSnug$par)[2]
if(useNugget == TRUE) {nuggCauEucWLSnug = exp(fitCauEucWLSnug$par)[3]} else
  nuggCauEucWLSnug = 0
SigCauEucWLSnug = psilCauEucWLSnug*CauMod(EdisMat,rangCauEucWLSnug) +
  diag(rep(nuggCauEucWLSnug, times = dim(EdisMat)[1]))
CrosValCauEucWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigCauEucWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Hole Effect Model with Euclidean Distance, Nugget, WLS fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitHolEucWLSnug = optim(theta, vgrmFitCWLS, empsvgm = empsvgmE.out$svgm,
  dist = empsvgmE.out$mndist, npair = empsvgmE.out$npair,
  aCorMod = HolMod, useNugget = useNugget)
psilHolEucWLSnug = exp(fitHolEucWLSnug$par)[1]
rangHolEucWLSnug = exp(fitHolEucWLSnug$par)[2]
if(useNugget == TRUE) {nuggHolEucWLSnug = exp(fitHolEucWLSnug$par)[3]} else
  nuggHolEucWLSnug = 0
SigHolEucWLSnug = psilHolEucWLSnug*HolMod(EdisMat,rangHolEucWLSnug) +
  diag(rep(nuggHolEucWLSnug, times = dim(EdisMat)[1]))
CrosValHolEucWLSnug = CVsummStats(NULL, rd$non_motorised, 
  LOOCV(SigHolEucWLSnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Exponential Model with Euclidean Distance, Nugget, REML fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitExpEucREMLnug = optim(theta, m2LL, z = rd$non_motorised, Dist = EdisMat, 
  aCorMod = ExpMod)
psilExpEucREMLnug = exp(fitExpEucREMLnug$par)[1]
rangExpEucREMLnug = exp(fitExpEucREMLnug$par)[2]
nuggExpEucREMLnug = exp(fitExpEucREMLnug$par)[3]
SigExpEucREMLnug = psilExpEucREMLnug*ExpMod(EdisMat,rangExpEucREMLnug) +
  diag(rep(nuggExpEucREMLnug, times = dim(EdisMat)[1]))
CrosValExpEucREMLnug = CVsummStats(fitExpEucREMLnug, rd$non_motorised, 
  LOOCV(SigExpEucREMLnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Spherical Model with Euclidean Distance, Nugget, REML fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitSphEucREMLnug = optim(theta, m2LL, z = rd$non_motorised, Dist = EdisMat, 
  aCorMod = SphMod)
psilSphEucREMLnug = exp(fitSphEucREMLnug$par)[1]
rangSphEucREMLnug = exp(fitSphEucREMLnug$par)[2]
nuggSphEucREMLnug = exp(fitSphEucREMLnug$par)[3]
SigSphEucREMLnug = psilSphEucREMLnug*SphMod(EdisMat,rangSphEucREMLnug) +
  diag(rep(nuggSphEucREMLnug, times = dim(EdisMat)[1]))
CrosValSphEucREMLnug = CVsummStats(fitSphEucREMLnug, rd$non_motorised, 
  LOOCV(SigSphEucREMLnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Gaussian Model with Euclidean Distance, Nugget, REML fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitGauEucREMLnug = optim(theta, m2LL, z = rd$non_motorised, Dist = EdisMat, 
  aCorMod = GauMod)
psilGauEucREMLnug = exp(fitGauEucREMLnug$par)[1]
rangGauEucREMLnug = exp(fitGauEucREMLnug$par)[2]
nuggGauEucREMLnug = exp(fitGauEucREMLnug$par)[3]
SigGauEucREMLnug = psilGauEucREMLnug*GauMod(EdisMat,rangGauEucREMLnug) +
  diag(rep(nuggGauEucREMLnug, times = dim(EdisMat)[1]))
CrosValGauEucREMLnug = CVsummStats(fitGauEucREMLnug, rd$non_motorised, 
  LOOCV(SigGauEucREMLnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Cauchy Model with Euclidean Distance, Nugget, REML fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitCauEucREMLnug = optim(theta, m2LL, z = rd$non_motorised, Dist = EdisMat, 
  aCorMod = CauMod)
psilCauEucREMLnug = exp(fitCauEucREMLnug$par)[1]
rangCauEucREMLnug = exp(fitCauEucREMLnug$par)[2]
nuggCauEucREMLnug = exp(fitCauEucREMLnug$par)[3]
SigCauEucREMLnug = psilCauEucREMLnug*CauMod(EdisMat,rangCauEucREMLnug) +
  diag(rep(nuggCauEucREMLnug, times = dim(EdisMat)[1]))
CrosValCauEucREMLnug = CVsummStats(fitCauEucREMLnug, rd$non_motorised, 
  LOOCV(SigCauEucREMLnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#    Hole Effect Model with Euclidean Distance, Nugget, REML fit
# -----------------------------------------------------------------------------

useNugget = TRUE
theta = log(c(4,10000,1))
if(useNugget == FALSE) theta = theta[1:2]
fitHolEucREMLnug = optim(theta, m2LL, z = rd$non_motorised, Dist = EdisMat, 
  aCorMod = HolMod)
psilHolEucREMLnug = exp(fitHolEucREMLnug$par)[1]
rangHolEucREMLnug = exp(fitHolEucREMLnug$par)[2]
nuggHolEucREMLnug = exp(fitHolEucREMLnug$par)[3]
SigHolEucREMLnug = psilHolEucREMLnug*HolMod(EdisMat,rangHolEucREMLnug) +
  diag(rep(nuggHolEucREMLnug, times = dim(EdisMat)[1]))
CrosValHolEucREMLnug = CVsummStats(fitHolEucREMLnug, rd$non_motorised, 
  LOOCV(SigHolEucREMLnug,rd$non_motorised))

# -----------------------------------------------------------------------------
#               Reduce Rank Exponential Model
# -----------------------------------------------------------------------------
theta = c(log(2), log(15000), log(10000), log(.7))
RRexpEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist)
sigmapRRexp = exp(RRexpEst$par)[1]
alphaRRexp = exp(RRexpEst$par)[2]
rhoRRexp = exp(RRexpEst$par)[3]
sigma0RRexp = exp(RRexpEst$par)[4]
SigRRexp = sigmapRRexp^2*exp(-rrDist/alphaRRexp) %*% 
  solve(exp(-knDist/rhoRRexp)) %*% t(exp(-rrDist/alphaRRexp)) + 
	diag(rep(sigma0RRexp^2, times = length(rrDist[,1])))
CVRRexp = CVsummStats(RRexpEst, rd$non_motorised, 
  LOOCV(SigRRexp,rd$non_motorised))

# -----------------------------------------------------------------------------
#               Reduce Rank Spherical Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
RRsphEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'sph')
sigmapRRsph = exp(RRsphEst$par)[1]
alphaRRsph = exp(RRsphEst$par)[2]
rhoRRsph = exp(RRsphEst$par)[3]
sigma0RRsph = exp(RRsphEst$par)[4]
SigRRsph = sigmapRRsph^2*acor.sph(rrDist,alphaRRsph) %*% 
	solve(acor.sph(knDist,rhoRRsph)) %*% 
	t(acor.sph(rrDist,alphaRRsph)) + 
	diag(rep(sigma0RRsph^2, times = length(rrDist[,1])))
CVRRsph = CVsummStats(RRsphEst, rd$non_motorised, 
  LOOCV(SigRRsph,rd$non_motorised))

# -----------------------------------------------------------------------------
#               Reduce Rank Gaussian Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
RRgauEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'gau')
sigmapRRgau = exp(RRgauEst$par)[1]
alphaRRgau = exp(RRgauEst$par)[2]
rhoRRgau = exp(RRgauEst$par)[3]
sigma0RRgau = exp(RRgauEst$par)[4]
SigRRgau = sigmapRRgau^2*acor.gau(rrDist,alphaRRgau) %*% 
	solve(acor.gau(knDist,rhoRRgau)) %*% 
	t(acor.gau(rrDist,alphaRRgau)) + 
	diag(rep(sigma0RRgau^2, times = length(rrDist[,1])))
CVRRgau = CVsummStats(RRgauEst, rd$non_motorised, 
  LOOCV(SigRRgau,rd$non_motorised))

# -----------------------------------------------------------------------------
#               Reduce Rank Cauchy Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
RRcauEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'cau')
sigmapRRcau = exp(RRcauEst$par)[1]
alphaRRcau = exp(RRcauEst$par)[2]
rhoRRcau = exp(RRcauEst$par)[3]
sigma0RRcau = exp(RRcauEst$par)[4]
SigRRcau = sigmapRRcau^2*acor.cau(rrDist,alphaRRcau) %*% 
	solve(acor.cau(knDist,rhoRRcau)) %*% 
	t(acor.cau(rrDist,alphaRRcau)) + 
	diag(rep(sigma0RRcau^2, times = length(rrDist[,1])))
CVRRcau = CVsummStats(RRcauEst, rd$non_motorised, 
  LOOCV(SigRRcau,rd$non_motorised))


# -----------------------------------------------------------------------------
#    Variance Components: Reduce Rank + Euclidean Spherical Models
# -----------------------------------------------------------------------------

theta = c(log(1), log(20000), log(1), log(20000), log(1), log(20000))
VCsphEst = optim(theta, m2LLvcSph, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, EdisMat = EdisMat)
sigmapVCsphLin = exp(VCsphEst$par)[1]
alphaVCsphLin = exp(VCsphEst$par)[2]
sigmapVCsphEuc = exp(VCsphEst$par)[3]
alphaVCsphKnt = exp(VCsphEst$par)[4]
sigma0VCsph = exp(VCsphEst$par[5])
alphaVCsphEuc = exp(VCsphEst$par[6])
SigVCsph = diag(rep(sigma0VCsph, times = dim(ldisMat)[1])) + 
  sigmapVCsphLin*SphMod(rrDist,alphaVCsphLin)%*%
  solve(SphMod(knDist,alphaVCsphKnt))%*%
  t(SphMod(rrDist,alphaVCsphLin)) + 
  sigmapVCsphEuc*SphMod(EdisMat,alphaVCsphEuc)
CVVCsph = CVsummStats(VCsphEst, rd$non_motorised, 
  LOOCV(SigVCsph,rd$non_motorised))

