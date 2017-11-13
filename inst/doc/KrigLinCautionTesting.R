## ----echo = FALSE, include = FALSE---------------------------------------
################################################################################
################################################################################
# smaller font size for chunks
options(scipen = 8)
################################################################################
################################################################################

## ----loadLibrary, echo = FALSE, include = FALSE--------------------------
################################################################################
################################################################################
  # load the library anew each time
  library(KrigLinCaution)
################################################################################
################################################################################

## ----autocorrModels, echo=FALSE, include = FALSE, fig.height = 11, fig.width = 11, cache = TRUE----
################################################################################
################################################################################
layout(matrix(c(1,2,3,3), nrow = 2, byrow = TRUE))
alpha = 3
par(mar = c(6,6,5,1))
plot((1:100)/10,2*acor.exp((1:100)/10,alpha), type = 'l', lwd = 3,
	xlab = 'Distance', ylab = 'Autocovariance', ylim = c(-1,3),
	col = '#e41a1c', cex.lab = 2, cex.axis = 1.5)
points(0,3, pch = 19, cex = 2)
lines((1:100)/10,2*acor.sph((1:100)/10,alpha), lwd = 3,	col = '#4daf4a')
lines((1:100)/10,2*acor.gau((1:100)/10,alpha), lwd = 3,	col = '#377eb8')
lines((1:100)/10,2*acor.cau((1:100)/10,alpha), lwd = 3,	col = '#984ea3')
lines((1:100)/10,2*sin(10*((1:100)/10)/alpha)/(10*((1:100)/10)/alpha), lwd = 3,	col = '#ff7f00')
legend(4.5,3, legend = c('Exponential','Gaussian','Spherical','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 1.8,
	col = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))
mtext('(a)', adj = -.15, padj = -.5, cex = 3)

par(mar = c(6,6,5,1))
plot((1:100)/10,3-2*acor.exp((1:100)/10,alpha), type = 'l', lwd = 3,
	xlab = 'Distance', ylab = 'Semivariance', ylim = c(0,4),
	col = '#e41a1c', cex.lab = 2, cex.axis = 1.5)
points(0,0, pch = 19, cex = 2)
lines((1:100)/10,3-2*acor.sph((1:100)/10,alpha), lwd = 3,	col = '#4daf4a')
lines((1:100)/10,3-2*acor.gau((1:100)/10,alpha), lwd = 3,	col = '#377eb8')
lines((1:100)/10,3-2*acor.cau((1:100)/10,alpha), lwd = 3,	col = '#984ea3')
lines((1:100)/10,3-2*sin(10*((1:100)/10)/alpha)/(10*((1:100)/10)/alpha), lwd = 3,	col = '#ff7f00')
mtext('(b)', adj = -.15, padj = -.5, cex = 3)

par(mar = c(6,6,5,1))
plot((1:1000)/100,acor.exp((1:1000)/100,.5), type = 'l', lwd = 3,
	xlab = 'Distance', ylab = 'Autocorrelation', ylim = c(0,1),
	cex.lab = 2, cex.axis = 1.5)
lines((1:1000)/100,acor.exp((1:1000)/100,2), lty = 2, lwd = 3)
lines((1:1000)/100,acor.exp((1:1000)/100,8), lty = 3, lwd = 5)
legend(8,1, legend = c(expression(alpha == 0.5),expression(alpha == 2),expression(alpha == 8)),
	lty = c(1,2,3), lwd = c(3,3,5), cex = 2)
mtext('(c)', adj = -.06, padj = -.5, cex = 3)
par(mfrow=c(1,1))
################################################################################
################################################################################

## ----CautionEx, echo=FALSE, include = FALSE, fig.height = 18.5, fig.width = 12, cache = TRUE----
################################################################################
################################################################################
layout(matrix(1:6, nrow = 3, byrow = TRUE))
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#                 EXAMPLE 1
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# number of points
n = 11
# Draw a Cirle
par(mar = c(6,6,5,1))
plot(c(-1, 1), c(-1, 1), type = "n", bty = 'n', xlab = '', ylab ='',
	xaxt = 'n', yaxt = 'n')
# prepare "circle data"
radius <- 1
theta <- seq(0, 2 * pi, length = 200)
# draw the circle
lines(x = radius * cos(theta), y = radius * sin(theta),
	lwd = 3)
# put points on the circle
theta <- seq(0, 2 * pi, length = (n+1))
points(x = radius * cos(theta), y = radius * sin(theta),
	pch = 19, cex = 4)
mtext('(a)', adj = -.15, padj = -.5, cex = 3)

# Here is a simple little example with 5 points on a circle
# and the shortest path distances among them
# scaled by (2*pi/n), which just scales the range parameter
# so it won't affect PD of covariance matrix
Dis = matrix(c(
  0, 1, 2, 2, 1,
  1, 0, 1, 2, 2,
  2, 1, 0, 1, 2,
  2, 2, 1, 0, 1,
  1, 2, 2, 1, 0)
, nrow = 5)
# Generalize to any odd number of points

Dis = matrix(0, nrow = n, ncol = n)
basepattern = c(0,1:floor(n/2),rev(1:floor(n/2)))
Disc = basepattern
for(i in 2:n)
	Disc = rbind(Disc,c(basepattern[(n-i+2):n],basepattern[1:(n-i+1)]))
Dis = Disc*2*pi/n
 
ex1 = NULL
alpha = 1
for(i in 1:200) {
  alpha = 3*i/200
  # exponential model
  mexp = minEigVal(acor.exp, Dis, alpha)
  # Gaussian model
  mgau = minEigVal(acor.gau, Dis, alpha)  
	# Spherical
  msph = minEigVal(acor.sph, Dis, alpha)
  # Cauchy
  mcau = minEigVal(acor.cau, Dis, alpha)
	# Hole effect
  mhol = minEigVal(acor.hol, Dis, alpha)
	ex1 = rbind(ex1, data.frame(alpha = alpha,
		mexp = mexp, mgau = mgau, msph = msph, mcau = mcau, mhol = mhol))
}
par(mar = c(6,6,5,1))
plot(ex1$alpha, ex1$mexp, ylim = c(-.5,1), type = 'l', lwd = 4,
	xlab = 'Alpha', ylab = 'Minimum Eigenvalue', col = '#e41a1c',
	cex.lab = 2.5, cex.axis = 2)
lines(ex1$alpha, ex1$mgau, col = '#377eb8', lwd = 4)
lines(ex1$alpha, ex1$msph, col = '#4daf4a', lwd = 4)
lines(ex1$alpha, ex1$mcau, col = '#984ea3', lwd = 4)
lines(ex1$alpha, ex1$mhol, col = '#ff7f00', lwd = 4)
lines(c(min(ex1$alpha), max(ex1$alpha)),c(0,0), lty = 2, lwd = 2)
legend(1.35,1, legend = c('Exponential','Gaussian','Spherical','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 2.5,
	col = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))
mtext('(b)', adj = -.15, padj = -.5, cex = 3)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#                 EXAMPLE 2
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

makeFigNetwork127()

n = 7
N = matrix(0,nrow = 2^n-1, ncol = 2^n-1)
for(i in 1:(2^(n-1)-1))
	N[(2*i):(2*i+1),i] = c(1,1)
N = N + t(N)

NeiNei = function(N, times)
{
	NN = N
	for(i in 2:times)
		NN = NN %*% N
	NN = (NN > 0)*1
	diag(NN) = rep(0, times = length(NN[,1]))
	NN
}
Nmats <- vector("list", 2*n-1)
Nmats[[1]] = N
i = 1
for(i in 2:(2*n-1)) {
	Ntemp = NeiNei(N, i)
	for(j in 1:(i-1))
	  if(any((Nmats[[j]] > 0) & (Ntemp > 0)))
      Ntemp[(Nmats[[j]] > 0) & (Ntemp > 0)] = 0
	Nmats[[i]] = Ntemp
}
Dis = N
for(i in 2:(2*n-1))
  Dis = Dis + i*Nmats[[i]]

ex2 = NULL
alpha = 1
for(i in 1:500) {
  alpha = 7*i/500
  # exponential model
  mexp = minEigVal(acor.exp, Dis, alpha)
  # Gaussian model
  mgau = minEigVal(acor.gau, Dis, alpha)  
	# Spherical
  msph = minEigVal(acor.sph, Dis, alpha)
  # Cauchy
  mcau = minEigVal(acor.cau, Dis, alpha)
	# Hole effect
  mhol = minEigVal(acor.hol, Dis, alpha)
	ex2 = rbind(ex2, data.frame(alpha = alpha,
		mexp = mexp, mgau = mgau, msph = msph, mcau = mcau, mhol = mhol))
}
par(mar = c(6,6,5,1))
plot(ex2$alpha, ex2$mexp, ylim = c(-1,1), type = 'l', lwd = 4,
	xlab = 'Alpha', ylab = 'Minimum Eigenvalue', col = '#e41a1c',
	cex.lab = 2.5, cex.axis = 2)
lines(ex2$alpha, ex2$mgau, col = '#377eb8', lwd = 4)
lines(ex2$alpha, ex2$msph, col = '#4daf4a', lwd = 4)
lines(ex2$alpha, ex2$mcau, col = '#984ea3', lwd = 4)
lines(ex2$alpha, ex2$mhol, col = '#ff7f00', lwd = 4)
lines(c(min(ex2$alpha), max(ex2$alpha)),c(0,0), lty = 2, lwd = 2)
mtext('(d)', adj = -.15, padj = -.5, cex = 3)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#                 EXAMPLE 3
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

n = 5
yg = kronecker(seq(1:n),rep(1, times = n))
xg = kronecker(rep(1, times = n),seq(1:n))
N = as.matrix(dist(cbind(xg,yg), diag = TRUE, upper = TRUE) )
I=diag(n^2)

N[N < 1.1] = 1
N[N > 1.1] = 0
diag(N) = 0
N[1,2] = N[2,1] = 0
N[14,15] = N[15,14] = 0
N[12,17] = N[17,12] = 0
N[17,18] = N[18,17] = 0

Nmats <- vector("list", 8)
Nmats[[1]] = N
for(i in 2:8) {
	Ntemp = NeiNei(N, i)
	for(j in 1:(i-1))
    Ntemp[(Nmats[[j]] > 0) & (Ntemp > 0)] = 0
	Nmats[[i]] = Ntemp
}
Dis = N
for(i in 2:8)
  Dis = Dis + i*Nmats[[i]]

ind = as.vector(N) == 1
xy1 = cbind(kronecker(xg, rep(1, times = n^2)),kronecker(yg, rep(1, times = n^2)))
xy2 = cbind(kronecker(rep(1, times = n^2), xg),kronecker(rep(1, times = n^2), yg))

par(mar = c(6,6,5,1))
plot(xg,yg, pch = 16, cex = 3, xlab = 'Column', ylab = 'Row', cex.lab = 3,
cex.axis = 2, xlim = c(.8,5.2), ylim = c(.8, 5.2))
for(i in 1:length(ind))
  if(ind[i] == 1)
    lines(c(xy1[i,1],xy2[i,1]),c(xy1[i,2],xy2[i,2]), lwd = 4)
lines(c(1,2),c(1,1), lty = 2, lwd = 2)
lines(c(2,2),c(3,4), lty = 2, lwd = 2)
lines(c(2,3),c(4,4), lty = 2, lwd = 2)
lines(c(4,5),c(3,3), lty = 2, lwd = 2)
points(xg,yg, pch = 16, cex = 8, col = 'white')
points(xg,yg, pch = 1, cex = 8)
text(xg,yg,as.character(1:25), cex = 2.5)
mtext('(e)', adj = -.15, padj = -.5, cex = 3)

ex3 = NULL
alpha = 1
for(i in 1:200) {
  alpha = 8*i/200
  # exponential model
  mexp = minEigVal(acor.exp, Dis, alpha)
  # Gaussian model
  mgau = minEigVal(acor.gau, Dis, alpha)  
	# Spherical
  msph = minEigVal(acor.sph, Dis, alpha)
  # Cauchy
  mcau = minEigVal(acor.cau, Dis, alpha)
	# Hole effect
  mhol = minEigVal(acor.hol, Dis, alpha)
	ex3 = rbind(ex3, data.frame(alpha = alpha,
		mexp = mexp, mgau = mgau, msph = msph, mcau = mcau, mhol = mhol))
}
par(mar = c(6,6,5,1))
plot(ex3$alpha, ex3$mexp, ylim = c(-1,1), type = 'l', lwd = 3,
	xlab = 'Alpha', ylab = 'Minimum Eigenvalue', col = '#e41a1c',
	cex.lab = 2.5, cex.axis = 2)
lines(ex3$alpha, ex3$mgau, col = '#377eb8', lwd = 3)
lines(ex3$alpha, ex3$msph, col = '#4daf4a', lwd = 3)
lines(ex3$alpha, ex3$mcau, col = '#984ea3', lwd = 3)
lines(ex3$alpha, ex3$mhol, col = '#ff7f00', lwd = 3)
lines(c(min(ex3$alpha), max(ex3$alpha)),c(0,0), lty = 2, lwd = 2)
mtext('(f)', adj = -.15, padj = -.5, cex = 3)
par(mfrow=c(1,1))
################################################################################
################################################################################

## ----realLinDistEigVals, echo=FALSE, include = FALSE, fig.height = 11, fig.width = 8, cache = TRUE----
################################################################################
################################################################################
data(semivarTable)
#linear network distance matrix
ldisMat = matrix(semivar.table$Network_length, nrow = 239, ncol = 239)
#Euclidean distance matrix
EdisMat = matrix(semivar.table$Euclidean_length, nrow = 239, ncol = 239)
# get and apply names to keep things straight
matNames = matrix(semivar.table$Origin, nrow = 239, ncol = 239)[1,]
rownames(EdisMat) = matNames
colnames(EdisMat) = matNames
rownames(ldisMat) = matNames
colnames(ldisMat) = matNames

# try various models on linear network distances
exReal = NULL
for(i in 1:100) {
  alpha = 40000*i^2/100^2
  # exponential model
  mexp = min(eigen(exp(-ldisMat/alpha))$values)
  # Gaussian model
  mgau = min(eigen(exp(-(ldisMat/alpha)^2))$values)
  # Spherical
  msph = min(eigen((1 - 1.5*ldisMat/alpha + .5*ldisMat^3/alpha^3)*(ldisMat < alpha))$values)
  # Cauchy
  mrqu = min(eigen(1/(1 + (ldisMat/alpha)^2))$values)
	# Hole effect
	mhol = sin(ldisMat/alpha)/(ldisMat/alpha)
	diag(mhol) = rep(1, times = length(mhol[,1]))
	mhol = min(eigen(mhol)$values)
	exReal = rbind(exReal, data.frame(alpha = alpha,
		mexp = mexp, mgau = mgau, msph = msph, mrqu = mrqu, mhol = mhol))
}
# try various models on Euclidean distances
EucReal = NULL
for(i in 1:100) {
  alpha = 40000*i^2/100^2
  # exponential model
  mexp = min(eigen(exp(-EdisMat/alpha))$values)
  # Gaussian model
  mgau = min(eigen(exp(-(EdisMat/alpha)^2))$values)
  # Spherical
  msph = min(eigen((1 - 1.5*EdisMat/alpha + .5*EdisMat^3/alpha^3)*(EdisMat < alpha))$values)
  # Cauchy
  mrqu = min(eigen(1/(1 + (EdisMat/alpha)^2))$values)
	# Hole effect
	mhol = sin(EdisMat/alpha)/(EdisMat/alpha)
	diag(mhol) = rep(1, times = length(mhol[,1]))
	mhol = min(eigen(mhol)$values)
	EucReal = rbind(EucReal, data.frame(alpha = alpha,
		mexp = mexp, mgau = mgau, msph = msph, mrqu = mrqu, mhol = mhol))
}

# plot the results
layout(matrix(1:2, nrow = 2))
par(mar = c(6,6,5,1))
plot(exReal$alpha, exReal$mexp, ylim = c(-.1,1), type = 'l', lwd = 3,
	xlab = 'Alpha', ylab = 'Minimum Eigenvalue', col = '#e41a1c',
	cex.lab = 2, cex.axis = 1.5)
lines(exReal$alpha, exReal$mgau, col = '#377eb8', lwd = 3)
lines(exReal$alpha, exReal$msph, col = '#4daf4a', lwd = 3)
lines(exReal$alpha, exReal$mrqu, col = '#984ea3', lwd = 3)
lines(exReal$alpha, exReal$mhol, col = '#ff7f00', lwd = 3)
lines(c(min(exReal$alpha), max(exReal$alpha)),c(0,0), lty = 2, lwd = 2)
legend(20000,1, legend = c('Exponential','Gaussian','Spherical','Cauchy','Hole Effect'),
	lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 2,
	col = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'))
mtext('(a)', adj = -.10, padj = -.6, cex = 3)

par(mar = c(6,6,5,1))
plot(EucReal$alpha, EucReal$mexp, ylim = c(-.1,1), type = 'l', lwd = 3,
	xlab = 'Alpha', ylab = 'Minimum Eigenvalue', col = '#e41a1c',
	cex.lab = 2, cex.axis = 1.5)
lines(EucReal$alpha, EucReal$mgau, col = '#377eb8', lwd = 3)
lines(EucReal$alpha, EucReal$msph, col = '#4daf4a', lwd = 3)
lines(EucReal$alpha, EucReal$mrqu, col = '#984ea3', lwd = 3)
lines(EucReal$alpha, EucReal$mhol, col = '#ff7f00', lwd = 3)
lines(c(min(EucReal$alpha), max(EucReal$alpha)),c(0,0), lty = 2, lwd = 2)
mtext('(b)', adj = -.10, padj = -.6, cex = 3)
par(mfrow=c(1,1))
################################################################################
################################################################################

## ----reduRank, echo=FALSE, include = FALSE, cache = TRUE-----------------
################################################################################
################################################################################
# create knot locations using kmeans clustering on spatial coordinates
data(semivarTable)
locs = unique(data.frame(semivar.table$Origin, semivar.table$X_origin,semivar.table$Y_origin))
par(mar = c(5,5,1,1))
plot(locs[,2:3], cex = 2, pch = 1, xlab = 'x coordinate', ylab = 'y coordinate',
  cex.lab = 2, cex.axis = 1.5)

cbind(as.character(locs[,1]),matNames)

# create spatial knots using kmeans
set.seed(1001) # kmeans uses random starting points.  This ensures repeatability.
nknots = 120
km = kmeans(locs[,2:3],nknots,40)
points(km$centers, pch = 19, cex = 1.5, col = 'blue')
# find nearest observed site on network
dall = as.matrix(dist(rbind(locs[,2:3],km$centers), diag = TRUE, upper = TRUE))
mins = apply(dall[1:239,(239+1):(239+nknots)],2,min)
knots = NULL
for(i in 1:nknots)
  knots = rbind(knots, locs[which(dall[1:239,239 + i] == mins[i])[1],])
points(knots[,2:3], col = 'red', pch = 19)
rownames(knots) = as.character(knots[,1])
################################################################################
################################################################################

## ----rrFits, echo=FALSE, include = FALSE, cache = TRUE-------------------
################################################################################
################################################################################
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


###########################   Motorised Data

# -----------------------------------------------------------------------------
#               Reduce Rank Exponential Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRexpEst = optim(theta, m2LLrr, z = rd$motorised,  
	rrDist = rrDist, knDist = knDist)
end = Sys.time()
REMLtimeFast = end - start

sigmapRRexp = exp(RRexpEst$par)[1]
alphaRRexp = exp(RRexpEst$par)[2]
rhoRRexp = exp(RRexpEst$par)[3]
sigma0RRexp = exp(RRexpEst$par)[4]

SigMat = sigmapRRexp^2*exp(-rrDist/alphaRRexp) %*% solve(exp(-knDist/rhoRRexp)) %*% t(exp(-rrDist/alphaRRexp)) + 
		diag(rep(sigma0RRexp^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
n = length(rd$motorised)
CVoutRRexp = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#               Reduce Rank Spherical Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRsphEst = optim(theta, m2LLrr, z = rd$motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'sph')
end = Sys.time()
end - start

sigmapRRsph = exp(RRsphEst$par)[1]
alphaRRsph = exp(RRsphEst$par)[2]
rhoRRsph = exp(RRsphEst$par)[3]
sigma0RRsph = exp(RRsphEst$par)[4]

SigMat = sigmapRRsph^2*acor.sph(rrDist,alphaRRsph) %*% 
	solve(acor.sph(knDist,rhoRRsph)) %*% 
	t(acor.sph(rrDist,alphaRRsph)) + 
	diag(rep(sigma0RRsph^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
n = length(rd$motorised)
CVoutRRsph = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#               Reduce Rank Gaussian Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRgauEst = optim(theta, m2LLrr, z = rd$motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'gau')
end = Sys.time()
end - start

sigmapRRgau = exp(RRgauEst$par)[1]
alphaRRgau = exp(RRgauEst$par)[2]
rhoRRgau = exp(RRgauEst$par)[3]
sigma0RRgau = exp(RRgauEst$par)[4]

SigMat = sigmapRRgau^2*acor.gau(rrDist,alphaRRgau) %*% 
	solve(acor.gau(knDist,rhoRRgau)) %*% 
	t(acor.gau(rrDist,alphaRRgau)) + 
	diag(rep(sigma0RRgau^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
n = length(rd$motorised)
CVoutRRgau = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#               Reduce Rank Cauchy Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRcauEst = optim(theta, m2LLrr, z = rd$motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'cau')
end = Sys.time()
end - start

sigmapRRcau = exp(RRcauEst$par)[1]
alphaRRcau = exp(RRcauEst$par)[2]
rhoRRcau = exp(RRcauEst$par)[3]
sigma0RRcau = exp(RRcauEst$par)[4]

SigMat = sigmapRRcau^2*acor.cau(rrDist,alphaRRcau) %*% 
	solve(acor.cau(knDist,rhoRRcau)) %*% 
	t(acor.cau(rrDist,alphaRRcau)) + 
	diag(rep(sigma0RRcau^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
n = length(rd$motorised)
CVoutRRcau = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)


# -----------------------------------------------------------------------------
#               Original Ladle Model
# -----------------------------------------------------------------------------

V = 4.718*exp(-ldisMat/7620)
Vi = solve(V)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
CVout = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = V, 
	Vi = Vi, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#          Exponential Model with Linear Distance + Nugget
# -----------------------------------------------------------------------------

theta = log(c(4,10000,1))
start = Sys.time()
ExpLinNugEst = optim(theta, m2LLexp, z = rd$motorised,  
	EucDist = ldisMat)
end = Sys.time()
end - start

sigmapLinNugMot = exp(ExpLinNugEst$par)[1]
alphaLinNugMot = exp(ExpLinNugEst$par)[2]
sigma0LinNugMot = exp(ExpLinNugEst$par)[3]

SigMat = sigmapLinNugMot^2*exp(-EdisMat/alphaLinNugMot) + 
	diag(rep(sigma0LinNugMot^2, times = length(EdisMat[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
n = length(rd$motorised)
CVoutLinNugMot = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

AICLinNugMot = ExpLinNugEst$value + 6
corLinNugMot = cor(rd$motorised, CVoutLinNugMot[,1])
#RMSPE
RMSPELinNugMot = sqrt(mean((rd$motorised - CVoutLinNugMot[,1])^2))
# 90% Interval coverage
CI90LinNugMot = mean(CVoutLinNugMot[,1] - qnorm(.95)*CVoutLinNugMot[,2] < rd$motorised &  
	rd$motorised < CVoutLinNugMot[,1] + qnorm(.95)*CVoutLinNugMot[,2])


# -----------------------------------------------------------------------------
#          Exponential Model with Euclidean Distance + Nugget
# -----------------------------------------------------------------------------

theta = log(c(4,10000,1))
start = Sys.time()
LadNugEst = optim(theta, m2LLexp, z = rd$motorised,  
	EucDist = EdisMat)
end = Sys.time()
end - start

sigmapEucNugMot = exp(LadNugEst$par)[1]
alphaEucNugMot = exp(LadNugEst$par)[2]
sigma0EucNugMot = exp(LadNugEst$par)[3]

SigMat = sigmapEucNugMot^2*exp(-EdisMat/alphaEucNugMot) + 
	diag(rep(sigma0EucNugMot^2, times = length(EdisMat[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$motorised)
n = length(rd$motorised)
CVoutEucNugMot = crossVal(z = matrix(rd$motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

AICEucNugMot = LadNugEst$value + 6
corEucNugMot = cor(rd$motorised, CVoutEucNugMot[,1])
#RMSPE
RMSPEEucNugMot = sqrt(mean((rd$motorised - CVoutEucNugMot[,1])^2))
# 90% Interval coverage
CI90EucNugMot = mean(CVoutEucNugMot[,1] - qnorm(.95)*CVoutEucNugMot[,2] < rd$motorised &  
	rd$motorised < CVoutEucNugMot[,1] + qnorm(.95)*CVoutEucNugMot[,2])


###########################   Non-motorised Data

# -----------------------------------------------------------------------------
#               Reduce Rank Exponential Model
# -----------------------------------------------------------------------------
theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRnonexpEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist)
end = Sys.time()
end - start

sigmapRRnonexp = exp(RRnonexpEst$par)[1]
alphaRRnonexp = exp(RRnonexpEst$par)[2]
rhoRRnonexp = exp(RRnonexpEst$par)[3]
sigma0RRnonexp = exp(RRnonexpEst$par)[4]

SigMat = sigmapRRnonexp^2*exp(-rrDist/alphaRRnonexp) %*% solve(exp(-knDist/rhoRRnonexp)) %*% t(exp(-rrDist/alphaRRnonexp)) + 
		diag(rep(sigma0RRnonexp^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutRRnonexp = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#               Reduce Rank Spherical Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRnonsphEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'sph')
end = Sys.time()
end - start

sigmapRRnonsph = exp(RRnonsphEst$par)[1]
alphaRRnonsph = exp(RRnonsphEst$par)[2]
rhoRRnonsph = exp(RRnonsphEst$par)[3]
sigma0RRnonsph = exp(RRnonsphEst$par)[4]

SigMat = sigmapRRnonsph^2*acor.sph(rrDist,alphaRRnonsph) %*% 
	solve(acor.sph(knDist,rhoRRnonsph)) %*% 
	t(acor.sph(rrDist,alphaRRnonsph)) + 
	diag(rep(sigma0RRnonsph^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutRRnonsph = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#               Reduce Rank Gaussian Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRnongauEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'gau')
end = Sys.time()
end - start

sigmapRRnongau = exp(RRnongauEst$par)[1]
alphaRRnongau = exp(RRnongauEst$par)[2]
rhoRRnongau = exp(RRnongauEst$par)[3]
sigma0RRnongau = exp(RRnongauEst$par)[4]

SigMat = sigmapRRnongau^2*acor.gau(rrDist,alphaRRnongau) %*% 
	solve(acor.gau(knDist,rhoRRnongau)) %*% 
	t(acor.gau(rrDist,alphaRRnongau)) + 
	diag(rep(sigma0RRnongau^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutRRnongau = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#               Reduce Rank Cauchy Model
# -----------------------------------------------------------------------------

theta = c(log(2), log(15000), log(10000), log(.7))
start = Sys.time()
RRnoncauEst = optim(theta, m2LLrr, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, corMod = 'cau')
end = Sys.time()
end - start

sigmapRRnoncau = exp(RRnoncauEst$par)[1]
alphaRRnoncau = exp(RRnoncauEst$par)[2]
rhoRRnoncau = exp(RRnoncauEst$par)[3]
sigma0RRnoncau = exp(RRnoncauEst$par)[4]

SigMat = sigmapRRnoncau^2*acor.cau(rrDist,alphaRRnoncau) %*% 
	solve(acor.cau(knDist,rhoRRnoncau)) %*% 
	t(acor.cau(rrDist,alphaRRnoncau)) + 
	diag(rep(sigma0RRnoncau^2, times = length(rrDist[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutRRnoncau = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)


# -----------------------------------------------------------------------------
#               Original Ladle Model
# -----------------------------------------------------------------------------

V = 5.093*exp(-ldisMat/14245)
Vi = solve(V)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
CVnonout = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = V, 
	Vi = Vi, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# -----------------------------------------------------------------------------
#          Exponential Model with Linear Distance + Nugget
# -----------------------------------------------------------------------------

theta = log(c(4,10000,1))
start = Sys.time()
ExpLinNugNonEst = optim(theta, m2LLexp, z = rd$non_motorised,  
	EucDist = ldisMat)
end = Sys.time()
end - start

sigmapLinNugNon = exp(ExpLinNugNonEst$par)[1]
alphaLinNugNon = exp(ExpLinNugNonEst$par)[2]
sigma0LinNugNon = exp(ExpLinNugNonEst$par)[3]

SigMat = sigmapLinNugNon^2*exp(-EdisMat/alphaLinNugNon) + 
	diag(rep(sigma0LinNugNon^2, times = length(EdisMat[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutLinNugNon = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

AICLinNugNon = ExpLinNugNonEst$value + 6
corLinNugNon = cor(rd$non_motorised, CVoutLinNugNon[,1])
#RMSPE
RMSPELinNugNon = sqrt(mean((rd$non_motorised - CVoutLinNugNon[,1])^2))
# 90% Interval coverage
CI90LinNugNon = mean(CVoutLinNugNon[,1] - qnorm(.95)*CVoutLinNugNon[,2] < rd$non_motorised &  
	rd$non_motorised < CVoutLinNugNon[,1] + qnorm(.95)*CVoutLinNugNon[,2])


# -----------------------------------------------------------------------------
#       Exponential Model with Linear Distance plus Nugget
# -----------------------------------------------------------------------------

theta = log(c(4,10000,1))
start = Sys.time()
LadNugEstNon = optim(theta, m2LLexp, z = rd$non_motorised,  
	EucDist = EdisMat)
end = Sys.time()
end - start

sigmapEucNugNon = exp(LadNugEstNon$par)[1]
alphaEucNugNon = exp(LadNugEstNon$par)[2]
sigma0EucNugNon = exp(LadNugEstNon$par)[3]

SigMat = sigmapEucNugNon^2*exp(-EdisMat/alphaEucNugNon) + 
	diag(rep(sigma0EucNugNon^2, times = length(EdisMat[,1])))

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$motorised)
CVoutEucNugNon = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

AICEucNugNon = LadNugEstNon$value + 6
corEucNugNon = cor(rd$non_motorised, CVoutEucNugNon[,1])
#RMSPE
RMSPEEucNugNon = sqrt(mean((rd$non_motorised - CVoutEucNugNon[,1])^2))
# 90% Interval coverage
CI90EucNugNon = mean(CVoutEucNugNon[,1] - qnorm(.95)*CVoutEucNugNon[,2] < rd$non_motorised &  
	rd$non_motorised < CVoutEucNugNon[,1] + qnorm(.95)*CVoutEucNugNon[,2])


## ----CVplots, echo=FALSE, include = FALSE--------------------------------

CVsummStats = function(optimFit, obsVec, CVtable)
{
  list(
    AIC = optimFit$value + 2*length(optimFit$par) + 2,
    corr = cor(obsVec, CVtable[,1]),
    RMSPE = sqrt(mean((obsVec - CVtable[,1])^2)),
    CI90 = mean(CVtable[,1] - qnorm(.95)*sqrt(CVtable[,2]) < obsVec &  
                obsVec < CVtable[,1] + qnorm(.95)*sqrt(CVtable[,2]))
  )
}

CVsummStats(RRexpEst, rd$motorised, CVoutRRexp)
# AIC, corr, RMSPE, and CI90 for RRexp model for motorised data
AICexp = RRexpEst$value + 8
corRRexp = cor(rd$motorised, CVoutRRexp[,1])
RMSPERRexp = sqrt(mean((rd$motorised - CVoutRRexp[,1])^2))
CI90RRexp = mean(CVoutRRexp[,1] - qnorm(.95)*sqrt(CVoutRRexp[,2]) < rd$motorised &  
	rd$motorised < CVoutRRexp[,1] + qnorm(.95)*CVoutRRexp[,2])

# AIC, corr, RMSPE, and CI90 for RRsph model for motorised data
AICsph = RRsphEst$value + 8
corRRsph = cor(rd$motorised, CVoutRRsph[,1])
RMSPERRsph = sqrt(mean((rd$motorised - CVoutRRsph[,1])^2))
CI90RRsph = mean(CVoutRRsph[,1] - qnorm(.95)*CVoutRRsph[,2] < rd$motorised &  
	rd$motorised < CVoutRRsph[,1] + qnorm(.95)*CVoutRRsph[,2])

# AIC, corr, RMSPE, and CI90 for RRgau model for motorised data
AICgau = RRgauEst$value + 8
corRRgau = cor(rd$motorised, CVoutRRgau[,1])
RMSPERRgau = sqrt(mean((rd$motorised - CVoutRRgau[,1])^2))
CI90RRgau = mean(CVoutRRgau[,1] - qnorm(.95)*CVoutRRgau[,2] < rd$motorised &  
	rd$motorised < CVoutRRgau[,1] + qnorm(.95)*CVoutRRgau[,2])

# AIC, corr, RMSPE, and CI90 for RRcau model for motorised data
AICcau = RRcauEst$value + 8
corRRcau = cor(rd$motorised, CVoutRRcau[,1])
RMSPERRcau = sqrt(mean((rd$motorised - CVoutRRcau[,1])^2))
CI90RRcau = mean(CVoutRRcau[,1] - qnorm(.95)*CVoutRRcau[,2] < rd$motorised &  
	rd$motorised < CVoutRRcau[,1] + qnorm(.95)*CVoutRRcau[,2])

# AIC, corr, RMSPE, and CI90 for LinEN model for motorised data
corLKexp= cor(rd$motorised, CVout[,1])
RMSPELKexp = sqrt(mean((rd$motorised - CVout[,1])^2))
CI90LKexp = mean(CVout[,1] - qnorm(.95)*CVout[,2] < rd$motorised &  
	rd$motorised < CVout[,1] + qnorm(.95)*CVout[,2])

# AIC, corr, RMSPE, and CI90 for RRexp model for non-motorised data
AICnonexp = RRnonexpEst$value + 8
corRRnonexp = cor(rd$non_motorised, CVoutRRnonexp[,1])
RMSPERRnonexp = sqrt(mean((rd$non_motorised - CVoutRRnonexp[,1])^2))
CI90RRnonexp = mean(CVoutRRnonexp[,1] - qnorm(.95)*CVoutRRnonexp[,2] < rd$non_motorised &  
	rd$non_motorised < CVoutRRnonexp[,1] + qnorm(.95)*CVoutRRnonexp[,2])

# AIC, corr, RMSPE, and CI90 for RRsph model for non-motorised data
AICnonsph = RRnonsphEst$value + 8
corRRnonsph = cor(rd$non_motorised, CVoutRRnonsph[,1])
RMSPERRnonsph = sqrt(mean((rd$non_motorised - CVoutRRnonsph[,1])^2))
CI90RRnonsph = mean(CVoutRRnonsph[,1] - qnorm(.95)*CVoutRRnonsph[,2] < rd$non_motorised &  
	rd$non_motorised < CVoutRRnonsph[,1] + qnorm(.95)*CVoutRRnonsph[,2])

# AIC, corr, RMSPE, and CI90 for RRgau model for non-motorised data
AICnongau = RRnongauEst$value + 8
corRRnongau = cor(rd$non_motorised, CVoutRRnongau[,1])
RMSPERRnongau = sqrt(mean((rd$non_motorised - CVoutRRnongau[,1])^2))
CI90RRnongau = mean(CVoutRRnongau[,1] - qnorm(.95)*CVoutRRnongau[,2] < rd$non_motorised &  
	rd$non_motorised < CVoutRRnongau[,1] + qnorm(.95)*CVoutRRnongau[,2])

# AIC, corr, RMSPE, and CI90 for RRcau model for non-motorised data
AICnoncau = RRnoncauEst$value + 8
corRRnoncau = cor(rd$non_motorised, CVoutRRnoncau[,1])
RMSPERRnoncau = sqrt(mean((rd$non_motorised - CVoutRRnoncau[,1])^2))
CI90RRnoncau = mean(CVoutRRnoncau[,1] - qnorm(.95)*CVoutRRnoncau[,2] < rd$non_motorised &  
	rd$non_motorised < CVoutRRnoncau[,1] + qnorm(.95)*CVoutRRnoncau[,2])

# AIC, corr, RMSPE, and CI90 for LinEN model for non-motorised data
corLKnonexp= cor(rd$non_motorised, CVnonout[,1])
RMSPELKnonexp = sqrt(mean((rd$non_motorised - CVnonout[,1])^2))
CI90LKnonexp = mean(CVnonout[,1] - qnorm(.95)*CVnonout[,2] < rd$non_motorised &  
	rd$non_motorised < CVnonout[,1] + qnorm(.95)*CVnonout[,2])

## ----Tab-CVstats, echo= FALSE, include = FALSE---------------------------
  library(xtable)
  CVstats1 = as.matrix( 
    rbind( 
      c(4.718, 7620, NA, NA, NA, corLKexp, RMSPELKexp, CI90LKexp),
			c(sigmapLinNugMot, alphaLinNugMot, NA, sigma0LinNugMot, AICLinNugMot, corLinNugMot, RMSPELinNugMot, CI90LinNugMot),
			c(sigmapEucNugMot, alphaEucNugMot, NA, sigma0EucNugMot, AICEucNugMot, corEucNugMot, RMSPEEucNugMot, CI90EucNugMot),
      c(sigmapRRexp, alphaRRexp, rhoRRexp, sigma0RRexp, AICexp, corRRexp, RMSPERRexp, CI90RRexp),
			c(sigmapRRsph, alphaRRsph, rhoRRsph, sigma0RRsph, AICsph, corRRsph, RMSPERRsph, CI90RRsph),
			c(sigmapRRgau, alphaRRgau, rhoRRgau, sigma0RRgau, AICgau, corRRgau, RMSPERRgau, CI90RRgau),
			c(sigmapRRcau, alphaRRcau, rhoRRcau, sigma0RRcau, AICcau, corRRcau, RMSPERRcau, CI90RRcau)
    ) 
  ) 
  string = c("Ladle$^a$","LinEN$^b$","EucEN$^b$","RRexp$^c$", "RRsph$^c$", "RRgau$^c$", "RRcau$^c$")
  rownames(CVstats1) = string
	CVstats2 = as.matrix( 
    rbind( 
      c(5.093, 14245, NA, NA, NA, corLKnonexp, RMSPELKnonexp, CI90LKnonexp),
			c(sigmapLinNugNon, alphaLinNugNon, NA, sigma0LinNugNon, AICLinNugNon, corLinNugNon, RMSPELinNugNon, CI90LinNugNon),
			c(sigmapEucNugNon, alphaEucNugNon, NA, sigma0EucNugNon, AICEucNugNon, corEucNugNon, RMSPEEucNugNon, CI90EucNugNon),
			c(sigmapRRnonexp, alphaRRnonexp, rhoRRnonexp, sigma0RRnonexp, AICnonexp, corRRnonexp, RMSPERRnonexp, CI90RRnonexp),
			c(sigmapRRnonsph, alphaRRnonsph, rhoRRnonsph, sigma0RRnonsph, AICnonsph, corRRnonsph, RMSPERRnonsph, CI90RRnonsph),
			c(sigmapRRnongau, alphaRRnongau, rhoRRnongau, sigma0RRnongau, AICnongau, corRRnongau, RMSPERRnongau, CI90RRnongau),
			c(sigmapRRnoncau, alphaRRnoncau, rhoRRnoncau, sigma0RRnoncau, AICnoncau, corRRnoncau, RMSPERRnoncau, CI90RRnoncau)
    ) 
  ) 
  string = c("Ladle$^a$","LinEN$^b$","EucEN$^b$","RRexp$^c$", "RRsph$^c$", "RRgau$^c$", "RRcau$^c$")
  rownames(CVstats2) = string

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(CVstats1, 
      align = c('l',rep('l', times = length(CVstats1[1,]))),
      digits = c(0,2,0,0,2,2,3,3,3),
      caption = 'Cross-validation statistics',
      label = 'tab:CVstats'
    ),
    size = 'footnotesize',
    include.rownames = TRUE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  ) 

## ----results = 'asis', echo = FALSE--------------------------------------
  print(
    xtable(CVstats2, 
      align = c('l',rep('l', times = length(CVstats2[1,]))),
      digits = c(0,2,0,0,2,2,3,3,3),
      caption = 'Cross-validation statistics',
      label = 'tab:CVstats'
    ),
    size = 'footnotesize',
    include.rownames = TRUE,
    sanitize.rownames.function = identity,
    only.contents = TRUE,
    include.colnames = FALSE
  )

# -----------------------------------------------------------------------------
#       What can go Wrong?  Ladle data
# -----------------------------------------------------------------------------

#linear network distance matrix
ldisMat = matrix(semivar.table$Network_length, nrow = 239, ncol = 239)
#Euclidean distance matrix
EdisMat = matrix(semivar.table$Euclidean_length, nrow = 239, ncol = 239)

ldists = ldisMat[lower.tri(ldisMat)]
Edists = EdisMat[lower.tri(EdisMat)]
plot(Edists,ldists, pch = 19, cex = .8, xlim = c(0,max(ldists,Edists)),
  ylim = c(0,max(ldists,Edists)), xlab = 'Euclidean Distance',
  ylab = 'Linear Network Distance', cex.lab = 2, cex.axis = 1.5,
  col = rgb(0,0,0,.05))
lines(c(0,max(ldists,Edists)),c(0,max(ldists,Edists)), lty = 2,
  lwd = 3, col = 'blue')
  
empsvgm = function(z, distMat, breaks)
{
  # get squared differences among all pairs of points
  df2 <- (abs(z%o%rep(1, times = length(z)) - rep(1, times = length(z))%o%z))^2
  # remove duplicates from symmetric difference matrix
  # divide by 2 to get semivariances
  df2 = df2[lower.tri(df2)]/2
  # remove duplicated distances from symmetric distance matrix
  ldists = ldisMat[lower.tri(ldisMat)]
  # classify distances into distance bins
  cutvec = cut(ldists, breaks)
  out = data.frame(
    svgm = aggregate(df2, by = list(cutvec), mean)[,2],
    mndist = aggregate(ldists, by = list(cutvec), mean)[,2],
    npair = aggregate(df2, by = list(cutvec), function(x) {length(x)})[,2]
  )
  out
}

brks = c(0,10000,20000,30000,40000,50000,60000,max(ldists)+1)
empsvgm.out = empsvgm(z = rd$non_motorised, distMat = ldisMat, breaks = brks)
brks = c(0,10000,15000,20000,25000,30000,35000,40000,45000,50000,60000,max(ldists)+1)
empsvgm.out = empsvgm(z = rd$non_motorised, distMat = ldisMat, breaks = brks)

plot(empsvgm.out$mndist, empsvgm.out$svgm, 
  ylim = c(0,max(empsvgm.out$svgm)),
  xlim = c(0, max(empsvgm.out$mndist)),
  pch = 19, cex = empsvgm.out$npair/500,
  xlab = "Linear Network Distance",
  ylab = "Semivariogram", cex.axis = 1.5, cex.lab = 2)
#points(empsvgm.out1$mndist, empsvgm.out1$svgm,
#  pch = 19, cex = empsvgm.out1$npair/1000, col = '#377eb8')

sphMod = function(dist,psil,rang){
  (psil*(3*dist/(2*rang) - dist^3/(2*rang^3)))*(dist <= rang) +
  psil*(dist > rang)
}

WLSfuncSphNoNug = function(theta,empsvgm,dist,npair) {
  sum((empsvgm-sphMod(dist,theta[1],theta[2]))^2*npair)
}

theta = c(4,40000)
sphfitNoNug = optim(theta, WLSfuncSphNoNug, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair)
d4fit = 1:85000
lines(d4fit,sphMod(d4fit,sphfitNoNug$par[1],sphfitNoNug$par[2]), lwd = 4,
  col = '#4daf4a', lty  = 2)

WLSfuncSphNug = function(theta,empsvgm,dist,npair) {
  sum((empsvgm-(sphMod(dist,theta[1],theta[2])+theta[3]))^2*npair)
}
theta = c(2.5, 40000, 1.5)
sphfitNug = optim(theta, WLSfuncSphNug, empsvgm = empsvgm.out$svgm,
  dist = empsvgm.out$mndist, npair = empsvgm.out$npair)
lines(d4fit,sphMod(d4fit,sphfitNug$par[1],sphfitNug$par[2]) + sphfitNug$par[3], lwd = 4,
  col = '#4daf4a', lty  = 1)
points(0,0,pch = 19, cex = 2, col = '#4daf4a')

sphNoNugCovMat = sphfitNoNug$par[1] - sphMod(ldisMat,sphfitNoNug$par[1],sphfitNoNug$par[2])
min(eigen(sphNoNugCovMat)$values)

SigMat = sphNoNugCovMat
SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutSphNoNugWLS = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)
min(CVoutSphNoNugWLS[,2])
sum(CVoutSphNoNugWLS[,2] < 0)

SphNugCovMat = sphfitNug$par[1] - sphMod(ldisMat,sphfitNug$par[1],sphfitNug$par[2]) + diag(rep(sphfitNug$par[3], time = n))
min(eigen(SphNugCovMat)$values)

SigMat = SphNugCovMat
SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutSphNugWLS = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)
min(CVoutSphNugWLS[,2])
sum(CVoutSphNugWLS[,2] < 0)

corSphNugWLS = cor(rd$non_motorised, CVoutSphNugWLS[,1])
RMSPESphNugWLS = sqrt(mean((rd$non_motorised - CVoutSphNugWLS[,1])^2))
CI90SphNugWLS = mean(CVoutSphNugWLS[,1] - qnorm(.95)*sqrt(CVoutSphNugWLS[,2]) < rd$non_motorised &  
	rd$non_motorised < CVoutSphNugWLS[,1] + qnorm(.95)*sqrt(CVoutSphNugWLS[,2]))

GauMod = function(dist,psil,rang){
  psil*(1 - exp(-(dist/rang)^2))
}

WLSfuncGauNoNug = function(theta,empsvgm,dist,npair) {
  sum((empsvgm-GauMod(dist,theta[1],theta[2]))^2*npair)
}

theta = c(4,40000)
GaufitNoNug = optim(theta, WLSfuncGauNoNug, empsvgm = empsvgm.out1$svgm,
  dist = empsvgm.out1$mndist, npair = empsvgm.out1$npair)
d4fit = 1:85000
lines(d4fit,GauMod(d4fit,GaufitNoNug$par[1],Gaufit1NoNug$par[2]), lwd = 4,
  col = '#377eb8', lty  = 2)

WLSfuncGauNug = function(theta,empsvgm,dist,npair) {
  sum((empsvgm-(GauMod(dist,theta[1],theta[2])+theta[3]))^2*npair)
}
theta = c(2.5, 40000, 1.5)
GaufitNug = optim(theta, WLSfuncGauNug, empsvgm = empsvgm.out1$svgm,
  dist = empsvgm.out1$mndist, npair = empsvgm.out1$npair)
lines(d4fit,GauMod(d4fit,GaufitNug$par[1],GaufitNug$par[2]) + GaufitNug$par[3], lwd = 4,
  col = '#377eb8', lty  = 1)

GauNoNugCovMat = GaufitNoNug$par[1] - GauMod(ldisMat,GaufitNoNug$par[1],GaufitNoNug$par[2])
min(eigen(GauNoNugCovMat)$values)
sum(eigen(GauNoNugCovMat)$values < 0)/n

SigMat = GauNoNugCovMat
SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutGauNoNugWLS = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)
min(CVoutGauNoNugWLS[,2])
sum(CVoutGauNoNugWLS[,2] < 0)
sum(CVoutGauNoNugWLS[,2] < 0)/n

GauNugCovMat = GaufitNug$par[1] - GauMod(ldisMat,GaufitNug$par[1],GaufitNug$par[2])
  + diag(rep(GaufitNug$par[3], time = n))
eigen(GauNugCovMat)$values
sum(eigen(GauNugCovMat)$values < 0)/n

SigMat = GauNugCovMat
SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%rd$non_motorised)
n = length(rd$non_motorised)
CVoutGauNugWLS = crossVal(z = matrix(rd$non_motorised, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)
min(CVoutGauNugWLS[,2])
sum(CVoutGauNugWLS[,2] < 0)
sum(CVoutGauNugWLS[,2] < 0)/n

lwd1 = 4
layout(matrix(1:6, nrow = 3, byrow = TRUE))
par(mar = c(5,5,1,1))
hist(CVoutRRnonsph[,2], col = 'grey50', main = '', xlab = "Estimated Variances",
  cex.lab = 2, cex.axis = 1.5, breaks = seq(0,2.2, by = 0.1))
lines(c(0,0),c(-5,150), col = '#e6550d', lwd = lwd1, lty = 2)
hist(CVoutEucNugNon[,2], col = 'grey50', main = '', xlab = "Estimated Variances",
  cex.lab = 2, cex.axis = 1.5, breaks = seq(0,2.2, by = 0.1))
lines(c(0,0),c(-5,150), col = '#e6550d', lwd = lwd1, lty = 2)
hist(CVoutSphNoNugWLS[,2], col = 'grey50', main = '', xlab = "Estimated Variances",
  cex.lab = 2, cex.axis = 1.5)
lines(c(0,0),c(-5,200), col = '#e6550d', lwd = lwd1, lty = 2)
hist(CVoutSphNugWLS[,2], col = 'grey50', main = '', xlab = "Estimated Variances",
  cex.lab = 2, cex.axis = 1.5, breaks = seq(0,4.2,by = .2))
lines(c(0,0),c(-3,100), col = '#e6550d', lwd = lwd1, lty = 2)
hist(CVoutGauNoNugWLS[,2], col = 'grey50', main = '', xlab = "Estimated Variances",
  cex.lab = 2, cex.axis = 1.5, breaks = seq(-39,16,by = 1))
lines(c(0,0),c(-6,150), col = '#e6550d', lwd = lwd1, lty = 2)
hist(CVoutGauNugWLS[,2], col = 'grey50', main = '', xlab = "Estimated Variances",
  cex.lab = 2, cex.axis = 1.5, breaks = seq(-16,16,by = 2))
lines(c(0,0),c(-3,120), col = '#e6550d', lwd = lwd1, lty = 2)

# -----------------------------------------------------------------------------
#       Variance Component Model
# -----------------------------------------------------------------------------

knDist
rrDist

sphCorr = function(dist,rang){
  1 - ((3*dist/(2*rang) - dist^3/(2*rang^3))*(dist <= rang) +
  (dist > rang))
}

nugg = .5
psrr = 1
pseu = 1
rarr = 25000
raeu = 9000
theta = log(c(psrr,rarr,pseu,raeu,nugg))
#undebug(m2LLvcSph)
m2LLvcSph(theta, zs, rrDist, knDist, EdisMat)
vcSphNonFit = optim(theta, m2LLvcSph, z = rd$non_motorised,  
	rrDist = rrDist, knDist = knDist, EdisMat = EdisMat)
exp(vcSphNonFit$par)
nugg = exp(vcSphNonFit$par[5])
psrr = exp(vcSphNonFit$par[1])
pseu = exp(vcSphNonFit$par[3])
rarr = exp(vcSphNonFit$par[2])
raeu = exp(vcSphNonFit$par[4])

SigMat = diag(rep(nugg, times = n)) + 
  psrr*sphCorr(rrDist,rarr)%*%solve(sphCorr(knDist,raeu))%*%t(sphCorr(rrDist,rarr)) + 
  pseu*sphCorr(EdisMat,raeu)

SigMatInv = solve(SigMat)
covb = sum(SigMatInv)
covbi = 1/covb
bhat = covb*sum(SigMatInv%*%zs)
n = length(zs)
CVoutvcSph = crossVal(z = matrix(zs, nrow = n, ncol = 1), 
	X = matrix(1, nrow = n, ncol = 1), 
	V = SigMat, 
	Vi = SigMatInv, 
	n = n, 
	p = 1, 
	covb = covb, 
	covbi = covbi, 
	bhat = bhat)

# AIC, corr, RMSPE, and CI90 for LinEN model for non-motorised data
AICsph = vcSphNonFit$value + 10
corLKnonexp= cor(rd$non_motorised, CVoutvcSph[,1])
RMSPELKnonexp = sqrt(mean((rd$non_motorised - CVoutvcSph[,1])^2))
CI90LKnonexp = mean(CVoutvcSph[,1] - qnorm(.95)*sqrt(CVoutvcSph[,2]) < rd$non_motorised &  
	rd$non_motorised < CVoutvcSph[,1] + qnorm(.95)*sqrt(CVoutvcSph[,2]))


