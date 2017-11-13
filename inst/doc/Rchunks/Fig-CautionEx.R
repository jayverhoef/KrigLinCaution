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
legend(1.35,1, legend = c('Exponential','Gaussian','Spherical','Cauchy',
  'Hole Effect'), lty = c(1,1,1,1,1), lwd = c(3,3,3,3,3), cex = 2.5,
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
xy1 = cbind(kronecker(xg, rep(1, times = n^2)),
  kronecker(yg, rep(1, times = n^2)))
xy2 = cbind(kronecker(rep(1, times = n^2), xg),
  kronecker(rep(1, times = n^2), yg))

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
