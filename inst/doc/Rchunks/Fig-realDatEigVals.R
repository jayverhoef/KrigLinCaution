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
  alpha = 40*i^2/100^2
  # exponential model
  mexp = min(eigen(exp(-ldisMat/(1000*alpha)))$values)
  # Gaussian model
  mgau = min(eigen(exp(-(ldisMat/(1000*alpha))^2))$values)
  # Spherical
  msph = min(eigen((1 - 1.5*ldisMat/(1000*alpha) + .5*ldisMat^3/(1000*alpha)^3)*(ldisMat/1000 < alpha))$values)
  # Cauchy
  mrqu = min(eigen(1/(1 + (ldisMat/(1000*alpha))^2))$values)
	# Hole effect
	mhol = sin(ldisMat/(1000*alpha))/(ldisMat/(1000*alpha))
	diag(mhol) = rep(1, times = length(mhol[,1]))
	mhol = min(eigen(mhol)$values)
	exReal = rbind(exReal, data.frame(alpha = alpha,
		mexp = mexp, mgau = mgau, msph = msph, mrqu = mrqu, mhol = mhol))
}
# try various models on Euclidean distances
EucReal = NULL
for(i in 1:100) {
  alpha = 40*i^2/100^2
  # exponential model
  mexp = min(eigen(exp(-EdisMat/(1000*alpha)))$values)
  # Gaussian model
  mgau = min(eigen(exp(-(EdisMat/(1000*alpha))^2))$values)
  # Spherical
  msph = min(eigen((1 - 1.5*EdisMat/(1000*alpha) + .5*EdisMat^3/(1000*alpha)^3)*(EdisMat/1000 < alpha))$values)
  # Cauchy
  mrqu = min(eigen(1/(1 + (EdisMat/(1000*alpha))^2))$values)
	# Hole effect
	mhol = sin(EdisMat/(1000*alpha))/(EdisMat/(1000*alpha))
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
legend(20,1, legend = c('Exponential','Gaussian','Spherical','Cauchy','Hole Effect'),
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

