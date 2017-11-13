layout(matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))

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
legend(5,1, legend = c(expression(alpha == 0.5),expression(alpha == 2),expression(alpha == 8)),
	lty = c(1,2,3), lwd = c(3,3,5), cex = 2)
mtext('(c)', adj = -.15, padj = -.5, cex = 3)

par(mar = c(6,6,5,1))
plot((1:1000)/100,acor.sph((1:1000)/100,.5), type = 'l', lwd = 3,
	xlab = 'Distance', ylab = 'Autocorrelation', ylim = c(0,1),
	cex.lab = 2, cex.axis = 1.5)
lines((1:1000)/100,acor.sph((1:1000)/100,2), lty = 2, lwd = 3)
lines((1:1000)/100,acor.sph((1:1000)/100,8), lty = 3, lwd = 5)
mtext('(d)', adj = -.15, padj = -.5, cex = 3)

par(mfrow=c(1,1))
