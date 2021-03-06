# remove duplicated distances from symmetric distance matrix
ldists = ldisMat[lower.tri(ldisMat)]/1000
Edists = EdisMat[lower.tri(EdisMat)]/1000
par(mar = c(5,5,1,1))
plot(Edists,ldists, pch = 19, cex = .8, xlim = c(0,max(ldists,Edists)),
  ylim = c(0,max(ldists,Edists)), xlab = 'Euclidean Distance (kilometers)',
  ylab = 'Linear Network Distance (kilometers)', cex.lab = 2, cex.axis = 1.5,
  col = rgb(0,0,0,.05))
lines(c(0,max(ldists,Edists)),c(0,max(ldists,Edists)), lty = 2,
  lwd = 3, col = 'blue')
