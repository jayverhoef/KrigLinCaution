plot(c(0,0), c(-1,0), type = 'l', xlim= c(-0.9,0.72), ylim = c(-1.01,0.8), 
  bty = 'n', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', lwd = 4)
lines(c(0,-sqrt(0.5)), c(0,sqrt(0.5)), lwd = 4)
lines(c(0,sqrt(0.5)), c(0,sqrt(0.5)), lwd = 4)
points(c(0,0,-sqrt(.5),sqrt(.5)), c(-1,0,sqrt(0.5),sqrt(0.5)), pch = 19, cex = 5)
points(x = .3/sqrt(2), y = .3/sqrt(2), cex = 5, lwd = 3)
text(x = c(0,0,-sqrt(.5),sqrt(.5)), y = c(-1,0,sqrt(0.5),sqrt(0.5)),
  labels = c(1,2,3,4), cex = 5, pos = 2, offset = 1.5)

