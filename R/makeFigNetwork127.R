#' makes stream network figure, part (c) in Figure 2
#'
#' makes stream network figure, part (c) in Figure 2
#'
#' @return a plot
#'
#' @author Jay Ver Hoef
 
 makeFigNetwork127 = function()
{

RU = function(xy,radmult) {
	x = xy$x + sin(2*pi*radmult)
  y = xy$y + cos(2*pi*radmult)
	data.frame(x=x,y=y)
}
RD = function(xy,radmult) {
	x = xy$x + sin(2*pi*radmult)
  y = xy$y - cos(2*pi*radmult)
	data.frame(x=x,y=y)
}
LU = function(xy,radmult) {
	x = xy$x - sin(2*pi*radmult)
  y = xy$y + cos(2*pi*radmult)
	data.frame(x=x,y=y)
}
LD = function(xy,radmult) {
	x = xy$x - sin(2*pi*radmult)
  y = xy$y - cos(2*pi*radmult)
	data.frame(x=x,y=y)
}


cex = .8
plot(c(-5.6,5.6), c(-1.5,5.6), type = "n", bty = 'n', xlab = '', ylab ='',
	xaxt = 'n', yaxt = 'n')
lines(c(0,0),c(0,1), lwd = 7, col = 'skyblue2')
xy = data.frame(x = 0, y = 1)
points(xy, pch = 19, cex = cex)
plist = xy

#debug(grow)
glist = c('RU','LU') 
radmult = rep(1/4, times = 4)
lwd = 6
pch = 19
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c('RU','RD','LU','LD') 
radmult = rep(1/8, times = 8)
lwd = 5
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c('LU','RU','RU','RD','RU','LU','LU','LD') 
radmult = rep(1/8, times = 8)
lwd = 4
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c('LU','RU','LU','RU', 'RU','RD','RU','RD', 
	'RU','LU','RU','LU', 'LU','LD','LU','LD') 
radmult = c(rep(1/16, times = 4), rep(3/16, times = 4), rep(1/16, times = 4), rep(3/16, times = 4))
lwd = 3
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c(rep(c('LU','RU'), times = 4), rep(c('RU','RD'), times = 4), 
	rep(c('RU','LU'), times = 4), rep(c('LU','LD'), times = 4)) 
radmult = c(rep(1/32, times = 8), rep(7/32, times = 8), rep(1/32, times = 8), rep(7/32, times = 8))
lwd = 2
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c(rep(c('LU','RU'), times = 8), rep(c('RU','RD'), times = 8), 
	rep(c('RU','LU'), times = 8), rep(c('LU','LD'), times = 8)) 
radmult = c(rep(1/64, times = 16), rep(15/64, times = 16), rep(1/64, times = 16), rep(15/64, times = 16))
lwd = 1
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

xy = data.frame(x = 0, y = 1)
points(xy, pch = 19, cex = cex)
plist = xy

#debug(grow)
glist = c('RU','LU') 
radmult = rep(1/4, times = 4)
lwd = 6
pch = 19
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
#				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
#				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
#				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
#        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c('RU','RD','LU','LD') 
radmult = rep(1/8, times = 8)
lwd = 5
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
#				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
#				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
#				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
#        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c('LU','RU','RU','RD','RU','LU','LU','LD') 
radmult = rep(1/8, times = 8)
lwd = 4
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
#				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
#				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
#				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
#        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c('LU','RU','LU','RU', 'RU','RD','RU','RD', 
	'RU','LU','RU','LU', 'LU','LD','LU','LD') 
radmult = c(rep(1/16, times = 4), rep(3/16, times = 4), rep(1/16, times = 4), rep(3/16, times = 4))
lwd = 3
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
#				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
#				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
#				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
#        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c(rep(c('LU','RU'), times = 4), rep(c('RU','RD'), times = 4), 
	rep(c('RU','LU'), times = 4), rep(c('LU','LD'), times = 4)) 
radmult = c(rep(1/32, times = 8), rep(7/32, times = 8), rep(1/32, times = 8), rep(7/32, times = 8))
lwd = 2
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
#				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
#				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
#				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
#        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
plist = plistnew

glist = c(rep(c('LU','RU'), times = 8), rep(c('RU','RD'), times = 8), 
	rep(c('RU','LU'), times = 8), rep(c('LU','LD'), times = 8)) 
radmult = c(rep(1/64, times = 16), rep(15/64, times = 16), rep(1/64, times = 16), rep(15/64, times = 16))
lwd = 1
  plistnew = NULL
  for(i in 1:length(plist[,1])) {
		for(j in 1:2) {
	    if(glist[2*(i - 1) + j] == 'RU') {
#				lines(rbind(plist[i,],RU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	    if(glist[2*(i - 1) + j] == 'RD') {
#				lines(rbind(plist[i,],RD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(RD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, RD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LU') {
#				lines(rbind(plist[i,],LU(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LU(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LU(plist[i,],radmult[2*(i - 1) + j]))
  	  }
      if(glist[2*(i - 1) + j] == 'LD') {
#        lines(rbind(plist[i,],LD(plist[i,],radmult[2*(i - 1) + j])), lwd = lwd, col = 'skyblue2')
        points(LD(plist[i,],radmult[2*(i - 1) + j]), pch = pch, cex = cex)
        plistnew = rbind(plistnew, LD(plist[i,],radmult[2*(i - 1) + j]))
  	  }
	  }
  }
mtext('(c)', adj = -.15, padj = -.5, cex = 3)
}


