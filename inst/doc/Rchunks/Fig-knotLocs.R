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
