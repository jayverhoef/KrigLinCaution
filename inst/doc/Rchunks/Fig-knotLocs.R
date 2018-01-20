data(semivarTable)
locs = unique(data.frame(semivar.table$Origin, semivar.table$X_origin,semivar.table$Y_origin))
par(mar = c(5,5,1,1))

cbind(as.character(locs[,1]),matNames)

# create spatial knots using kmeans
set.seed(1001) # kmeans uses random starting points.  This ensures repeatability.
nknots = 120
km = kmeans(locs[,2:3],nknots,40)
# find nearest observed site on network
dall = as.matrix(dist(rbind(locs[,2:3],km$centers), diag = TRUE, upper = TRUE))
mins = apply(dall[1:239,(239+1):(239+nknots)],2,min)
knots = NULL
for(i in 1:nknots)
  knots = rbind(knots, locs[which(dall[1:239,239 + i] == mins[i])[1],])
rownames(knots) = as.character(knots[,1])
xy = locs[,2:3]

#commands used to create network and store in package
#library(rgdal)
# read in shapefiles
#path = '/media/jay/Hitachi2GB/00NMML/activePapers/KrigLinCaution/KrigLinCaution_package/KrigLinCaution/inst/rawdata/ShapeFiles/'
#roads.rg <- readOGR(path, "Clipped_roads")
#trails.rg <- readOGR(path, "Clipped_trails")
#save('roads.rg', 'trails.rg', file  = '/media/jay/Hitachi2GB/00NMML/activePapers/KrigLinCaution/KrigLinCaution_package/KrigLinCaution/data/network.rda')
#xym = matrix(c(min(xy[,1]), min(xy[,2]), 
#  min(xy[,1]), max(xy[,2]), 
#  max(xy[,1]), max(xy[,2]),
#  max(xy[,1]), min(xy[,2])), ncol = 2, byrow = TRUE)
#p = Polygon(xym)
#ps = Polygons(list(p),1)
#sps = SpatialPolygons(list(ps))
#proj4string(sps) = proj4string(roads.rg)
#df = data.frame(number = 1)
# Create a spatial polygon data frame (includes shp attributes)
#spdf = SpatialPolygonsDataFrame(sps, df)
#writeOGR(spdf, layer = 'bbPolygon', path, driver="ESRI Shapefile")
data(network)
par(mar = c(5,5,1,1))
plot(xy, type = 'n', cex = 2, pch = 1, xlab = 'x coordinate', ylab = 'y coordinate',
  cex.lab = 2, cex.axis = 1.5)
box(lwd = 5)
sp:::plot(trails.rg, col = 'grey', lwd = 1.5, add = TRUE)
sp:::plot(roads.rg, lwd = 2.2, add = TRUE)
points(xy, pch = 1, cex = 1.5, lwd = 1.5)
points(km$centers, pch = 19, cex = 1.2, col = 'blue')
points(knots[,2:3], col = 'red', pch = 19, cex = .8)


