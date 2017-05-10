# copy of part of the original code and data found at http://dx.doi.org/10.5061/dryad.62t17
library(plyr)

path1='/mnt/Hitachi2GB/00NMML/activePapers/KrigLinCaution/'
path2='KrigLinCaution_package/KrigLinCaution/inst/rawdata/'
ND_ED_cameras = read.csv(paste0(path1,path2,'ND_ED_cameras.csv'))

save(ND_ED_cameras, file = paste0(path1,'KrigLinCaution_package/KrigLinCaution/data/',
 'ND_ED_cameras.rda'))


BLUPs <- read.csv(paste0(path1,path2,'BLUP_all_final_day.csv'))
save(BLUPs, file = paste0(path1,'KrigLinCaution_package/KrigLinCaution/data/',
 'BLUPS.rda'))

test<-merge(ND_ED_cameras,BLUPs,by.x="Origin",by.y="Location")

test2<-merge(test,BLUPs,by.x="Destination",by.y="Location")

colnames(test2)[9]<-"m_origin"
colnames(test2)[10]<-"nm_origin"
colnames(test2)[11]<-"m_dest"
colnames(test2)[12]<-"nm_dest"

test2$m_diff<-abs(test2$m_origin-test2$m_dest)
test2$nm_diff<-abs(test2$nm_origin-test2$nm_dest)
##Check links from camera to camera
links<-ddply(ND_ED_cameras,"Origin",summarise,
             N=length(Origin)
)

##Create semivariance table

semivar.table<-test2[order(test2$Origin,test2$Destination),]

diff_sqrd<-semivar.table$m_diff^2

semivar.table<-cbind(semivar.table,diff_sqrd)

summary(diff_sqrd)

semivariance<-0.5*semivar.table$diff_sqrd

summary(semivariance)

semivar.table<-cbind(semivar.table,semivariance)

save(semivar.table, file = paste0(path1,'KrigLinCaution_package/KrigLinCaution/data/',
 'semivarTable.rda'))


