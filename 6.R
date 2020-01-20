# Quantitative Geography - 6 # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #

# Using R package:aspace

# Measures of Centrality
#1.(Weighted) Mean center 
#2.Median center
#3.Central feature

# Measures of Dispersion
#1.Standard Distance
#2.Weighted Std. Distance
#3.Standard Deviational Ellipse

rm(list = ls())
library(rgdal)
setwd("D:/R_Labs/_QG2017")
School <- readOGR(dsn = "SHP", layer = "Schools", encoding="utf8")
School.attr <-data.frame(School)
head(School.attr)

# Generating no. of student in each school
School.attr$Students<-as.integer (runif(424,100,1000))

# Generating school type: cluster vs. isolation
for (i in 1: 424) {
  
  if (School.attr$NEAR_DIST[i]< 500) {
    School.attr$type[i]<- "Cluster"
  } else School.attr$type[i]<- "Isolation"
  
}

index<- School.attr$type== "Cluster"
school_cluster<-School[index,]

length(school_cluster)
plot(school_cluster)

School.coord<-data.frame(x=School.attr$X_coor, y=School.attr$Y_coor, type=School.attr$type, 
                         students=School.attr$Students)


library(aspace)

### Mean Center
Mean.Center<-mean_centre(id=1, weighted=FALSE, weights=NULL, points=School.coord[,1:2])
W.Mean.Center<-mean_centre(id=1, weighted=TRUE, weights=School.attr$Students, points=School.coord[,1:2])

plot(School.coord[,1:2],asp=1, axes=FALSE, xlab="", ylab="",  pch=16, cex=0.6)
points(Mean.Center$CENTRE.x, Mean.Center$CENTRE.y, col="red", cex=1, pch=16)
points(W.Mean.Center$CENTRE.x, W.Mean.Center$CENTRE.y, col="blue", cex=1, pch=16)

### Median Center
Median.Center<-median_centre(id=1,points=School.coord[,1:2])

### Compare Mean vs. Median
plot(School.coord[,1:2],asp=1, axes=FALSE, xlab="", ylab="",  pch=16, cex=0.6)
points(Mean.Center$CENTRE.x, Mean.Center$CENTRE.y, col="red", cex=1, pch=16)
points(Median.Center$median.x, Median.Center$median.y, col="blue", cex=1, pch=16)

### SDD: Standard Distance

school.SDD<- calc_sdd(id=1, points=School.coord[,1:2])
school.SDD2<- calc_sdd(id=1, points=School.coord[,1:2], weighted = TRUE, 
                       weights =School.attr$Students)¡@#weighted
plot(School.coord[,1:2],asp=1, axes=FALSE, xlab="", ylab="", type="n")
plot_sdd(plotnew=FALSE, plotcentre=FALSE, centre.col="red", centre.pch="MC", 
         sdd.col="red",sdd.lwd=1,titletxt="", plotpoints=TRUE,points.col="black")
points(school.SDD$CENTRE.x, school.SDD$CENTRE.y, col="red", cex=1, pch=16)

## SDD to shapefile example
shp <- convert.to.shapefile(sddloc,sddatt,"id",5) # ESRI Shape type 1=point, 3=polyLine, 5=polygon
write.shapefile(shp, "SDD_Shape", arcgis=T) # Replace "." with "\_" in column names for ArcGIS

?convert.to.shapefile()
?plot_sdd()

### SDE:Standard Deviational Ellipse

plot(School.coord[,1:2], asp=1, axes=FALSE, xlab="", ylab="", type="n")
school.SDE<- calc_sde(id=1, points=School.coord[,1:2])

plot_sde(plotnew=FALSE, plotcentre=FALSE, centre.col="red", centre.pch="1", 
         sde.col="red",sde.lwd=1,titletxt="", plotpoints=TRUE,points.col="black")
points(school.SDD$CENTRE.x, school.SDD$CENTRE.y, col="red", cex=1, pch=16)

## Central Feature

school.CF<- CF(id=1, points=School.coord[,1:2])
plot(School.coord[,1:2], asp=1, axes=FALSE, xlab="", ylab="")
points(school.CF$CF.x,school.CF$CF.y, col="red", cex=1, pch=16)

### Group by School Types

type<-School.attr$type
Students<-School.attr$Students

new<-aggregate(Students~type, School.attr, sum)
newid<-new[,1]

xx<-vector(); yy<-vector(); ctype<-vector()
for (i in 1:2){
  index<-(type == newid[i])
  newschool<-School.coord[index,]
  newschool.SDD<- calc_sdd(id=1, points=newschool[,1:2])
  xx[i]<-newschool.SDD$CENTRE.x
  yy[i]<-newschool.SDD$CENTRE.y
  ctype[i]<-newid[i]
}

newcenterxy<-cbind(xx,yy, ctype)

plot(School.coord[,1:2],asp=1, axes=FALSE, xlab="", ylab="",  pch=16, cex=0.6)
points(newcenterxy[1,1],newcenterxy[1,2], col="red", cex=1, pch=16)
points(newcenterxy[2,1],newcenterxy[2,2], col="blue", cex=1, pch=16)