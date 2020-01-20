# Quantitative Geography - 11: Hotspot Analysis # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #

rm(list = ls())
library(spdep)
library(rgdal)
fpath <- file.choose()
TWPOP <- readOGR(dsn = fpath, layer = "Popn_TWN", encoding="utf8")

head(TWPOP@data)
TWTable<-TWPOP@data

County<-TWPOP$County
Sel<- County =='新北市' | County == '臺北市' 
TWN_North<- TWPOP[Sel,]
plot(TWN_North)

# 1. Defining Spatial Neighbors
TWN_nb<-poly2nb(TWN_North)

# 2. Building Weighting Matrix
TWN_nb_w<- nb2listw(TWN_nb, zero.policy=T)

# 3. Attribute
Popn<-TWN_North@data$Popn;
Popn<-as.numeric(paste(Popn))

# 4. Local Moran
LISA.Popn <- localmoran(Popn, TWN_nb_w, zero.policy=T)
printCoefmat(data.frame(LISA.Popn))

TWN_North$Popn2 <- Popn/1000
TWN_North$z.li <- LISA.Popn[,4]
TWN_North$pvalue <- LISA.Popn[,5]
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(TWN_North, zcol="Popn2", col.regions=lm.palette(20), main="Popn")
spplot(TWN_North, zcol="z.li", col.regions=lm.palette(20), main="Local Moran")

lm.palette2 <- colorRampPalette(c( "red","orange","white"), space = "rgb")
spplot(TWN_North, zcol="pvalue", col.regions=lm.palette2(20), main="p-value")

# Export to a new SHP file
writeOGR(TWN_North, dsn="shapefiles", layer="Popn_LISA", driver="ESRI Shapefile")

# 5. LISA Map
chk<-Popn-mean(Popn)
zi<- LISA.Popn[,4]
quadrant <- vector(mode="numeric",length=nrow(LISA.Popn))
quadrant[chk>0 & zi>0] <- 1 # H-H
quadrant[chk<0 & zi>0] <- 2 # L-L
quadrant[chk>0 & zi<0] <- 3 # H-L
quadrant[chk<0 & zi<0] <- 4 # L-H

signif <- 0.05
quadrant[LISA.Popn[, 5]> signif] <- 5
colors <- c("red", "blue", "lightpink", "skyblue2", rgb(.95, .95, .95))
plot(TWN_North, border="grey", col=colors[quadrant], main = "LISA Cluster Map: Population")
legend("bottomright",legend=c("High-High","Low-Low","High-Low","Low-High"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)

# 6. Local Gi*

TWN_nb_in<-include.self(TWN_nb); summary(TWN_nb_in)
TWN_nb_in_w<- nb2listw(TWN_nb_in, zero.policy=T)
LG<-localG(Popn, TWN_nb_in_w); LG

LG1<-0
for (i in 1:41){LG1[i]<-LG[i]}
TWN_North$LG<-LG1
lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")
spplot(TWN_North, zcol="LG", col.regions=lm.palette(20), main="Local Gi*")

# Mapping Hotspot Areas 
LGV<-TWN_North$LG
chk<-LGV-1.65

quadrant <- vector(mode="numeric",length=41)
quadrant[chk>0] <- 1 # Cluster
quadrant[chk<0] <- 2 # Non-cluster

colors <- c("red", "lightgray")
plot(TWN_North, border="darkgrey", col=colors[quadrant], main = "Cluster Map: Population")
legend("bottomright",legend=c("Cluster","Non-cluster"), fill=colors,bty="n",cex=0.7,y.intersp=1,x.intersp=1)
