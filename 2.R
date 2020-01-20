# Quantitative Geography - 2 # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #
# source: Brunsdon and Comber(2015), Chapter 3 #

library(GISTools)
data(newhaven)
ls()
plot(roads)
plot(blocks)
block.attr<-data.frame(blocks)
head(block.attr)

########## 1. Plotting Spatial Objects ###########
par(mar = c(0,0,0,0)) 
plot(blocks)
plot(roads, add=TRUE, col="red")

plot(blocks)
plot(breach, col = "red", add = TRUE)

plot(blocks, lwd = 0.5, border = "grey50") 
plot(breach, col = "red", pch = 1, add = TRUE)

colors()

### Map Layout #### 
library(GISTools)
data(newhaven)
# plot spatial data
par(mar = c(0,0,2,0))
plot(blocks)
plot(breach,add=TRUE,col= 'red', pch = 1)
# embellish the map
locator()
map.scale(534750,152000,miles2ft(2), "Miles",4,0.5) 
north.arrow(533043.9,154617.4,miles2ft(0.2),col= 'lightblue') 
title('New Haven, CT')

locator()

### Map Layout (In-class Task #1) ### 
library(rgdal)
setwd("D:/R_Labs/_QG2017")
EPA.STN <- readOGR(dsn = "Lyr", layer = "EPA_STN1")
Popn.TWN <- readOGR(dsn = "Lyr", layer = "Popn_TWN", encoding="Big5")
par(mar = c(0,2,2,2))
plot(Popn.TWN, col='lightgreen', border="grey", bg='aliceblue')
Popn.TWN.attr<-data.frame(Popn.TWN)
plot(EPA.STN,add=TRUE,col= 'blue', pch = 16)
map.scale(63030.22,2472112,50000, "50 km",1,1) 
north.arrow(49646.41,2534913,10000,col= 'lightblue') 
title('Taiwan EPA Stations')


### Adding Contexts (OpenStreetMaps as Background) ###

### Map Projection Transformation ###
library(rgdal);library(sp)
proj4string(Popn.TWN)
TWN.LongLat <- spTransform(Popn.TWN, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

### OpenStreepMap ###
library(OpenStreetMap)
# define upper left, lower right corners
ul <- as.vector(cbind(bbox(TWN.LongLat)[2,2], bbox(TWN.LongLat)[1,1]))
lr <- as.vector(cbind(bbox(TWN.LongLat)[2,1], bbox(TWN.LongLat)[1,2]))
# download the map tile
MyMap <- openmap(ul,lr,9, "esri-topo")
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=FALSE) 
plot(spTransform(EPA.STN, osm()), pch = 16, add = TRUE, col="red", cex=1.2)


########## 2. Mapping Spatial Data Attributes ###########

library(GISTools)
data(newhaven)
data.frame(blocks)
head(data.frame(blocks))
colnames(data.frame(blocks))
data.frame(blocks)$P_VACANT
blocks$P_VACANT
attach(data.frame(blocks))
par(mar = c(3,5,3,3))
hist(P_VACANT)

### Choropleth Mapping ###
par(mar = c(3,5,3,3))
choropleth(blocks, blocks$P_VACANT)
vacant.shades = auto.shading(blocks$P_VACANT)
vacant.shades
choro.legend(533000,161000,vacant.shades)

### Set Shading Argument (no. of intervals) ###
par(mar = c(0,5,0,3))
vacant.shades = auto.shading(blocks$P_VACANT,n=7)
# plot the map 
choropleth(blocks,blocks$P_VACANT,shading=vacant.shades) 
choro.legend(533000,161000,vacant.shades)

### Set Shading Argument (colors) ###
brewer.pal(5,'Blues')
vacant.shades = auto.shading(blocks$P_VACANT, cols=brewer.pal(7,"Greens"), n=7)
choropleth(blocks, blocks$P_VACANT,shading=vacant.shades)
choro.legend(533000,161000,vacant.shades)

### Set Shading Argument (cutters) ###
vacant.shades = auto.shading(blocks$P_VACANT, n=5, cols=brewer.pal(5,"Blues"), cutter=rangeCuts)
choropleth(blocks,blocks$P_VACANT,shading=vacant.shades)
choro.legend(533000,161000,vacant.shades)


########## 3. Basic statistical analysis  ###########

### Histogram ###
data(newhaven)
par(mar = c(5,5,3,3))
hist(blocks$P_VACANT, breaks = 20, col = "blue",
     border = "grey",
     main = "The distribution of vacant property percentages",
     xlab = "percentage vacant", xlim = c(0,40))

### Boxplot ###

index <- blocks$P_VACANT > 10 
high.vac <- blocks[index,] 
low.vac <- blocks[!index,]

# set plot parameters and shades
cols = rev(brewer.pal(3, "Blues")) 
par(mfrow = c(1,2))
par(mar = c(2.5,2,3,1))
attach(data.frame(high.vac))

boxplot(P_OWNEROCC,P_WHITE,P_BLACK, 
        names=c("OwnerOcc", "White", "Black"),
        col=cols, cex.axis = 0.7, main = "High Vacancy")
detach(data.frame(high.vac))

# do the same for the second boxplot & variables 
attach(data.frame(low.vac))
boxplot(P_OWNEROCC,P_WHITE,P_BLACK, 
        names=c("OwnerOcc","White", "Black"), 
        col=cols, cex.axis = 0.7, main = "Low Vacancy")
detach(data.frame(low.vac)) 

# reset par(mfrow) and plot margins 
par(mfrow=c(1,1))
par(mar=c(5,4,4,2))


### Scatter Plot and Regression Lines ###

plot(blocks$P_VACANT/100, blocks$P_WHITE/100) 
plot(blocks$P_VACANT/100, blocks$P_BLACK/100)

# transfer to %
p.vac <- blocks$P_VACANT/100 
p.w <- blocks$P_WHITE/100 
p.b <- blocks$P_BLACK/100

plot(p.vac, p.w, xlab= "Proportion Vacant", ylab = "Proprtion White or Black",
     col = "blue", xlim = c(0, 0.8))
points(p.vac,p.b, col = "black", pch=16) 


# fit regressions
mod.1 <- lm(p.vac ~ p.w) 
mod.2 <- lm(p.vac ~ p.b)
summary(mod.1)

# fit some trend lines from the 2 regression model coefficients
abline(a = coef(mod.1)[1], b= coef(mod.1)[2],
       lty = 1, col = "blue"); #white
abline(a = coef(mod.2)[1], b= coef(mod.2)[2],
       lty = 1, col = "black"); #black
# add some legend items
legend(0.71, 0.20, legend = "Black", bty = "n", cex = 0.8) 
legend(0.71, 0.095, legend = "White", bty = "n", cex = 0.8)


