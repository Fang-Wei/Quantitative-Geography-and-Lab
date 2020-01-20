# Quantitative Geography - 5 # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #
# reference: Brunsdon and Comber(2015), Chapter 5 #

library(GISTools)
data(newhaven)

#1. Fishnet: GridTopology
bb <- bbox(tracts)
grd <- GridTopology(cellcentre.offset=c(bb[1,1]-200,bb[2,1]-200),
                    cellsize=c(10000,10000), cells.dim = c(5,5)) 

GRD_Layer <- SpatialPolygonsDataFrame(as.SpatialPolygons.GridTopology(grd),
             data = data.frame(c(1:25)), match.ID = FALSE) 

names(GRD_Layer) <- "ID"
head(GRD_Layer@data)

plot(GRD_Layer)
plot (tracts, add=TRUE, border = "red", lwd =2)
head(tracts@data)

tractsdf<-data.frame(tracts)

#2. Spatial intersection

Overlay_Layer <- gIntersection(GRD_Layer, tracts, byid = T)

plot(Overlay_Layer, lwd =2)
names(Overlay_Layer)

tmp <- strsplit(names(Overlay_Layer), " ") 
tmp2<-unlist(tmp)

tracts.id<-vector(); gridlayer.id<-vector()

x=1
for (i in seq(1,142,2)) {
  gridlayer.id[x]<-tmp2[i]
  x=x+1  
} 

y=1
for (i in seq(2,142,2)) {
  tracts.id[y]<-tmp2[i]
  y=y+1  
} 

df=data.frame(tracts.id, gridlayer.id )

#3. Field calculation

for (i in 1:71) {
  df$area1[i] <- tracts$AREA[as.numeric(tracts.id[i])+1] 
  df$house1[i] <- tracts$HSE_UNITS[as.numeric(tracts.id[i])+1] 
}
Overlay_LayerNew@data
Overlay_LayerNew<- SpatialPolygonsDataFrame(Overlay_Layer, data=df,match.ID = F)

Overlay_LayerNew$area2<- poly.areas(Overlay_LayerNew)
area1 <- Overlay_LayerNew$area1
area2 <- Overlay_LayerNew$area2
house1<- Overlay_LayerNew$house1
Overlay_LayerNew$house_pct<- as.integer ((area2/area1) * house1)

house_pct<-Overlay_LayerNew$house_pct
gridlayer.id<-Overlay_LayerNew$gridlayer.id

grid.house<- xtabs(house_pct ~ gridlayer.id )
df1<-data.frame(grid.house)
df1[,1]<-substring(df1[,1], 2,3)
df1[,1]<-as.numeric(df1[,1])
names(df1)<- c("ID","Houses")

#4. Data join

library(dplyr)
GRD_Layer@data<-left_join(GRD_Layer@data, df1)

n=nrow(GRD_Layer@data)
for (i in 1:n){
  if (is.na(GRD_Layer$Houses[i])) {GRD_Layer$Houses[i]=0}
  }

#5. Mapping

# set the plot parameters and the shading variable
par(mar=c(0,0,0,0))
shades = auto.shading(GRD_Layer$Houses, n = 5, cols = brewer.pal(5, "Greens")) 
# map the data
choropleth(GRD_Layer, GRD_Layer$Houses, shades) 
plot(tracts,  border = "red", add = T)
choro.legend(530000, 159115, bg = "white", shades, title = "No. of houses", under = "") 
# reset the plot margins
par (mar=c(5,4,4,2))



