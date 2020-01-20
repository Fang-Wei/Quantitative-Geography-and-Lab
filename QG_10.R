# Quantitative Geography - 10: Spatial Autocorrelation # 
# Instrucutor: Tzai-Hung Wen (NTU Geography) #

rm(list = ls())
library(spdep)
library(rgdal)
TWPOP <- readOGR(dsn = "Popn_TWN", layer = "Popn_TWN", encoding="utf8")

head(TWPOP@data)
TWTable<-TWPOP@data

County<-TWPOP$County
Sel<- County == "·s¥_¥«" | County == "»O¥_¥«" 
TWN_North<- TWPOP[Sel,]
plot(TWN_North)

#####################
## Spatial Neighbors
#####################

### 1. Contiguity: QUEEN vs. ROOK

#?poly2nb: Construct neighbours list 
TWN_nb<-poly2nb(TWN_North) #QUEEN = TRUE
summary(TWN_nb)

TWN_nb2<-poly2nb(TWN_North,queen=FALSE ) # ROOK

# 1-1 Buiding Neighborhood Matrix
TWN_nb_w.mat <-nb2mat(TWN_nb, style="B"); TWN_nb_w.mat
# style = B is the basic binary coding, 
# W is row standardised, C is globally standardised

# 1.2. Finding neighbors of a district
TWN_nb[1]

# 1.3. Plot the Neighborhood Matrix
coords<-coordinates(TWN_North)
plot(TWN_North)
plot(TWN_nb, coords, add=T)


### 2. K-nearest Neighbors (KNN)

IDs <-TWN_North@data$UNI_ID
TWN_kn1<-knn2nb(knearneigh(coords, k=2), row.names=IDs)
plot(TWN_North)
plot(TWN_kn1, coords, add=T)

TWN_kn1[1]


### 3. Distance-based (fixed distance band)

TWN_ran1<-dnearneigh(coords, d1=0, d2=20000, row.names=IDs)
plot(TWN_North)
plot(TWN_ran1, coords, add=T)

#########################
## From Spatial Neighbors to ListW (Weighting matrix)
#########################

TWN_nb_w<- nb2listw(TWN_nb, zero.policy=T) # default: style = "W" 
TWN_nb_w$weight[1]

#########################
## Spatial Autocorrelation 
#########################

# 1. Mapping the attribute
library(GISTools)
shades = auto.shading(as.numeric(TWN_North$Popn), n = 5, cols = brewer.pal(5, "Greens")) 
choropleth(TWN_North, as.numeric(TWN_North$Popn), shades) 

# 2.Moran?€™s I Statistic
Popn<-TWN_North@data$Popn;
Popn<-as.numeric(Popn)
M<-moran.test(Popn, listw=TWN_nb_w, zero.policy=T); M

# 3.Monte-Carlo simulation of Moran?€™s I
bperm<-moran.mc(Popn,listw=TWN_nb_w,nsim=999)
hist(bperm$res, freq=TRUE, breaks=20, xlab="Simulated Moran's I")
abline(v=0.18, col="red")

# 4.Moran Spatial Correlogram
cor<-sp.correlogram(TWN_nb, Popn, order=3, method="I", style="W")
print(cor); plot(cor)

# 5.Moran Scatter Plot
nci <- moran.plot (Popn, TWN_nb_w, labels=IDs , xlab="Popn Density", ylab="SL Popn Density")

# 6.Getis-Ord General G Statistic
TWN_ran1_wb<-nb2listw(TWN_ran1, style="B", zero.policy=T)
G<-globalG.test(Popn, listw=TWN_ran1_wb); G


