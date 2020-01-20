library(rgdal)
library(GISTools)
library(dplyr)

Elementary_School <- readOGR(dsn = "QG_Data4", layer = "Elementary_School")
Tpe_Fastfood <- readOGR(dsn = "QG_Data4", layer = "Tpe_Fastfood")
Tpe_Town <- readOGR(dsn = "QG_Data4", layer = "Tpe_Town", encoding="Big5")

