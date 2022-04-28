
library(sf)
library(plotKML)
library(sp)
library(ggplot2)
library(dplyr)
source("./scripts/Functions.R")

# read data on applications (manually compiled from e-Permit by T Moloney 1/19/2022)
read<-read.csv("./data/applications.csv") 
read<-read[c(1,2),]

# use custom function mypts() to plot the location of applications
x<-read$long
y<-read$lat
z<-read$approp_gpm 
Locids<-read$name 
coords<-CRS("+init=EPSG:4326") # WGS84
pts<-mypts(x,y,z,coords,Locids)

pts<-st_as_sf(pts)
pts.transform <- st_transform(pts st_crs("+init=EPSG:3755"))  # change projection to NAD83 / Wyoming State Plane East
buff<-st_buffer(pts.transform,1320)
buff<-st_buffer(pts.transform,2640)
buff<-st_buffer(pts.transform,3960)
buff<-st_buffer(pts.transform,5280)

plot(buff$geometry)
