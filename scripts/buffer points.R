
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
pts.transform <- st_transform(pts, st_crs("+init=EPSG:3755"))  # change projection to NAD83 / Wyoming State Plane East
buff_100<-st_buffer(pts.transform,100)
buff_500<-st_buffer(pts.transform,500)
buff_1320<-st_buffer(pts.transform,1320)
buff_2640<-st_buffer(pts.transform,2640)
buff_3960<-st_buffer(pts.transform,3960)
buff_5280<-st_buffer(pts.transform,5280)

buff<-rbind(buff_100,buff_500,buff_1320,buff_2640,buff_3960, buff_5280)

henry_results<-read_csv("./outputs/images/Henry/result_tbl.csv")
oliver_results<-read_csv("./outputs/images/Oliver/result_tbl.csv")
a<-0
for (i in 1:6) {
  a<-c(a,henry_results$mean_drawdown_20_yrs[i],oliver_results$mean_drawdown_20_yrs[i])
}
buff <- buff  %>% mutate(drawdwon=a[2:13])


sf::st_write(buff, "./outputs/all_buffers.shp")


plot(buff$geometry)
