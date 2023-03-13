##Map of Collection Sites from Summer 2019
##code sourced and adapted by Jess Glanz 202101

#clear dataframe#####
rm(list=ls())

##Install packages
# load packages
#easy and fun
library(tidyverse)
library(lubridate)
library(data.table)
library(ggrepel)
library(ggpubr)
library(ggsn)
library(magick)
library(maps)
library(maptools)
library(ggplot2)
library(grid)
library(ggimage)
library(ggmap)
library(here)


#set wd
here()

#Load lat-long data for sites
lola<-Summer19CollectionSites

#Map of Moorea with sites
#make map and set sources
register_google(key="")#enter API key from google cloud account
myMap <- get_googlemap(center = c(-149.84, -17.54), zoom = 12, maptype = 'satellite' ) 

#Get the bounding box for the map and reformat it into a data.frame for scalebar
bb <- attr(myMap, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))


#Add the scalebar to a ggmap, need ggsn package
map <- ggmap(myMap) + 
  labs(x = 'Longitude', y = 'Latitude') +
  scalebar(data = bb2, dist = 2, st.color = "white",transform = TRUE, model  = "GRS67",  dist_unit = "km", #data are bounding box of map, model gives dataum from google maps, transform recognizes as GPS points as decimal units, location sets location on map, anchor sets bounds of locatio non map
           location = "topright", st.dist = 0.036, box.fill = "white", #sets scalebar bottom left, st.dist is distance bewteen box and numbers,box.fill fills box
           anchor = c( x = bb$ll.lon + 0.05, y = bb$ll.lat +0.03)) + #sets scalebar in specific position long x lat
  geom_point(aes(x = Long, y = Lat, color=Flow),
             alpha = .5, size = 3, shape = 20, data=lola)+  #geom_point adds specific plot points to map 
scale_color_manual(values = c("High" = "red", "Low" = "orange")) +
  theme(legend.position="none")

#Order is important for saving this with the "north" symbol
pdf(file="MooreaMap.pdf",width=10,height=8)
#add north symbol to map
north2(map,  x=.26, y=.28, symbol=4)
dev.off()


#Map of Northeast shore with sites
#make map and set sources
myMap <- get_googlemap(center = c(-149.800, -17.4803), maptype = "satellite", zoom = 14) 

#Get the bounding box for the map and reformat it into a data.frame for scalebar
bb <- attr(myMap, "bb")
bb2 <- data.frame(long = unlist(bb[c(2, 4)]), lat = unlist(bb[c(1,3)]))
View(bb2)

#Add the scalebar to a ggmap, need ggsn package
map <- ggmap(myMap) + 
  labs(x = 'Longitude', y = 'Latitude') +
  scalebar(data = bb2, dist = 0.5, st.color = "white",transform = TRUE, model  = "GRS67",  dist_unit = "km", #data are bounding box of map, model gives dataum from google maps, transform recognizes as GPS points as decimal units, location sets location on map, anchor sets bounds of location on map
           location = "topright", st.dist = 0.036, box.fill = "white", #sets scalebar bottom left, st.dist is distance bewteen box and numbers,box.fill fills box
           anchor = c( x = bb$ll.lon +0.05 , y = bb$ll.lat+0.04)) + #sets scalebar in specific position long x lat
  geom_point(aes(x = Long, y = Lat, color=Flow),
             alpha = .5, size = 9, shape = 20, data=lola)+  #geom_point adds specific plot points to map 
  scale_color_manual(values = c("High" = "red", "Low" = "orange")) +
  theme(legend.position="none")


#Order is important for saving this with the "north" symbol
pdf(file="SiteMap.pdf",width=10,height=8)
#add north symbol to map
north2(map,  x=.77, y=.85, symbol=4)
dev.off()



