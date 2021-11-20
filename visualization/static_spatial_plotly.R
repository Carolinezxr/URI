library(sf)           # Objects and functions for geospatial data
library(rgdal)        # Functions for spatial data input/output
library(ggplot2)      # Graphing functions
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(readr)        # Functions for reading data
library('plotly')
library("gapminder")
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(maptools) 
library(reshape2)
library(sp)

library(geojson)

# Static
beat_map<-st_read("E:\\data\\amityrosado\\Policing-GIS\\Merged without O'Hare.shp") 
beat_map<-beat_map[,c(3,9)]
data<-read.csv("E:\\data\\raw_crimes_beat_transfer.csv")

data1<-subset(data, Date=="2017-08-12")
data1<-as.data.frame(t(data1[,3:307]))
data1$beat_num<-substring(unlist(rownames(data1)),2)
colnames(data1)[1]<-"num_reports"

data1 <- mutate(data1, beat_num = as.character(beat_num))
beat_map <- left_join(beat_map, data1, by = c("beat_num" = "beat_num"))
beat_map[is.na(beat_map)]<-0

p<-ggplot(beat_map) +  geom_sf(aes(fill = num_reports))+  
  scale_fill_gradient(low="white", high="red")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("2017-08-12")

fig<-ggplotly(p)

fig

