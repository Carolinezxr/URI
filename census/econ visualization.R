library(readr)
library(sf)           # Objects and functions for geospatial data
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(ggplot2)      # Graphing functions

setwd("E:/data/census")

econ<-read.csv("DP03_zone_economics.csv")


income<-as.data.frame(cbind(econ$Zone,econ$Per.capita.income.dollars))
colnames(income)<-c("Zone","Income")

income$household<-econ$Mean.household.income.dollars

income$population<-econ$Population.16.years.and.over
income$labor<-econ$In.labor.force
income$unlabor<-income$population-income$labor
income$unemployment<-income$unlabor/income$population

beat_map<-st_read("E:\\data\\amityrosado\\Policing-GIS\\Merged without O'Hare.shp")
beat_map<-beat_map[,c(6,9)]

beat_map <- left_join(beat_map, income, by = c("Zone" = "Zone"))



# text
# beat_points <- sf::st_point_on_surface(beat_map)
# beat_coords <- as.data.frame(sf::st_coordinates(beat_points))
# beat_coords$Zone<-beat_map$Zone
# 
# beat_coords2<-beat_coords %>%
#   group_by(Zone) %>%
#   filter(row_number()==ceiling(n()/2))


ggplot(beat_map) +  
  geom_sf(aes(fill = unemployment))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("unempolyment rate")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")
