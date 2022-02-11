library(readr)
library(sf)           # Objects and functions for geospatial data
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(ggplot2)      # Graphing functions

setwd("E:/data/census")

social<-read.csv("DP02_zone_social.csv")

visual<-as.data.frame(cbind(social$Zone,social$Population.25.years.and.over))
colnames(visual)<-c("Zone","education")

visual$bachelor<-social$Bachelor.s.degree.or.higher
visual$primary<-social$Less.than.9th.grade

visual$highEdu<-visual$bachelor/visual$education
visual$lowEdu<-visual$primary/visual$education

visual$household<-social$Total.households
visual$no_family<-social$Nonfamily.households
visual$no_family_Pct<-visual$no_family/visual$household



beat_map<-st_read("E:\\data\\amityrosado\\Policing-GIS\\Merged without O'Hare.shp")
beat_map<-beat_map[,c(6,9)]

beat_map <- left_join(beat_map, visual, by = c("Zone" = "Zone"))



# text
# beat_points <- sf::st_point_on_surface(beat_map)
# beat_coords <- as.data.frame(sf::st_coordinates(beat_points))
# beat_coords$Zone<-beat_map$Zone
# 
# beat_coords2<-beat_coords %>%
#   group_by(Zone) %>%
#   filter(row_number()==ceiling(n()/2))


ggplot(beat_map) +  
  geom_sf(aes(fill = no_family_Pct))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Non Family Household Pct)")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")
