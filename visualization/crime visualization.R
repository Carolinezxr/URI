library(sf)           # Objects and functions for geospatial data
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(ggplot2)      # Graphing functions



setwd("E:/data/asset")

# Static
beat_map<-st_read("E:\\data\\amityrosado\\Policing-GIS\\Merged without O'Hare.shp") 
beat_map<-beat_map[,c(6,9)]
data<-read.csv("E:\\data\\crimes_data_zone_simplified_summary_day.csv")

data$day<-as.Date(data$day,"%Y-%m-%d")

Violent<-subset(data, (day>="2018-08-01"& day<="2018-08-31"))[,c('day','Zone','Violent')]

Violent<-aggregate(Violent$Violent, list(Violent$Zone), mean)
colnames(Violent)<-c("Zone","Violent")

Property<-subset(data, (day>="2018-08-01"& day<="2018-08-31"))[,c('day','Zone','Property')]

Property<-aggregate(Property$Property, list(Property$Zone), mean)
colnames(Property)<-c("Zone","Property")


beat_map <- left_join(beat_map, Violent, by = c("Zone" = "Zone"))
beat_map <- left_join(beat_map, Property, by = c("Zone" = "Zone"))

beat_map[is.na(beat_map)]<-0

# text
beat_points <- sf::st_point_on_surface(beat_map)
beat_coords <- as.data.frame(sf::st_coordinates(beat_points))
beat_coords$Zone<-beat_map$Zone

beat_coords2<-beat_coords %>%
  group_by(Zone) %>%
  filter(row_number()==ceiling(n()/2))

#plot
p1<-ggplot(beat_map) +  
  geom_sf(aes(fill = Violent))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Violent Crime Report (2018-08)")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")


p2<-ggplot(beat_map) +  
  geom_sf(aes(fill = Property))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Property Crime Report (2018-08)")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")

library(ggpubr)
ggarrange(p1, p2, 
          ncol = 2, nrow = 1, align = "h")
