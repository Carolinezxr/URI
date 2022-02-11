library(readr)
library(sf)           # Objects and functions for geospatial data
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(ggplot2)      # Graphing functions

setwd("E:/data/census")

demographic<-read.csv("DP05_zone_demographic.csv")

# pop weighted
pop<-as.data.frame(cbind(demographic$Zone,demographic$Total.population))
colnames(pop)<-c("Zone","Population")

# crime report in Aug
data<-read.csv("E:\\data\\crimes_data_zone_simplified_summary_day.csv")

data$day<-as.Date(data$day,"%Y-%m-%d")

Violent<-subset(data, (day>="2018-08-01"& day<="2018-08-31"))[,c('day','Zone','Violent')]

Violent<-aggregate(Violent$Violent, list(Violent$Zone), mean)
colnames(Violent)<-c("Zone","Violent")

Property<-subset(data, (day>="2018-08-01"& day<="2018-08-31"))[,c('day','Zone','Property')]

Property<-aggregate(Property$Property, list(Property$Zone), mean)
colnames(Property)<-c("Zone","Property")

# visualization
visual<-merge(pop,Violent)
visual<-merge(visual,Property)

visual$wViolent<-visual$Violent  / visual$Population
visual$wProperty<-visual$Property  / visual$Population

beat_map<-st_read("E:\\data\\amityrosado\\Policing-GIS\\Merged without O'Hare.shp") 
beat_map<-beat_map[,c(6,9)]

beat_map <- left_join(beat_map, visual, by = c("Zone" = "Zone"))

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
  geom_sf(aes(fill = wViolent))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Violent Crime Report per capita (2018-08)")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")


p2<-ggplot(beat_map) +  
  geom_sf(aes(fill = wProperty))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Weighted Property Crime Report per capita (2018-08)")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")

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

# white
pop$White<-demographic$White
pop$WhitePct<-pop$White/pop$Population

beat_map <- left_join(beat_map, pop, by = c("Zone" = "Zone"))
beat_map[is.na(beat_map)]<-0

ggplot(beat_map) +  
  geom_sf(aes(fill = WhitePct))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("White Population Percentage")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")

# female
pop$Female<-demographic$Female
pop$FemalePct<-pop$Female/pop$Population

beat_map <- left_join(beat_map, pop, by = c("Zone" = "Zone"))
beat_map[is.na(beat_map)]<-0

ggplot(beat_map) +  
  geom_sf(aes(fill = FemalePct))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Female Population Percentage")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")

# older than 65
pop$Older<-demographic$X65.years.and.over
pop$OlderPct<-pop$Older/pop$Population

beat_map <- left_join(beat_map, pop, by = c("Zone" = "Zone"))
beat_map[is.na(beat_map)]<-0

ggplot(beat_map) +  
  geom_sf(aes(fill = OlderPct))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Older than 65 Percentage")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")

# younger than 18
pop$Youth<-demographic$Under.18.years
pop$YouthPct<-pop$Youth/pop$Population

beat_map <- left_join(beat_map, pop, by = c("Zone" = "Zone"))
beat_map[is.na(beat_map)]<-0

ggplot(beat_map) +  
  geom_sf(aes(fill = YouthPct))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("Under 18 Percentage")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")
