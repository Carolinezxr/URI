library(readr)
library(sf)           # Objects and functions for geospatial data
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(ggplot2)      # Graphing functions

setwd("E:/data/census")

housing<-read.csv("DP04_zone_housing.csv")

visual<-as.data.frame(cbind(housing$Zone,housing$No.vehicles.available))
colnames(visual)<-c("Zone","vehicles")
visual$occupied<-housing$Occupied.housing.units
visual$no_vehicles_pct<-visual$vehicles/visual$occupied

visual$fuel<-housing$No.fuel.used
visual$no_fuel_pct<-visual$fuel/visual$occupied

visual$owner<-housing$Owneroccupied.units
visual$luxury<-housing$X1000000.or.more
visual$cheap<-housing$Less.than.50000
visual$luxuryPct<-visual$luxury/visual$owner
visual$cheapPct<-visual$cheap/visual$owner

visual$rent<-housing$Occupied.units.paying.rent
visual$highRent<-housing$X3000.or.more
visual$lowRent<-housing$Less.than.500
visual$highRentPct<-visual$highRent/visual$rent
visual$lowRentPct<-visual$lowRent/visual$rent


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
  geom_sf(aes(fill = no_fuel_pct))+  
  scale_fill_gradient(low="#56B1F7", high="#132B43",na.value = "grey50")+
  theme_bw(base_family = "STKaiti")+
  ggtitle("No Heating Pct")+
  geom_text(data=beat_coords2,aes(X,Y,label =Zone), colour = "white")
