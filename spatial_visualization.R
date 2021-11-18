library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
library(pdftools)
library(RSocrata)

socrata.file <- "https://data.cityofchicago.org/resource/suj7-cg3j.csv"
vehicle.data <- read.socrata(socrata.file)
vehicle.sept16 <- vehicle.data %>% filter(year(creation_date) == 2016) %>%
  filter(month(creation_date) == 9)
vehicles.final <- vehicle.sept16 %>% select(comm = community_area, 
                                            lat = latitude, lon = longitude)
vehicle.points <- st_transform(vehicle.points,32616)

vehicle.coord <- vehicles.final %>% filter(!(is.na(lat)))
vehicle.points = st_as_sf(vehicle.coord, coords = c("lon", "lat"), crs = 4326, agr = "constant")
plot(vehicle.points)



comm.file <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson"
chicago.comm <- read_sf(comm.file)
chicago.comm <- st_transform(chicago.comm,32616)

vehicle.points <- st_transform(vehicle.points,32616)
comm.pts <- st_join(vehicle.points,chicago.comm["area_num_1"])
comm.pts$area_num_1 <- as.integer(comm.pts$area_num_1)
chicago.comm$area_num_1 <- as.integer(chicago.comm$area_num_1)
veh.cnts <- comm.pts %>% count(area_num_1)
veh.cnts <- veh.cnts %>% rename(comm = area_num_1, AGG.COUNT = n)
chicago.comm <- st_join(chicago.comm,veh.cnts, by = c("area_num_1" = "comm"))
tm_shape(chicago.comm) +
  tm_polygons("AGG.COUNT")
