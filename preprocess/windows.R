library(readr)
#library(rmgarch,lib='~/R/x86_64-pc-linux-gnu-library/3.2/')
library(rugarch)
library(lubridate)
library(zoo)
library(xts)
library(ggplot2)
library(dplyr)
library(knitr)
library(corrplot)
library(forecast)
#library(rmgarch)
library(xtable)
library(fBasics)
library(car)
library(date)
library(parallel)
library(forecast)
library(tseries)
library(FinTS)
library(TTR)
library(PerformanceAnalytics)
library(forcats)

trans<-function(x){
  a<-aggregate(x$`Primary Type`,by=list(day=x$day, type=x$`Primary Type`),length)
  
  type_ts2<-as.data.frame(unique(a$day))
  colnames(type_ts2)<-"day"
  for (i in 1:length(unique(a$type))){
    b<-subset(a[which(a$type==unique(a$type)[i]),])
    type_ts2<-merge(type_ts2,b[,c(1,3)],by="day",all=TRUE)
    colnames(type_ts2)[(i+1)]<-unique(a$type)[i]
  }
  
  b<-type_ts2
  rownames(b)<-b$day
  b<-b[,-1]
}

crimes_data<-read_csv("/home/xuranzeng/project/crimes_data.csv",)
crimes_data$day<-date(as.Date(unlist(crimes_data$Date),"%m/%d/%Y"))
crimes_data_sample<-crimes_data[,c(5,10, 3,4,8,9)]
crimes_types<-c("ASSAULT","BATTERY" , "HOMICIDE", "ROBBERY" )
crimes_data_sample1<-subset(crimes_data_sample,crimes_data_sample[,3]=="HOMICIDE")
# sum all crimes each day
#raw_frequency<-subset(crimes_data_sample[which(crimes_data_sample$`Primary Type`  %in% crimes_types),])
crimes_data_sample<-crimes_data_sample1
raw_crimes<-aggregate(crimes_data_sample$day,by=list(Beat=crimes_data_sample$Beat, day=crimes_data_sample$day),length)
colnames(raw_crimes)<-c("Beat","Date","#crimes")
#write.csv(raw_crimes,"/home/xuranzeng/project/data/raw_crimes_beat.csv")

raw_crimes2<-as.data.frame(unique(raw_crimes$Date))
colnames(raw_crimes2)<-"Date"
for (i in 1:length(unique(raw_crimes$Beat))){
  b<-subset(raw_crimes, Beat==unique(raw_crimes$Beat)[i])
  raw_crimes2<-merge(raw_crimes2,b[,c(2,3)],by="Date",all=TRUE)
  colnames(raw_crimes2)[(i+1)]<-unique(raw_crimes$Beat)[i]
  cat("merging:",i,'\n')
}
raw_crimes2[is.na(raw_crimes2)]<-0
#write.csv(raw_crimes2,"/home/xuranzeng/project/data/homicide_crimes_beat_transfer.csv")







raw_crimes2<-read_csv("/home/xuranzeng/project/data/raw_crimes_beat_transfer.csv",)
rownames(raw_crimes2)<-raw_crimes2$Date
head(raw_crimes2)

library(RcppRoll)

#write.csv(roll_sum_30,"/home/xuranzeng/project/data/raw_crimes_beat_roll_sum_30.csv")


roll_vol_i<-function(data, i){
  roll_vol_30<-as.data.frame(data[i:nrow(data),'Date'])
  rownames(roll_vol_30)<-roll_vol_30[,1]
  raw_crimes2<-data[,3:ncol(data)]
  for (j in 1:ncol(raw_crimes2)){
    roll_vol_30[,j]<-roll_sd(unlist(raw_crimes2[,j]),n=i)
  }
  colnames(roll_vol_30)<-colnames(raw_crimes2)
  return(roll_vol_30)
}

roll_vol_30<-roll_vol_i(raw_crimes2,30)

roll_sum_i<-function(data, i){
  roll_sum_30<-as.data.frame(data[i:nrow(data),'Date'])
  rownames(roll_sum_30)<-roll_sum_30[,1]
  raw_crimes2<-data[,3:ncol(data)]
  for (j in 1:ncol(raw_crimes2)){
    roll_sum_30[,j]<-roll_sum(unlist(raw_crimes2[,j]),n=i)
  }
  colnames(roll_sum_30)<-colnames(raw_crimes2)
  roll_sum_30$sum<-rowSums(roll_sum_30)
  return(roll_sum_30)
}

roll_summary<-function(data,interval){
  roll_sum_30_summary<-as.data.frame(colnames(data))
  
  maxwindow<-NA
  max_num<-NA

  for (i in 1:ncol(data)){
    beat<-colnames(data)[i]
    max<-which.max(data[,beat])
    maxwindow[i]<-paste(rownames(data)[max-interval],rownames(data)[max],sep = '-')
    max_num[i]<-data[max,beat]
  }
  
  max_vol_window<-NA
  max_vol<-NA
  

  for (i in 1:ncol(data)){
    beat<-colnames(data)[i]
    max<-which.max(data[,beat])
    maxwindow[i]<-paste(rownames(data)[max-interval],rownames(data)[max],sep = '-')
    max_num[i]<-data[max,beat]
  }

  
  roll_sum_30_summary$max_num_window<-maxwindow
  roll_sum_30_summary$max_num<-max_num

  
  return(roll_sum_30_summary)
  
}

plot_max<-function(data,i){
    beat<-colnames(data)[i]
    max<-which.max(data[,beat])
    p<-ggplot(data=data,aes(x=as.Date(rownames(data)),y=data[,beat],xlab="year",ylab=beat))+
      geom_line()+
      geom_point(data = roll_sum_30,aes(x=as.Date(rownames(data)[max]),y=data[max,beat]), color="red", size=3)
    cat("highest reported crimes for beat",beat,":",rownames(data)[max-30],'-',rownames(data)[max],'\n')
    cat("# reported crimes:",data[max,beat],'\n')
    return(p)
}

roll_sum_30$sum<-rowSums(roll_sum_30)


plot_max(roll_sum_30,305)

roll_sum_30_summary<-roll_summary(roll_sum_30,30)



pattern<-read_csv("/home/xuranzeng/project/data/roll_sum_30_summary.csv",)
pattern<-pattern[,-1]
pattern1<-pattern[,1:3]
for (i in (1:nrow(pattern1))){
  ymd<-substring(pattern1[i,2],12,21)
  pattern1[i,'year']<-year(ymd(ymd))
  pattern1[i,'month']<-month(ymd(ymd))
}

pattern2<-aggregate(pattern1$month,by=list(month=pattern1$month),length)

pattern2 %>%
  ggplot( aes(x=month, y=x)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("num of best") +
  xlab("month")+
  theme_bw()

pattern1<-pattern[,c(1,4,5)]
for (i in (1:nrow(pattern1))){
  ymd<-substring(pattern1[i,2],12,21)
  pattern1[i,'year']<-year(ymd(ymd))
  pattern1[i,'month']<-month(ymd(ymd))
}

pattern2<-aggregate(pattern1$month,by=list(month=pattern1$month),length)
pattern2 %>%
  ggplot( aes(x=month, y=x)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("num of best") +
  xlab("month")+
  theme_bw()

pattern<-read_csv("/home/xuranzeng/project/data/roll_sum_7_summary.csv",)
pattern<-pattern[,-1]
pattern1<-pattern[,1:3]
for (i in (1:nrow(pattern1))){
  ymd<-substring(pattern1[i,2],12,21)
  pattern1[i,'year']<-year(ymd(ymd))
  pattern1[i,'week']<-week(ymd(ymd))
}

pattern2<-aggregate(pattern1$week,by=list(week=pattern1$week),length)

pattern2 %>%
  ggplot( aes(x=week, y=x)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("num of beat (max sum)") +
  xlab("week")+
  theme_bw()

pattern1<-pattern[,c(1,4,5)]
for (i in (1:nrow(pattern1))){
  ymd<-substring(pattern1[i,2],12,21)
  pattern1[i,'year']<-year(ymd(ymd))
  pattern1[i,'week']<-week(ymd(ymd))
}

pattern2<-aggregate(pattern1$week,by=list(week=pattern1$week),length)
pattern2 %>%
  ggplot( aes(x=week, y=x)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("num of beat (max volatility)") +
  xlab("week")+
  theme_bw()


crimes_data<-read_csv("/home/xuranzeng/project/crimes_data.csv",)
crimes_data$day<-date(as.Date(unlist(crimes_data$Date),"%m/%d/%Y"))
crimes_data_sample<-crimes_data[,c(5,10, 3,4,8,9)]

raw_crimes<-aggregate(crimes_data_sample$day,by=list(District=crimes_data_sample$District, day=crimes_data_sample$day),length)
colnames(raw_crimes)<-c("District","Date","#crimes")

raw_crimes2<-as.data.frame(unique(raw_crimes$Date))
colnames(raw_crimes2)<-"Date"
for (i in 1:length(unique(raw_crimes$District))){
  b<-subset(raw_crimes, District==unique(raw_crimes$District)[i])
  raw_crimes2<-merge(raw_crimes2,b[,c(2,3)],by="Date",all=TRUE)
  colnames(raw_crimes2)[(i+1)]<-unique(raw_crimes$District)[i]
  cat("merging:",i,'\n')
}
raw_crimes2[is.na(raw_crimes2)]<-0
#write.csv(raw_crimes2,"/home/xuranzeng/project/data/raw_crimes_district_transfer.csv")






raw_crimes2<-read_csv("/home/xuranzeng/project/data/raw_crimes_district_transfer.csv",)
rownames(raw_crimes2)<-raw_crimes2$Date

library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
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
