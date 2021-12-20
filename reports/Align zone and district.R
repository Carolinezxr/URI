library(readr)

library(lubridate)

library(dplyr)

crimes_data<-read_csv("E://data//project//crimes_data.csv",)
crimes_data$day<-date(as.Date(unlist(crimes_data$Date),"%m/%d/%Y"))
crimes_data$Zone<-NA
unique(crimes_data$District)
crimes_data$Zone[crimes_data$District=="016" |crimes_data$District=="017"] <- 1
crimes_data$Zone[crimes_data$District=="019"] <- 2
crimes_data$Zone[crimes_data$District=="012" |crimes_data$District=="014"] <- 3
crimes_data$Zone[crimes_data$District=="001" |crimes_data$District=="018"] <- 4
crimes_data$Zone[crimes_data$District=="002"] <- 5
crimes_data$Zone[crimes_data$District=="007" |crimes_data$District=="008"] <- 6
crimes_data$Zone[crimes_data$District=="003"] <- 7
crimes_data$Zone[crimes_data$District=="004" |crimes_data$District=="006"] <- 8
crimes_data$Zone[crimes_data$District=="005" |crimes_data$District=="022"] <- 9
crimes_data$Zone[crimes_data$District=="010" |crimes_data$District=="011"] <- 10
crimes_data$Zone[crimes_data$District=="020" |crimes_data$District=="024"] <- 11
crimes_data$Zone[crimes_data$District=="015" |crimes_data$District=="025"] <- 12
crimes_data$Zone[crimes_data$District=="009"] <- 13
a<-crimes_data[is.na(crimes_data$Zone),c("District","Zone")]
b<-crimes_data[!is.na(crimes_data$Zone),]

write.csv(b,"E://data//project//crimes_data_zone.csv")
