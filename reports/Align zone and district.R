crimes_data<-read_csv("E://data//project//crimes_data.csv",)
crimes_data$day<-date(as.Date(unlist(crimes_data$Date),"%m/%d/%Y"))
crimes_data$Zone<-NA
crimes_data$Zone[crimes_data$District==16 |crimes_data$District==17] <- 1
crimes_data$Zone[crimes_data$District==19] <- 2
crimes_data$Zone[crimes_data$District==12 |crimes_data$District==14] <- 3
crimes_data$Zone[crimes_data$District==1 |crimes_data$District==18] <- 4
crimes_data$Zone[crimes_data$District==2] <- 5
crimes_data$Zone[crimes_data$District==7 |crimes_data$District==8] <- 6
crimes_data$Zone[crimes_data$District==3] <- 7
crimes_data$Zone[crimes_data$District==4 |crimes_data$District==6] <- 8
crimes_data$Zone[crimes_data$District==5 |crimes_data$District==22] <- 9
crimes_data$Zone[crimes_data$District==10 |crimes_data$District==11] <- 10
crimes_data$Zone[crimes_data$District==20 |crimes_data$District==24] <- 11
crimes_data$Zone[crimes_data$District==15 |crimes_data$District==25] <- 12
crimes_data$Zone[crimes_data$District==9] <- 13
#write.csv(crimes_data,"E://data//project//crimes_data_zone.csv")