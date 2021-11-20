library(readr)

library(lubridate)

library(ggplot2)
library(dplyr)

library(date)


crimes_data<-read_csv("E:\\data\\project\\crimes_data.csv",)
crimes_data$day<-as.Date(unlist(crimes_data$Date),"%m/%d/%Y")

crimes_data_sample<-subset(crimes_data,day=="2018-08-12")
crimes_data_sample<-crimes_data_sample[,c(2, 3,4,10)]
crimes_data_sample2<-subset(crimes_data_sample,District=="016" |District=="017" )

crimes_data_sample2$Date<-as.POSIXct(crimes_data_sample2$Date, format="%m/%d/%Y %I:%M:%S %p")

crimes_data_sample2$hour<-hour(as.POSIXct(crimes_data_sample2$Date, format="%m/%d/%Y %I:%M:%S %p"))

raw_crimes<-aggregate(crimes_data_sample2[,c(2,5)],by=list(type=crimes_data_sample2$`Primary Type`, hour=crimes_data_sample2$hour),length)

raw_crimes<-raw_crimes[,c(2,1,3)]

colnames(raw_crimes)=c('hour',"type","value")



raw_crimes1<-raw_crimes[1:16,]
ggplot(raw_crimes1, ,mapping = aes(hour,value,fill=type)) +
  geom_bar(width = 2, colour="grey",stat = "identity") +
  theme_minimal() +
  scale_fill_brewer() +
  coord_polar(start=0) +
  scale_x_continuous("", limits = c(0, 12), breaks = seq(0, 12), labels = seq(0,12))

my_data <- read.table("E:\\data\\counts.txt")
my_data<-cbind(as.data.frame(as.numeric(rownames(my_data))-1),my_data)
colnames(my_data)<-c("hour","Cluster1","Cluster2","Cluster3")               
library(reshape2)
plot<-melt(my_data,id = "hour")


crime<-ggplot(raw_crimes,mapping = aes(hour,value,fill=type))+
  geom_bar(stat='identity') +
  labs(x = 'hour',y = 'num_reports') +
  theme_bw()


cluster<-ggplot(plot,mapping = aes(hour,value,fill=variable))+
  geom_bar(stat='identity') +
  labs(x = 'hour',y = 'clusters') +
  theme_bw() 

library(ggpubr)
ggarrange(crime, cluster, 
          labels = c("crime", "teo"),
          ncol = 1, nrow = 2, align = "v")




cluster<-ggplot(plot,mapping = aes(hour,value,fill=variable))+
  geom_bar(stat='identity') +  
  theme_minimal() +
  coord_polar(start=0) +
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

crime<-ggplot(raw_crimes,mapping = aes(hour,value,fill=type))+
  geom_bar(stat='identity') +  
  theme_minimal() +
  coord_polar(start=0) +
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

ggarrange(crime, cluster, 
          labels = c("crime", "teo"),
          ncol = 1, nrow = 2, align = "v")

