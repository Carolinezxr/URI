library(readr)

library(lubridate)

library(ggplot2)
library(dplyr)

library(date)

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

ggplot(raw_crimes,mapping = aes(hour,value,fill=type))+
  geom_bar(stat='identity') +
  labs(x = 'hour',y = 'num_reports') +
  theme(axis.title =element_text(size = 16),axis.text =element_text(size = 14, color = 'black'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw()

raw_crimes1<-raw_crimes[1:16,]
ggplot(raw_crimes1, ,mapping = aes(hour,value,fill=type)) +
  geom_bar(width = 2, colour="grey",stat = "identity") +
  theme_minimal() +
  scale_fill_brewer() +
  coord_polar(start=0) +
  scale_x_continuous("", limits = c(0, 12), breaks = seq(0, 12), labels = seq(0,12))
