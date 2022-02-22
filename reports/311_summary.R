library(readr)

setwd('/project/graziul/ra/xuranzeng')


request<-read_csv("request_simplified.csv")
request$DATE<-as.Date(request$CREATED_DATE, "%m/%d/%Y")

start_date<-"2018-07-01"
end_date<-"2019-06-30"

request1<-request[,c(2,3,8)]

request1<-request1[request1$DATE >= start_date & request1$DATE <= end_date, ]

a<-request1 %>% 
  count(SR_TYPE, DATE) %>%
  group_by(SR_TYPE)%>% 
  summarise(Mean=mean(n), Median=median(n), Std=sd(n))

a$Mean<-round(a$Mean,2)
a$Median<-round(a$Median,2)
a$Std<-round(a$Std,2)

#write.csv(a,"/project/graziul/ra/xuranzeng/311_summary.csv")


request<-read_csv("request_simplified_type.csv")
request$DATE<-as.Date(request$CREATED_DATE, "%m/%d/%Y")

start_date<-"2018-07-01"
end_date<-"2019-06-30"
request1<-request[,9:11]

request1<-request1[request1$DATE >= start_date & request1$DATE <= end_date, ]
a<-request1 %>% 
  count(TYPE, DATE) %>%
  group_by(TYPE)%>% 
  summarise(Mean=mean(n), Median=median(n), Std=sd(n))

a<-request1 %>%   
  count(TYPE2, DATE) %>%
  group_by(TYPE2)%>% 
  summarise(Mean=mean(n), Median=median(n), Std=sd(n))


a$Mean<-round(a$Mean,2)
a$Median<-round(a$Median,2)
a$Std<-round(a$Std,2)
