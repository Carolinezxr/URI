library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(zoo)
library(TTR)

################### preprocessing ##################
request<-read_csv("/project/graziul/ra/xuranzeng/request_simplified.csv")
type<-read_csv("/project/graziul/ra/xuranzeng/311_Service_Requests_-_Request_Types.csv")
request$DATE<-as.Date(request$CREATED_DATE, "%m/%d/%Y")

report<-read_csv("/home/xuranzeng/project/data/crimes_data_zone_simplified.csv")

start_date<-"2018-07-01"
end_date<-"2021-07-01"
district <- c("4",'6') #zone1

request$POLICE_DISTRICT<-as.numeric(request$POLICE_DISTRICT) # request data start from 2018-07-01
request1<-request[request$DATE >= start_date & request$DATE <= end_date & request$POLICE_DISTRICT %in% district, ]
report1<-report[report$day >= start_date & report$day <= end_date & report$District %in% district, ]

# label crime
violent_crime <- c("HOMICIDE","ASSAULT","BATTERY")
property_crime <- c("BURGLARY","THEFT","MOTOR VEHICLE THEFT")

report1$Violent<-ifelse(report1$`Primary Type` %in% violent_crime, TRUE, FALSE)
report1$Property<-ifelse(report1$`Primary Type` %in% property_crime, TRUE, FALSE)
report1$Total <- TRUE

report1 = report1[,c("day","Violent","Property","Arrest","Domestic","Total","Primary Type")]
report1_sum = report1%>%
  group_by(day) %>%
  summarise((across(1:5, sum)))

df<-request1 %>%
  group_by(DATE) %>%
  summarise(Request = n()) %>%
  rename("day"='DATE') %>%
  inner_join(report1_sum)

df_roll <- df %>%
  mutate(Request_roll = rollmean(Request, k = 7, fill = NA),
         Violent_roll = rollmean(Violent, k = 7, fill = NA),
         Property_roll = rollmean(Property, k = 7, fill = NA),
         Arrest_roll = rollmean(Arrest, k = 7, fill = NA),
         Total_roll = rollmean(Total, k = 7, fill = NA),
         Domestic_roll = rollmean(Domestic, k = 7, fill = NA)) 

df<-na.omit(df_roll[,c(1,8,9,10,11,12,13)])
colnames(df)<-c("day","Request","Violent","Property","Arrest","Total","Domestic")
#write.csv(df,"/project/graziul/ra/xuranzeng/zone1.csv")