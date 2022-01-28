library(readr)
library(dplyr)

library(zoo)
library(TTR)
library(tidyverse)

################### preprocessing ##################
request<-read_csv("/project/graziul/ra/xuranzeng/request_simplified.csv")
request$DATE<-as.Date(request$CREATED_DATE, "%m/%d/%Y")
request$POLICE_DISTRICT<-as.numeric(request$POLICE_DISTRICT) 


report<-read_csv("/home/xuranzeng/project/data/crimes_data_zone_simplified.csv")


#start_date<-"2018-07-01"
#end_date<-"2020-07-01"

#district <- 1:31
#district <- c("16",'17') #zone1
#district <- c("19") #zone2
#district <- c("12","14") #zone3
#district <- c("1","18") #zone4
#district <- c("2") #zone5
#district <- c("7","8") #zone6
#district <- c("3") #zone7
#district <- c("4","6") #zone8
#district <- c("5","22") #zone9
#district <- c("10","11") #zone10
#district <- c("20","24") #zone11
#district <- c("15","25") #zone12
district <- c("9") #zone13

#request1<-request[request$DATE >= start_date & request$DATE <= end_date & request$POLICE_DISTRICT %in% district, ]
request1<-request[request$POLICE_DISTRICT %in% district, ]


request2<-request1 %>%
  group_by(SR_TYPE,DATE) %>%
  summarise(n = n())%>% 
  ungroup()%>%
  pivot_wider(names_from = SR_TYPE, values_from = n)

rownames(request2)<-as.Date(request2$DATE)

request3<-request2 %>%
  mutate(across(colnames(request2)[-1]  , ~ rollmean(., 7, fill = NA, align = 'right'), 
                .names = '{col}_mean')) %>%
  ungroup %>%
  select(-colnames(request2)[-1] )


# na_count <-sapply(request3, function(y) sum(length(which(is.na(y)))))
# na_count <- data.frame(na_count)
# na_count$pct <- round(na_count[,1]/nrow(request3),2)*100



#report1<-report[report$day >= start_date & report$day <= end_date & report$District %in% district, ]
report1<-report[report$District %in% district, ]

# label crime
violent_crime <- c("HOMICIDE","ASSAULT","BATTERY")
property_crime <- c("BURGLARY","THEFT","MOTOR VEHICLE THEFT")

report1$Violent<-ifelse(report1$`Primary Type` %in% violent_crime, TRUE, FALSE)
report1$Property<-ifelse(report1$`Primary Type` %in% property_crime, TRUE, FALSE)
#report1$Total <- TRUE

report1 = report1[,c("day","Violent","Property")]
report1_sum = report1%>%
  group_by(day) %>%
  summarise((across(1:2, sum)))

report2 <- report1_sum %>%
  mutate(across(colnames(report1_sum)[-1]  , ~ rollmean(., 7, fill = NA, align = 'right'), 
                .names = '{col}_mean')) %>%
  ungroup %>%
  select(-colnames(report1_sum)[-1] )

request4 <- request3 %>%
  fill(names(.))%>%
  rename("day"='DATE') %>%
  inner_join(report2)

colnames(request4)<-gsub(" ","_",unlist(str_replace(unlist(colnames(request4)),'_mean','')))
#write.csv(request4,"/project/graziul/ra/xuranzeng/request_report/zone13.csv")


diff_all<-request4
days<-diff_all[,1]
diff_all[,2:ncol(request4),]<-log(diff_all[,2:ncol(request4),])
a<-as.data.frame(diff(as.matrix(diff_all[,2:ncol(request4),])))
dlog_all<-as.data.frame(cbind(days[2:nrow(days),1],a))


#write.csv(dlog_all,"/project/graziul/ra/xuranzeng/request_report/dzone13.csv")
