###############
# title: outliers visualization
# author: Xuran Zeng
# objective: Within the sample year(20180817-20190817), visualize average crimes 
# reports for each weekday, and see if the sample week (20180810-20180816) 
# contains outliers.
##############

############### load package and input ##############
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(reshape2)

crimes_data<-read_csv("/home/xuranzeng/project/data/crimes_data_zone.csv")
crimes_data_sample<-crimes_data1[,c("day","Zone","Primary Type")]

############### subset violent/non_violent reports #####################
crimes_types<-c("ASSAULT","BATTERY" , "HOMICIDE", "ROBBERY" )
crimes_data_sample_violent<-subset(crimes_data_sample,crimes_data_sample$`Primary Type` %in% crimes_types)
crimes_data_sample_non_violent<-subset(crimes_data_sample,!(crimes_data_sample$`Primary Type` %in% crimes_types))


############### define function #####################
num_reports<-function(data,num){
    data %>% 
    mutate(wday = wday(day))%>%
    group_by(Zone, wday) %>%
    summarise(n = round(n()/num,2))
}

sd_reports <- function(data){
  data %>% 
    mutate(wday = wday(day))%>%
    mutate(week = week(day))%>%
    group_by(Zone, week, wday) %>%
    summarise(n = n()) %>%
    group_by(Zone, wday) %>%
    summarise(sd = sd(n)) 
}

plot<-function(data1, data2, zone,sd){
  sdp<-subset(sd,Zone==zone )$sd
  zone1_all<-subset(data1,Zone==zone)
  zone1_sample<-subset(data2,Zone==zone)
  
  ggplot() +
    geom_col( data = zone1_all,aes(x = wday,y = n),fill="skyblue", alpha=0.7) +
    
    geom_errorbar( data = zone1_all,aes(x=wday, ymin=n-2*sdp, ymax=n+2*sdp), width=0.4, colour="orange", alpha=0.9, size=1.3)+
    geom_line(data = zone1_sample,aes(x = wday,y = n),colour="red", alpha=0.9, size=1.3)+
    geom_point(data = zone1_sample,aes(x = wday,y = n),shape=21,fill="white")+
    theme_bw()+
    geom_text(data = zone1_all,aes(x = wday,y = n,label =n),hjust = 0, nudge_x = 0.05)+
    geom_text(data = zone1_sample,aes(x = wday,y = n,label =n),hjust = 0, nudge_x = 0.05)
}

################ subset sample week and sample year####################
# sample year
start_day<-as.Date("2018-08-17")
end_day<-as.Date("2019-08-17")
sample_year <- subset(crimes_data_sample,day %in% seq(start_day, end_day,"days"))
#sample_year <- subset(crimes_data_sample_violent,day %in% seq(start_day, end_day,"days"))
#sample_year <- subset(crimes_data_sample_non_violent,day %in% seq(start_day, end_day,"days"))



start_day<-as.Date("2018-08-10")
end_day<-as.Date("2018-08-16")
sample_week <- subset(crimes_data_sample,day %in% seq(start_day, end_day,"days"))
#sample_week <- subset(crimes_data_sample_violent,day %in% seq(start_day, end_day,"days"))
#sample_week <- subset(crimes_data_sample_non_violent,day %in% seq(start_day, end_day,"days"))


################ get mean and sd of reports ####################
num_year<-num_reports(sample_year,53)
num_week<-num_reports(sample_week,1)
sd<-sd_reports(sample_year)



################ Visualization ####################
chart<-c(paste0('chart',1:13))
for (i in 1:13){
  p<-plot(num_year,num_week,i,sd)
  assign(chart[i],p)
}

ggarrange(chart1,chart2,chart3,chart4,chart5,chart6,
          ncol = 3, nrow = 2, align = "v",
          labels = c("Zone1","Zone2","Zone3","Zone4","Zone5","Zone6")
          )
ggarrange(chart7,chart8,chart9,chart10,chart11,chart12,chart13,
          ncol = 3, nrow = 3, align = "v",
          labels = c("Zone7","Zone8","Zone9","Zone10","Zone11","Zone12","Zone13")
)
###################################################

################ summary ####################
colnames(num_year)[3]<-"year"
colnames(num_week)[3]<-"week"

summary<-num_year %>%
  left_join(num_week, by=c("Zone","wday")) %>%
  left_join(sd,by=c("Zone","wday"))%>%
  mutate(upper = year+2*sd)%>%
  mutate(lower =year-2*sd)%>%
  mutate(outlier = ifelse(week>upper, 1, NA))%>%
  mutate(outlier = ifelse(week<lower, -1, outlier))%>%
  mutate(outlier = ifelse(is.na(outlier), 0, outlier))
  


ggplot(data=summary, aes(x=wday, y=Zone))+
  theme_bw()+
  geom_tile(aes(fill=factor(outlier)),colour="black")+
  scale_fill_manual(values=c( "white", "red"))+
  ggtitle("Outliers")+
  xlab("wdays")+
  ylab("zones")+
  scale_y_continuous("Zone", labels = as.character(summary$Zone), breaks = summary$Zone)+
  scale_x_continuous("wday", labels = as.character(summary$wday), breaks = summary$wday)
