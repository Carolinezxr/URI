#############
# functionality: categorize 196 types of requests into 7 types (TYPE1) or 4 types (TYPE2) and report summary statistics
# input: clean request data
# output: TYPE1, TYPE2, cleaned census data
# raw data path: /project/graziul/ra/xuranzeng
#############

library(readr)

setwd('/project/graziul/ra/xuranzeng')

df<-read_csv("request_simplified.csv")
df$TYPE<-NA

############# TYPE1
TransportationStreet<-c("SKA",'SFA','PHB','AAI','PCL','CSC',
                        'QAC','PCL3','SDO','FPC','WBT','PBE',
                        'PETCO','PHF','PBLDR','CSP','TNP',
                        'AAD','CHECKFOR','CAFE','PBS','PCE',
                        'PCD','PCC','PCB','SDW','SNPBLBS','SWSNOREM',
                        'SDP','SFN','SFD','SFK','SFQ','SFB','VBL',
                        'SFC','AAF','AAE')

Animals<-c('EBD','SGG','CIAC','SGQ','SGV','EAB','PET','EAQ','SGA','EAE','EAF')

ConsumerProtection <-c('RBL','CSF','CST','INR','LPRC','LIQUORCO',
                       'ODM','PSL','CORNVEND','HFB','HFF','BAG',
                       'BAM','MWC')

Garbage<-c("SCT",'SDR','SIE','SCC','SCX','SCB','SCS','SCQ')

HomeBuilding<-c('AVN','BBA','BUNGALOW','OCC','FAC',
                'GRAF','HOP','HDF','WBK','BBD',
                'NOSOLCPP','WBJ','BPI','RFC','SHVR','BBK','WCA')

environment<-c('NAA','JNS','SEL','SED','SEE','SEF','SCP')

Info<-c('311IOC')

df$TYPE[df$SR_SHORT_CODE %in% TransportationStreet] <- "TransStr"
df$TYPE[df$SR_SHORT_CODE %in% Animals] <- "Animals"
df$TYPE[df$SR_SHORT_CODE %in% ConsumerProtection] <- "ConProt"
df$TYPE[df$SR_SHORT_CODE %in% Garbage] <- "Garbage"
df$TYPE[df$SR_SHORT_CODE %in% HomeBuilding] <- "HomeBuild"
df$TYPE[df$SR_SHORT_CODE %in% environment] <- "Enviorn"
df$TYPE[df$SR_SHORT_CODE %in% Info] <- "311Info"
############# 

############# TYPE2
df2 <- df[!is.na(df$TYPE),]

public<-c("TransStr","HomeBuild","Enviorn")
private<-c("Garbage","Animals")
other<-c("ConProt")
info<-c("311Info")

df2$TYPE2<-NA
df2$TYPE2[df2$TYPE %in% public] <- "public"
df2$TYPE2[df2$TYPE %in% private] <- "private"
df2$TYPE2[df2$TYPE %in% other] <- "other"
df2$TYPE2[df2$TYPE %in% info] <- "311Info"
############# 


#write.csv(df2,"request_simplified_type_info.csv")

############# summary statistics
request<-df2
request$DATE<-as.Date(request$CREATED_DATE, "%m/%d/%Y")

start_date<-"2018-07-01"
end_date<-"2019-06-30"

request1<-request[,c("DATE","SR_TYPE","SR_SHORT_CODE","TYPE","TYPE2")]
request1<-request1[request1$DATE >= start_date & request1$DATE <= end_date, ]

# all 196 types
table<-request1 %>% 
  count(SR_TYPE, DATE) %>%
  group_by(SR_TYPE)%>% 
  summarise(Mean=mean(n), Median=median(n), Std=sd(n))

# TYPE: 7 types
table<-request1 %>% 
  count(TYPE, DATE) %>%
  group_by(TYPE)%>% 
  summarise(Mean=mean(n), Median=median(n), Std=sd(n))

# TYPE2: 4 types
table<-request1 %>% 
  count(TYPE2, DATE) %>%
  group_by(TYPE2)%>% 
  summarise(Mean=mean(n), Median=median(n), Std=sd(n))
############# 

