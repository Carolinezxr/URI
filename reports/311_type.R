library(readr)

setwd('/project/graziul/ra/xuranzeng')

df<-read_csv("request_simplified.csv")
df$TYPE<-NA


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

#Info<-c('311IOC')

df$TYPE[df$SR_SHORT_CODE %in% TransportationStreet] <- "TransStr"
df$TYPE[df$SR_SHORT_CODE %in% Animals] <- "Animals"
df$TYPE[df$SR_SHORT_CODE %in% ConsumerProtection] <- "ConProt"
df$TYPE[df$SR_SHORT_CODE %in% Garbage] <- "Garbage"
df$TYPE[df$SR_SHORT_CODE %in% HomeBuilding] <- "HomeBuild"
df$TYPE[df$SR_SHORT_CODE %in% environment] <- "Enviorn"
#df$TYPE[df$SR_SHORT_CODE %in% Info] <- "311Info"

df2 <- df[!is.na(df$TYPE),]

public<-c("TransStr","HomeBuild","Enviorn")
private<-c("Garbage","Animals")
other<-c("ConProt")

df2$TYPE2<-NA
df2$TYPE2[df2$TYPE %in% public] <- "public"
df2$TYPE2[df2$TYPE %in% private] <- "private"
df2$TYPE2[df2$TYPE %in% other] <- "other"


#write.csv(df2,"request_simplified_type.csv")

