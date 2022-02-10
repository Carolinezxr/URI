library(readr)

setwd("E:/data/census")

map<-read.csv("map.csv")[,c("NAMELSAD",'Zone')]
#census<-as.data.frame(t(read.csv("DP05.csv")))
#census<-as.data.frame(t(read.csv("ACSDP5Y2018.DP05-2022-02-10T214846.csv")))
#census<-as.data.frame(t(read.csv("ACSDP5Y2018.DP04-2022-02-10T212751.csv")))
census<-as.data.frame(t(read.csv("ACSDP5Y2018.DP03-2022-02-10T212857.csv")))
#census<-as.data.frame(t(read.csv("ACSDP5Y2018.DP02-2022-02-10T212955.csv")))


# process census
census[1,]<-gsub("[^0-9A-Za-z///' ]", "", census[1,])
colnames(census)<-census[1,]
census<-census[-1,]

# census tract
census$tract<-rownames(census)
census$tract<-sub("\\..Cook.*", "", census$tract)
census<-census[!duplicated(census$tract),]
rownames(census)<-census$tract

census$tract2<-sub(".*Census.Tract.", "", census$tract)


# map tract
map$tract2<-sub(".*Census Tract ", "", map$NAMELSAD)

# 
DP05<-merge(map,census,by='tract2')
DP05<-DP05[,c(-1,-2,-ncol(DP05))]

DP05 <- mutate_all(DP05, function(x) as.numeric(gsub(",","",x,fixed=TRUE))) # commas in data

DP05_zone<-aggregate(DP05[,2:ncol(DP05)], list(DP05$Zone), sum)
colnames(DP05_zone)[1]<-"Zone"

#write.csv(DP05_zone,"DP03_zone_economics.csv")
