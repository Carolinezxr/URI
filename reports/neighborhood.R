setwd('E:\\data\\project\\report_request')

Zone<-seq(1,13,1)

zone<-as.data.frame(Zone)

NH1<-c(2,3,11,12)
NH2<-c(3,4,1,11)
NH3<-c(1,2,4,13,10,12)
NH4<-c(2,3,13,5)
NH5<-c(4,13,6,7)
NH6<-c(10,13,5,7,8,9)
NH7<-c(5,6,8)
NH8<-c(6,7,9)
NH9<-c(6,8)
NH10<-c(3,12,13,6)
NH11<-c(1,2)
NH12<-c(1,3,10)
NH13<-c(3,4,5,6,10)
NH<-list(NH1,NH2,NH3,NH4,NH5,NH6,NH7,NH8,NH9,NH10,NH11,NH12,NH13)

census<-read.csv('selected_census.csv')[,-1]%>%
  select(-Violent,-Property,-ViolentCapta,-PropertyCapta,-MedIncome)

census[,7:10]<-NA
colnames(census)[7:10]<-paste('N',colnames(census)[3:6],sep='')

head(census)

for (i in 1:13){
  NHi<-unlist(NH[i])
  NHzone1<-subset(census,Zone %in% NHi)%>%
    mutate(w = Population/sum(Population))
  census[i,7:10]<-t(t(NHzone1[,3:6]) %*% NHzone1$w)
}

#write.csv(census,'neighborhood.csv')

rr<-read.csv('report_request_daily.csv')%>%
  left_join(census[,c('Zone','Population')],by='Zone')
rr<-na.omit(rr)
rr[,9:13]<-NA
colnames(rr)[9:13]<-paste('N',colnames(rr)[2:6],sep='')


for (i in 1:nrow(rr)){
  zone<-rr[i,'Zone']
  date<-rr[i,'Day']
  NHi<-unlist(NH[zone])
  NHzone1<-subset(rr,Zone %in% NHi & Day==date)%>%
    mutate(w=Population/sum(Population))
  rr[i,9:13]<-t(t(NHzone1[,2:6]) %*% NHzone1$w)
  cat('processing:',i,'\n')
}
#write.csv(census,'report_request_neighborhood.csv')
