library(readr)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)
library(xts)
library(ggplot2)
library(dplyr)
Bpc_freq<-read_csv("/project/graziul/data/bpc_freq.csv")
Bpc_freq$ts<-ymd_hm(unlist(Bpc_freq[,3]))
Bpc_freq<-Bpc_freq[,c(5,1,2,3,4)]


#description(zone-days-start-end-files-hours of speech)
zone_list<-unique(Bpc_freq$zone)

des<-data.frame(zone=zone_list)
for (i in 1:length(zone_list)){
  des[i,"ndays"]<-nrow(unique(Bpc_freq[which(Bpc_freq$zone==zone_list[i]),'date']))
  des[i,"start"]<-head(unique(Bpc_freq[which(Bpc_freq$zone==zone_list[i]),'ts']),1)
  des[i,"end"]<-tail(unique(Bpc_freq[which(Bpc_freq$zone==zone_list[i]),'ts']),1)
  des[i,"nfiles"]<-nrow(Bpc_freq[which(Bpc_freq$zone==zone_list[i]),])
  des[i,"num_bpc"]<-sum(Bpc_freq[which(Bpc_freq$zone==zone_list[i]),"num_bpc"])
}
print(des)

zone_1<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 1"),])
zone_10<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 10"),])
zone_11<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 11"),])
zone_12<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 12"),])
zone_13<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 13"),])
zone_2<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 2"),])
zone_3<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 3"),])
zone_4<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 4"),])
zone_5<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 5"),])
zone_6<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 6"),])
zone_8<-as.data.frame(Bpc_freq[which(Bpc_freq$zone=="Zone 8"),])


#see only zone 1


#plot
sma<-stats::filter(ts(zone_1[,c(1,5)])/144,filter=c(rep(1,144)))
ggplot(data = zone_1, aes(x=ts, y=num_bpc)) +  
  geom_line()+
  geom_line(aes(y=sma[,2]),color="red")+
  ggtitle("Zone_1")

#relitu
sum_by_freq<-function(data,freq){
  data %>%
    mutate(freq=freq(data$ts)) %>%
    group_by(freq) %>%
    summarise(sum_by_mon=sum(log(num_bpc)))
}

#month
a1<-sum_by_freq(zone_1,month)
colnames(a1)<-c("month","zone_1")
a2<-sum_by_freq(zone_2,month)
colnames(a2)<-c("month","zone_2")
a3<-sum_by_freq(zone_3,month)
colnames(a3)<-c("month","zone_3")
a4<-sum_by_freq(zone_4,month)
colnames(a4)<-c("month","zone_4")
a5<-sum_by_freq(zone_5,month)
colnames(a5)<-c("month","zone_5")
a6<-sum_by_freq(zone_6,month)
colnames(a6)<-c("month","zone_6")
a8<-sum_by_freq(zone_8,month)
colnames(a8)<-c("month","zone_8")
a10<-sum_by_freq(zone_10,month)
colnames(a10)<-c("month","zone_10")
a11<-sum_by_freq(zone_11,month)
colnames(a11)<-c("month","zone_11")
a12<-sum_by_freq(zone_12,month)
colnames(a12)<-c("month","zone_12")
a13<-sum_by_freq(zone_13,month)
colnames(a13)<-c("month","zone_13")

b<-cbind(a1, a2, a4,  a8, a10, a11, a12, a13)[,c(1,2,4,6,8,10,12,14,16)]
c<-merge(b, a3, by = 'month', all=T)
d<-merge(c, a5, by = 'month', all=T)
e<-merge(d, a6, by = 'month', all=T)
g<-e[,c(1,2,3,10,4,11,12,5,6,7,8,9)]
library(reshape2)
f<-melt(g, id="month")

mymonths<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
month.name<-sort(unique(f$month))
f$month2<-factor(f$month, levels=month.name, labels=mymonths)

ggplot(data=f, aes(x=month2, y=variable))+
  theme_bw(base_family = "STKaiti")+
  geom_tile(aes(fill=value),colour="white")+
  scale_fill_gradient(low="white", high="black")


data$month<-month(data$ts)
data$day<-day(data$ts)
mymonths<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
month.name<-sort(unique(data$month))
data$month2<-factor(data$month, levels=month.name, labels=mymonths)

ggplot(data=data, aes(x=month2, y=day))+
  theme_bw(base_family = "STKaiti")+
  geom_tile(aes(fill=num_bpc),colour="white")

#relitu
plot_hm<-function(data){
  data$week<-week(data$ts)
  data$weekdays<-weekdays(data$ts)
  zone_hm<-ggplot(data=data, aes(x=week, y=weekdays))+
    #theme_bw(base_family = "STKaiti")+
    geom_tile(aes(fill=num_bpc),colour="white")+
    scale_fill_gradient(low="white", high="black")
  return(zone_hm)
}
plot_hm(zone_1[,c(1,5)])+ggtitle("zone_1")

data<-zone_1

library("gridExtra")
grid.arrange(zone_1_hm,zone_2_hm,zone_3_hm,zone_4_hm,
             zone_5_hm,zone_6_hm,zone_8_hm,zone_10_hm,
             zone_11_hm,zone_12_hm,zone_13_hm,ncol=1, nrow=11)
             
par(mfrow=c(1,2))
plot_hm(zone_1[,c(1,5)]) + plot_hm(zone_2[,c(1,5)])



zone_1_hm<-plot_hm(zone_1[,c(1,5)])
zone_2_hm<-plot_hm(zone_2[,c(1,5)])
zone_3_hm<-plot_hm(zone_3[,c(1,5)])
zone_4_hm<-plot_hm(zone_4[,c(1,5)])
zone_5_hm<-plot_hm(zone_5[,c(1,5)])
zone_6_hm<-plot_hm(zone_6[,c(1,5)])
zone_8_hm<-plot_hm(zone_8[,c(1,5)])
zone_10_hm<-plot_hm(zone_10[,c(1,5)])
zone_11_hm<-plot_hm(zone_11[,c(1,5)])
zone_12_hm<-plot_hm(zone_12[,c(1,5)])
zone_13_hm<-plot_hm(zone_13[,c(1,5)])

plot_hm(zone_1[,c(1,5)])
plot_hm(zone_2[,c(1,5)])
plot_hm(zone_3[,c(1,5)])
plot_hm(zone_4[,c(1,5)])
plot_hm(zone_5[,c(1,5)])
plot_hm(zone_6[,c(1,5)])
plot_hm(zone_8[,c(1,5)])
plot_hm(zone_10[,c(1,5)])
plot_hm(zone_11[,c(1,5)])
plot_hm(zone_12[,c(1,5)])
plot_hm(zone_13[,c(1,5)])

#decompose-trend-seasonal-random
library(TTR)

tss<-ts(zone_1[,5],freq=1800,start=1)
a<-stl(tss,"periodic",robust=TRUE)
ts_components<-decompose(tss)
plot(ts_components)

ts_seasonally_adjusted<-tss-ts_components$seasonal
plot(ts_seasonally_adjusted)


#stationary test
print(acf(data$num_bpc, lag.max = 20, main="auto-correlation test(20)"))
print(pacf(data$num_bpc, lag.max = 20, main="partial auto-correlation test"))
print(acf(data$num_bpc, lag.max = 200, main="auto-correlation test(200)"))

library(urca)
summary(ur.kpss(data$num_bpc))

# nonstationary->stationary
library(forecast)
ndiffs(tss)
summary(ur.kpss(diff(tss)))

ts_adj<-diff(tss)
ts_adj %>% ggtsdisplay(main="")


# auto-arima

ts_adj<-diff(tss)
fit<-auto.arima(ts_adj,seasonal = FALSE)
summary(fit)

plot(fit %>% forecast(h=10) %>% autoplot(include=80))

checkresiduals(fit)
# Ljung-Box test: Null hypothesis is that data are all independent, the overall correlation coefficient is 0
# reject H0 means that data are correlated, residuals are not white-noise, this model is flawed

tsdiag(fit)

res<-fit$residuals
res2<-res^2
par(mfrow=c(1,2))
acf(as.vector(res2),main="ACF of Squared Residuals")
pacf(as.vector(res2),main="ACF of Squared Residuals")

#Eagles ARCH test
library(MTS)
archTest(res)

library(rugarch)
spec = ugarchspec()
print(spec)
def.fit=ugarchfit(spec=spec, data=ts_adj)
print(def.fit)

def.fit

plot(def.fit,which=3)

cat("Residuals nomorality test: ","\n","\n")
plot(def.fit,which=8)
plot(def.fit,which=9)
cat("Residuals correlation test: ","\n","\n")
plot(def.fit,which=10)
plot(def.fit,which=11)

a<-predict(def.fit,n.ahead=10)

ugarchforecast(def.fit, n.ahead=7, data=ts_adjust)


#combine num_bpc by day to see daily frequency thoughtout one year
date_1<-unique(zone_1$date)
sum_by_day<-data.frame(date=date_1)
for (i in 1:length(date_1)){
  sum_by_day[i,"n_file"]<-nrow(Bpc_freq[which(zone_1$date==date_1[i]),"num_bpc"]
)
  sum_by_day[i,"num_bpc"]<-sum(Bpc_freq[which(zone_1$date==date_1[i]),"num_bpc"]
  )
}

ggplot(data = sum_by_day, aes(x=date, y=num_bpc)) +
  geom_col(position = 'dodge')





month_1<-unique(month(zone_1$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){
  sum_by_month[i,"zone_1"]<-sum(Bpc_freq[which(month(zone_1$ts)==month_1[i]),"num_bpc"]
  )
}
a1<-sum_by_month

month_1<-unique(month(zone_2$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){
  sum_by_month[i,"zone_2"]<-sum(Bpc_freq[which(month(zone_2$ts)==month_1[i]),"num_bpc"]
  )
}
a2<-sum_by_month


month_1<-unique(month(zone_3$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){
  sum_by_month[i,"zone_3"]<-sum(Bpc_freq[which(month(zone_3$ts)==month_1[i]),"num_bpc"]
  )
}
a3<-sum_by_month

month_1<-unique(month(zone_4$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){
  sum_by_month[i,"zone_4"]<-sum(Bpc_freq[which(month(zone_4$ts)==month_1[i]),"num_bpc"]
  )
}
a4<-sum_by_month

month_1<-unique(month(zone_5$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){
  sum_by_month[i,"zone_5"]<-sum(Bpc_freq[which(month(zone_5$ts)==month_1[i]),"num_bpc"]
  )
}
a5<-sum_by_month

month_1<-unique(month(zone_6$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){

  sum_by_month[i,"zone_6"]<-sum(Bpc_freq[which(month(zone_6$ts)==month_1[i]),"num_bpc"]
  )
}
a6<-sum_by_month

month_1<-unique(month(zone_8$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){

  sum_by_month[i,"zone_8"]<-sum(Bpc_freq[which(month(zone_8$ts)==month_1[i]),"num_bpc"]
  )
}
a8<-sum_by_month

month_1<-unique(month(zone_10$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){

  sum_by_month[i,"zone_10"]<-sum(Bpc_freq[which(month(zone_10$ts)==month_1[i]),"num_bpc"]
  )
}
a10<-sum_by_month

month_1<-unique(month(zone_11$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){

  sum_by_month[i,"zone_11"]<-sum(Bpc_freq[which(month(zone_11$ts)==month_1[i]),"num_bpc"]
  )
}
a11<-sum_by_month

month_1<-unique(month(zone_12$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){

  sum_by_month[i,"zone_12"]<-sum(Bpc_freq[which(month(zone_12$ts)==month_1[i]),"num_bpc"]
  )
}
a12<-sum_by_month

month_1<-unique(month(zone_13$ts))
sum_by_month<-data.frame(month=month_1)
for (i in 1:length(month_1)){

  sum_by_month[i,"zone_13"]<-sum(Bpc_freq[which(month(zone_13$ts)==month_1[i]),"num_bpc"]
  )
}
a13<-sum_by_month
b<-cbind(a1, a2, a4,  a8, a10, a11, a12, a13)[,c(1,2,4,6,8,10,12,14,16)]
c<-merge(b, a3, by = 'month', all=T)
d<-merge(c, a5, by = 'month', all=T)
e<-merge(d, a6, by = 'month', all=T)
e[is.na(e)]=0
plot(e)

library(reshape2)
f<-melt(e, id="month")


mymonths<-c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
month.name<-sort(unique(f$month))
f$month2<-factor(f$month, levels=month.name, labels=mymonths)

ggplot(data=f, aes(x=month2, y=variable))+
  theme_bw(base_family = "STKaiti")+
  geom_tile(aes(fill=value),colour="white")+
  scale_fill_gradient(low="white", high="steelblue")


ggplot(data=f, aes(x=month2, y=value,group=variable, color=variable))+
  geom_point()+
  geom_line()+
  theme_bw()


# framework version


library(readr)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)
library(xts)
library(ggplot2)
Bpc_freq<-read_csv("/project/graziul/data/bpc_freq.csv")
Bpc_freq$ts<-ymd_hm(unlist(Bpc_freq$file))
Bpc_freq<-Bpc_freq[,c('ts','zone','num_bpc')]

# input the zone that want to analyze
MP = function(z, data){
  zone_name <-paste("Zone",z)
  cat("Model and predict data from:",zone_name,"\n")
  zone<-as.data.frame(Bpc_freq[which(Bpc_freq$zone==zone_name),])
  
  # plot time series
  print(plot(xts(zone$num_bpc,order.by=zone$ts),main=zone_name))
  
  # plot ACF, PACF 
  par(mfrow=c(1,2))
  acf(zone$num_bpc, lag.max = 20, main="ACF(20)")
  pacf(zone$num_bpc, lag.max = 20, main="PACF(20)")
  
  # stationary test
  cat("Conduct stationary test:","\n")
  library(urca)
  
  print(summary(ur.kpss(zone$num_bpc)))
  
  teststata = ur.kpss(zone$num_bpc) @teststat
  cval = ur.kpss(zone$num_bpc) @cval[4]
  
  if (teststata > cval) {
    cat("Test statistic is larger than critical value:","\n","\n")
    cat("Timeseries are not stationary","\n","\n")
    cat("Use diff to transfer data into stationary timeseries","\n","\n")
    
    # transfer nonstationary ts to stationary
    tss<-ts(zone$num_bpc,freq=length(zone$num_bpc),start=1)
    library(forecast)
    cat("number of diff:",ndiffs(tss),"\n","\n")
    ts_adj<-diff(tss,diff=ndiffs(tss))
    
    #check again
    
    cat("Conduct Stationary Test:","\n")
    print(summary(ur.kpss(ts_adj)))
    
    teststata2 = ur.kpss(ts_adj) @teststat
    cval2 = ur.kpss(ts_adj) @cval[4]
    
    if(teststata2 > cval2){
      cat("Test statistic is larger than critical value:","\n","\n")
      cat("Adjusted timeseries are not stationary","\n","\n")
      return(message("Adjusted timeseries are not stationary"))
    } else{
      
      cat("T-statistic is smaller than critical value:","\n","\n")
      cat("Adjusted timeseries are stationary","\n","\n")
      cat("Set up ARIMA model","\n","\n")
    }
    
    
  } else {
    cat("T-statistic is smaller than critical value:","\n","\n")
    cat("Timeseries are stationary","\n","\n")
    cat("Set up ARIMA model","\n","\n")
    ts_adj<-(zone$num_bpc)
  }
  
  # ARIMA
  fit<-auto.arima(ts_adj,seasonal = FALSE)
  print(summary(fit))
  
  # Check Residual
  cat("\n","Conduct residuals check:","\n","\n")
  
  print(Box.test(resid(fit),type="Ljung",lag=20))
  
  teststata3 = Box.test(resid(fit),type="Ljung",lag=20)$p.value
  cval3 = 0.05 ##
  
  if(teststata3 < cval3){
    cat("P-value is small:","\n","\n")
    cat("Reject null hypothesis: residuals are independent.","\n","\n")
    cat("So residuals are not white-noise, and this ARIMA model is flawed.","\n","\n")
    cat("Set up ARCH model","\n","\n")
    
    # Engle ARCH LM test
    # Null hypothesis: no ARCH effects
    
    cat("Conduct Arch-LM test:","\n","\n")
    library(FinTS)
    
    print(ArchTest(resid(fit)))
    
    teststata4 = ArchTest(resid(fit))$p.value
    cval4 = 0.05 ##
    
    if(teststata4<cval4){
      cat("P-value is small:","\n","\n")
      cat("Reject null hypothesis: no ARCH effects ","\n","\n")
      cat("ARCH effects exist","\n","\n")
      cat("Set up ARCH/GARCH model","\n","\n")
      library(rugarch)
      spec = ugarchspec()
      def.fit=ugarchfit(spec=spec, data=ts_adj)
      print(def.fit)
      print(plot(fitted(def.fit)))
      
    } else{
      return(message("no suitable model"))
    }
    
    
  } else {
    cat("P-value is large:","\n","\n")
    print("Residuals are white-noise")
    print("This ARIMA model is suitable")
    fit<-auto.arima(ts_adj,seasonal = FALSE)
    plot(fit %>% forecast(h=10) %>% autoplot(include=80))
  }
  
}

MP(13,Bpc_freq)
