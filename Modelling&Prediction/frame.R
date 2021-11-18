library(readr)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)
library(xts)
library(ggplot2)

Bpc_freq<-read_csv("/project/graziul/data/bpc_freq.csv")
Bpc_freq$ts<-ymd_hm(unlist(Bpc_freq$file))
Bpc_freq<-Bpc_freq[,c('ts','zone','num_bpc')]

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
      cat("Residuals nomorality test: ","\n","\n")
      plot(def.fit,which=8)
      plot(def.fit,which=9)
      cat("Residuals correlation test: ","\n","\n")
      plot(def.fit,which=11)
      cat("Forecast: ","\n","\n")       
      ugarchforecast(def.fit, n.ahead=7, data=ts_adjust)
      
      
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

sum_by_day = function(zone){
  date_1<-unique(as.Date(zone$ts))
  sum_by_day<-data.frame(date=date_1)
  for (i in 1:length(date_1)){
    sum_by_day[i,"num_bpc"]<-sum(Bpc_freq[which(as.Date(zone$ts)==date_1[i]),"num_bpc"]
    )
  }
  zone<-sum_by_day
}


extract_zone = function(z, data){
  zone_name <-paste("Zone",z)
  cat("Model and predict data from:",zone_name,"\n")
  zone<-as.data.frame(Bpc_freq[which(Bpc_freq$zone==zone_name),])
}

zone_1<-extract_zone(1, Bpc_freq)

zone_1_day<-sum_by_day(zone_1)


MP = function(zone){
  # plot time series
  ggplot(data = zone, aes(x=date, y=num_bpc)) +  
    geom_line()+
    ggtitle("Zone_1")
  
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
      cat("Residuals nomorality test: ","\n","\n")
      plot(def.fit,which=8)
      plot(def.fit,which=9)
      cat("Residuals correlation test: ","\n","\n")
      plot(def.fit,which=11)
      cat("Forecast: ","\n","\n")       
      ugarchforecast(def.fit, n.ahead=7, data=ts_adjust)
      
      
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

MP(zone_1_day)

MP2 = function(z, data){
  zone_name <-paste("Zone",z)
  cat("Model and predict data from:",zone_name,"\n")
  zone<-as.data.frame(Bpc_freq[which(Bpc_freq$zone==zone_name),])
  
  #sum by day
  date_1<-unique(as.Date(zone$ts))
  sum_by_day<-data.frame(date=date_1)
  for (i in 1:length(date_1)){
    sum_by_day[i,"num_bpc"]<-sum(Bpc_freq[which(as.Date(zone$ts)==date_1[i]),"num_bpc"]
    )
  }
  zone<-sum_by_day
  
  zone_1_day<-sum_by_day
  zone_1_day$date<-as.Date(ymd(zone_1_day$date))
  
  # plot time series
  ggplot(data = zone, aes(x=date, y=num_bpc)) +  
    geom_line()+
    ggtitle("Zone_1")
  
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
      cat("Residuals nomorality test: ","\n","\n")
      plot(def.fit,which=8)
      plot(def.fit,which=9)
      cat("Residuals correlation test: ","\n","\n")
      plot(def.fit,which=11)
      cat("Forecast: ","\n","\n")       
      ugarchforecast(def.fit, n.ahead=7, data=ts_adjust)
      
      
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
