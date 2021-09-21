

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
z = 1

MP = function(z, data){
  zone_name <-paste("Zone",z)
  cat("Model and predict data from:",zone_name,"\n")
  zone<-as.data.frame(Bpc_freq[which(Bpc_freq$zone==zone_name),])
  
  # plot time series
  plot(xts(zone$num_bpc,order.by=zone$ts),main=zone_name)
  
  # plot ACF, PACF 
  par(mfrow=c(1,2))
  acf(zone$num_bpc, lag.max = 20, main="ACF(20)")
  pacf(zone$num_bpc, lag.max = 20, main="PACF(20")
  
  # stationary test
  print("Conduct stationary test:")
  library(urca)
  
  summary(ur.kpss(diff(tss)))
  
  teststata = ur.kpss(zone$num_bpc) @teststat
  cval = ur.kpss(zone$num_bpc) @cval[4]
  
  if (teststata > cval) {
    print("Timeseries are not stationary")
    
    # transfer nonstationary ts to stationary
    tss<-ts(zone$num_bpc,freq=length(zone$num_bpc),start=1)
    library(forecast)
    cat("number of diff:",ndiffs(tss),"\n")
    ts_adj<-diff(tss,diff=ndiffs(tss))
    
    #check again
    teststata2 = ur.kpss(ts_adj) @teststat
    cval2 = ur.kpss(ts_adj) @cval[4]
    
    if(teststata2 > cval2){
      print("Adjusted timeseries are not stationary")
    } else{
      print("Adjusted timeseries are stationary")
      print("Set up ARIMA model")
    }
    
    
  } else {
    print("Timeseries are stationary")
    print("Set up ARIMA model")
    ts_adj<-(zone$num_bpc)
  }
  
  # ARIMA
  fit<-auto.arima(ts_adj,seasonal = FALSE)
  summary(fit)
  
  # Check Residual
  print("Conduct residuals check:")
  teststata2 = Box.test(resid(fit),type="Ljung",lag=20)$p.value
  cval2 = 0.05 ##
  
  if(teststata2 < cval2){
    print("Reject null hypothesis: residuals are independent.")
    print("So residuals are not white-noise, and this ARIMA model is flawed.")
    print("Set up ARCH model")
    
    # Engle ARCH LM test
    # Null hypothesis: no ARCH effects
    
    print("Conduct Arch-LM test:")
    library(FinTS)
    teststata3 = ArchTest(resid(fit))$p.value
    cval3 = 0.05 ##
    
    if(teststata3<cval3){
      print("ARCH effects exist")
      print("Set up ARCH/GARCH model")
      library(rugarch)
      spec = ugarchspec()
      def.fit=ugarchfit(spec=spec, data=ts_adj)
      return(def.fit)
    } else{
      return(message("no suitable model"))
    }
    
    
  } else {
    print("Residuals are white-noise")
    print("This ARIMA model is suitable")
    fit<-auto.arima(ts_adj,seasonal = FALSE)
    plot(fit %>% forecast(h=10) %>% autoplot(include=80))
  }
  
}

