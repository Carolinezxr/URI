#install.packages("seewave", repos="http://cran.at.r-project.org/")
library(seewave)
library(tuneR)


library(monitoR)



library(ggplot2)

raw<-readMP3("/home/xuranzeng/SER/201808040636-306923-27730.mp3")
#writeWave(raw,"tmp.wav",extensible=FALSE)
#wav<-readWave("/home/xuranzeng/SER/tmp.wav")

duration(raw,f)

length(raw@left)

data<-raw@left

f<-raw@samp.rate

plot(acoustat(data,f))

a<-cutw(raw,f,from=1501,to=1501.4)

#writeWave(tico,"/home/xuranzeng/SER/tmp.mp3",extensible=FALSE)

timeArray <- (0:(nrow(a)-1)) / f
timeArray <- timeArray * 1000 #scale to milliseconds

plot(timeArray, a, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')

library(dplyr)
library(reshape)

ss <- spec(data,f,plot=TRUE,fastdisp=TRUE)
# Pull all three measurements from spectro graph
# melt converts wide format into long format

# For amplitude, replace Var1 and Var 2 with new variables
amp = melt(ss$amp, value.name = "Amplitude") %>% 
  select(FrequencyIndex = Var1, TimeIndex = Var2, Amplitude)

# For frequency, add matching row num and changing value of frequency
# Frequency value from kHz to Hz
frequent = melt(ss$freq, value.name = "Frequency") %>% 
  mutate(FrequencyIndex = row_number(), Frequency = Frequency * 1000)

# For time, add a TimeIndex for left join later on
tm = melt(ss$time, value.name = "Time") %>% 
  mutate(TimeIndex = row_number())

dur<-0.5

# left join all datasets together 
# Only need Time, Frequency, and Amplitude
numeric_data <- amp %>% 
  left_join(frequent, by = "FrequencyIndex") %>% 
  left_join(tm, by = "TimeIndex") %>% 
  select(FrequencyIndex,Time, Frequency, Amplitude) %>% 
  filter(Time >= 1) %>% # shed first second of data
  filter(Time <= (dur - 1)) # shed last second of data

View(numeric_data, title="Numeric Data")

# TEO
teo<-TKEO(a,f)
teo<-na.omit(teo)
#ggplot(as.data.frame(teo),aes(x=time,y=V2))+geom_line()


# TEO autoccorrelation
autocor<-acf(teo[,2],lag.max = nrow(teo))$acf
c<-teo[,1]
c<-cbind(c,autocor)

ggplot(as.data.frame(c),aes(x=c,y=autocor))+
  geom_line()
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 13), se = FALSE)

# Envelop of TEO autocorrelation
## find local maxima
c<-as.data.frame(c)
TUX<-c(1,which(diff(sign(diff(c[,2])))==-2)+1)
TUY<-c[TUX,2]

plot(c[,1], c[,2], type='l')
lines(c[TUX,1],c[TUX,2],  col='red')

plot(c[TUX,1],c[TUX,2], type='l')

library(zoo)

x <- c[TUX,1]
y <- c[TUX,2]
id <- order(x)

AUC <- sum(diff(x[id])*rollmean(y[id],2))
AUC

