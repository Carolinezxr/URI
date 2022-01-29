library(readr)
library(stargazer)
library(zoo)
library(vars)
dzone1<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone1.csv")
dzone2<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone2.csv")
dzone3<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone3.csv")
dzone4<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone4.csv")
dzone5<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone5.csv")
dzone6<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone6.csv")
dzone7<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone7.csv")
dzone8<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone8.csv")
dzone9<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone9.csv")
dzone10<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone10.csv")
dzone11<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone11.csv")
dzone12<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone12.csv")
dzone13<-read.csv("/project/graziul/ra/xuranzeng/request_report/dzone13.csv")

zone1<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone1.csv")
zone2<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone2.csv")
zone3<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone3.csv")
zone4<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone4.csv")
zone5<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone5.csv")
zone6<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone6.csv")
zone7<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone7.csv")
zone8<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone8.csv")
zone9<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone9.csv")
zone10<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone10.csv")
zone11<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone11.csv")
zone12<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone12.csv")
zone13<-read.csv("/project/graziul/ra/xuranzeng/request_report/zone13.csv")

# full sample GC
type<-colnames(dzone11)[-c(1,2)]
GC <- data.frame(matrix(nrow = length(type), ncol = 13))
rownames(GC)<-type
colnames(GC)<-1:13

data<-dzone7
df<-na.locf(data[ , colSums(is.na(data)) < 0.9*nrow(data)])
df1 <- df[colSums(df == 0, na.rm=TRUE) < 0.8*nrow(df)]
NAMES = colnames(df1)
k = ncol(df1)
for (i in c(3:(k-2),k)){
  cause = NAMES[i]
  GC[cause,7]<-(round((grangertest(Violent ~ df1[,cause], order = 2, data = df1)$Pr[2]),2))
}

c<-GC[rowSums(is.na(GC)) != ncol(GC), ]

# 2020-07-01 to 2021-07-01

start_date<-"2020-07-01"
end_date<-"2021-07-01"

type<-colnames(dzone11)[-c(1,2)]
GC <- data.frame(matrix(nrow = length(type), ncol = 13))
rownames(GC)<-type
colnames(GC)<-1:13

data<-dzone13
data$day<-as.Date(data$day)
df0<-data[data$day >= start_date & data$day <= end_date, ]
df<-na.locf(data[ , colSums(is.na(df0)) < 0.9*nrow(df0)])
df1 <- df[colSums(df == 0, na.rm=TRUE) < 0.8*nrow(df)]
NAMES = colnames(df1)
k = ncol(df1)
for (i in c(3:(k-2),k)){
  cause = NAMES[i]
  GC[cause,13]<-(round((grangertest(Violent ~ df1[,cause], order = 2, data = df1)$Pr[2]),2))
}

c<-GC[rowSums(is.na(GC)) != ncol(GC), ]

# 2019-07-01 to 2020-07-01

start_date<-"2019-07-01"
end_date<-"2020-07-01"

type<-colnames(dzone11)[-c(1,2)]
GC <- data.frame(matrix(nrow = length(type), ncol = 13))
rownames(GC)<-type
colnames(GC)<-1:13

data<-dzone13
data$day<-as.Date(data$day)
df0<-data[data$day >= start_date & data$day <= end_date, ]
df<-na.locf(data[ , colSums(is.na(df0)) < 0.9*nrow(df0)])
df1 <- df[colSums(df == 0, na.rm=TRUE) < 0.8*nrow(df)]
NAMES = colnames(df1)
k = ncol(df1)
for (i in c(3:(k-2),k)){
  cause = NAMES[i]
  GC[cause,13]<-(round((grangertest(Violent ~ df1[,cause], order = 2, data = df1)$Pr[2]),2))
}

c<-GC[rowSums(is.na(GC)) != ncol(GC), ]




