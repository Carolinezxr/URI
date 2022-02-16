library(readr)
library(sf)           # Objects and functions for geospatial data
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(ggplot2)      # Graphing functions

setwd("E:/data/census")

demographic<-read.csv("DP05_zone_demographic.csv")
economics<-read.csv("DP03_zone_economics.csv")
housing<-read.csv("DP04_zone_housing.csv")
social<-read.csv("DP02_zone_social.csv")

# crime report in Aug
data<-read.csv("E:\\data\\crimes_data_zone_simplified_summary_day.csv")

data$day<-as.Date(data$day,"%Y-%m-%d")

Violent<-subset(data, (day>="2018-08-01"& day<="2018-08-31"))[,c('day','Zone','Violent')]

Violent<-aggregate(Violent$Violent, list(Violent$Zone), mean)
colnames(Violent)<-c("Zone","Violent")

Property<-subset(data, (day>="2018-08-01"& day<="2018-08-31"))[,c('day','Zone','Property')]

Property<-aggregate(Property$Property, list(Property$Zone), mean)
colnames(Property)<-c("Zone","Property")

census<-as.data.frame(cbind(demographic$Zone,demographic$Total.population))
colnames(census)<-c("Zone","Population")

census<-merge(census,Violent)
census<-merge(census,Property)

census$ViolentCapta<-census$Violent/census$Population
census$PropertyCapta<-census$Property/census$Population

## instability
# Percent of residents who moved into the block in the past five years (higher ??? more instable) 
move5y<-housing$Moved.in.2017.or.later
occupiedunit<-housing$Occupied.housing.units
move5yPct<-move5y/occupiedunit
census$move5yPct<-move5yPct
# Percent of renter (higher ??? more instable) 
renter<-housing$Renteroccupied
renterPct<-renter/occupiedunit
census$renterPct<-renterPct

## Disadvantage, poverty
# Percent residents receiving public assistance 
assis<-economics$With.cash.public.assistance.income
household<-economics$Total.households
assisPct<-assis/household
census$assisPct<-assisPct
# Median household income 
MedIncome<-economics$Median.household.income.dollars
census$MedIncome<-MedIncome

## Minority
# Percent non-white residents 
white<-demographic$White
nonwhite<-demographic$Total.population-demographic$White
nonwhitePct<-nonwhite/demographic$Total.population
census$nonwhitePct<-nonwhitePct


## Education

bachelor<-social$Bachelor.s.degree.or.higher
bachelorPct<-bachelor/census$Population
census$bachelorPct<-bachelorPct


#write.csv(census,"selected_census.csv")


corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  #require(Hmisc)
  library(rstatix)
  R <- cor_mat(x) # Matrix of correlation coefficients
  p <- cor_get_pval(R) # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  mystars<-mystars[,-1]
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R)[,-2], 2))[,-1]
  
  ## build a new matrix that includes the correlations with their appropriate stars
  Rnew<-R
  for (i in 1:nrow(R)){
    Rnew[i,]<-paste(R[i,], mystars[i,], sep="")
  }
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  rownames(Rnew) <- colnames(x)
  return(Rnew)
}


a<-corstars(census[,5:12],removeTriangle="lower")
