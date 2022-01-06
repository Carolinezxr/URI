# desciptive statistics

library(readr)
library(stargazer)


features<-read.csv('/project/graziul/ra/team_ser/GeMAPS_0105.csv')
features<-features[,-1]

HP<-subset(features,(Arousal=='High')&(Valence=='Positive'))
LP<-subset(features,(Arousal=='Low')&(Valence=='Positive'))
HN<-subset(features,(Arousal=='High')&(Valence=='Negative'))
LN<-subset(features,(Arousal=='Low')&(Valence=='Negative'))

stargazer(HP,
          type = 'text', 
          digits=2, align=T,
          title = paste("Summary Statistics: Hight Arousal & Positive Valence"))

stargazer(LP,
          type = 'text', 
          digits=2, align=T,
          title = paste("Summary Statistics: Low Arousal & Positive Valence"))

stargazer(HN,
          type = 'text', 
          digits=2, align=T,
          title = paste("Summary Statistics: Hight Arousal & Negative Valence"))

stargazer(LN,
          type = 'text', 
          digits=2, align=T,
          title = paste("Summary Statistics: Low Arousal & Negative Valence"))

