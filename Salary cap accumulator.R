library(xlsx)
library(tidyverse)

##BUILD BASE DATA SET
basedata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CLEANEDPunterEarnings.csv",1)

##OMIT MISSING ENTRIES
newdatavars<-c("Name","SALARY","TEAM","Position", "YEAR")
newdata<-basedata[newdatavars]
newdata<-na.omit(newdata)

##CHANGE VARIABLE TYPES AS APPROPRIATE
newdata$SALARY<-as.numeric(as.character(newdata$SALARY))
newdata$YEAR<-as.numeric(as.character(newdata$YEAR))
newdata$Name<-as.character(newdata$Name)
newdata$TEAM<-as.character(newdata$TEAM)
newdata$Position<-as.character(newdata$Position)

#BUILD CAP HIT MATRIX
capdata<-matrix(data=NA, nrow=(nrow(newdata)), ncol=4)
capdata<-as.data.frame(capdata)
colnames(capdata)<-c("team","year","pos","salcaphit")

#CHANGE VARIABLE TYPES IN CAPHIT MATRIC AS APPROPRIATE
capdata$team<-as.character(capdata$team)
capdata$year<-as.numeric(as.character(capdata$year))
capdata$salcaphit<-as.numeric(as.character(capdata$salcaphit))
capdata$pos<-as.character(capdata$pos)

##ACCUMULATE SALARY BY TEAM BY YEAR
agg=0
x=0
for(yr in 2007:2016){
  for(i in 2:dim(capdata)){
    if (newdata$YEAR[i]==yr){
      x=0
      agg=0
      while (newdata$YEAR[(i+x)]==yr){
        agg=(agg+newdata$SALARY[(i+x)])
        x=(x+1)
      }
        if (newdata$YEAR[i]!=newdata$YEAR[i-1]){
          capdata$salcaphit[i]<-agg
          capdata$team[i]<-newdata$TEAM[i]
          capdata$year[i]<-newdata$YEAR[i]
          capdata$pos[i]<-newdata$Position[i]
        }
    }
  }  
}

##CONSOLIDATE DATA
capdata<-na.omit(capdata)


#FIND AVERAGE SALARY CAP HIT PER YEAR
capdata<-capdata[order(capdata$year,capdata$team),]
capdata$avgsal<-NA
capdata$avgsal<-as.numeric(as.character(capdata$avgsal))
capdata

n=0
sumslry=0
for(yr in 2007:2016){
  for(i in 1:dim(capdata)){
    if (capdata$year[i]==yr){
      sumslry=(sumslry+capdata$salcaphit[i])
      avgsal=(sumslry/(n+1))
      if (capdata$year[i+1]!=capdata$year[i] || i==dim(capdata)){
        capdata$avgsal[i]<-avgsal
      }
      n=(n+1)
    }
    else if (capdata$year[i]!=yr){
      sumslry=0
      n=0
    }
  }
}

avgsalcaphit<-na.omit(capdata)
avgsalcaphit<-as.data.frame(avgsalcaphit)
keeps<-c("year","avgsal","pos")
avgsalcaphit<-avgsalcaphit[keeps]

#EXPORT
write.csv(avgsalcaphit, file="C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Average Salary Cap hits by Position/Salary/Punters.csv")

