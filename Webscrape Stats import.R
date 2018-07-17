library(xlsx)
library(tidyverse)
library(plyr)

##############################################IMPORT FANTASY DATA#####################################################################

positions=c("qb", "rb", "wr", "te", "k", "dl", "lb", "db", "k")

years<-c("2011", "2012", "2013", "2014", "2015", "2016")

index<-c(1:15)

statsaccess<-"C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/position-year-weekindex.csv"

statscombined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/qb-2011-week1.csv", stringsAsFactors = FALSE)

for (year in years){
  for (position in positions){
    for(i in index){
      access<-gsub("position", position, statsaccess)
      access<-gsub("year", year, access)
      access<-gsub("index", i, access)
      
      access<-read.csv(access, stringsAsFactors = FALSE)
      access$Week<-i
      access$Position<-position
      access$Year<-year
      
      statscombined<-rbind.fill(statscombined, access)
    }
  }
}

statscombined<-subset(statscombined, !is.na(statscombined$Week))
statscombined<-subset(statscombined, statscombined$Team!="")

statscombined<-subset(statscombined, select=-c(X, Games, Rank, Avg))

write.csv(statscombined, "C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/Combined/FULLCOMBINED.csv", row.names = FALSE)