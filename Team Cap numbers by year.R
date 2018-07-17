library(xlsx)
library(tidyverse)

caps<-(c(123000000,133000000,143280000,155270000))
years<-(c(2,3,4))

yearcombined<-read.xlsx("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Cap Stats by year.xlsx", 1)
newyears<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/NewCapStats.csv")

for (year in years){
  yearly<- read.xlsx("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Cap Stats by year.xlsx", year)
  yearcombined<-rbind.fill(yearcombined, yearly)
}

totalcombined<-rbind(newyears, yearcombined)

stldrop<-which(totalcombined$Team=="los angeles rams" & (totalcombined$Year=="2011" || totalcombined$Year=="2012" || totalcombined$Year=="2013" || totalcombined$Year=="2014" || totalcombined$Year=="2015"))

totalcombined<-totalcombined[-c(stldrop),]

write.csv(totalcombined, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CleanedCapStats.csv", row.names=FALSE)

#capstats<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CleanedCapStats.csv")