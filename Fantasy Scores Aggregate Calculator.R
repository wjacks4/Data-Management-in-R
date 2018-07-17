library(xlsx)
library(tidyverse)
library(plyr)
library(ggplot2)
library(reshape)
library(xts)

##IMPORT DATA SETS##
fulldatav2<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/fullcombinedPLUS.csv", stringsAsFactors=FALSE)

fulldatav2$Player<-ifelse(fulldatav2$Player=="Brandon Marshall" & fulldatav2$Team=="DEN" & fulldatav2$Year>=2012, "Brandon Marshall LB", fulldatav2$Player)

Fantasyagg<-subset(fulldatav2, select=c(Player, seasontotal, Points.For, W, Team, Year))
Fantasyagg<-subset(Fantasyagg, !is.na(Fantasyagg$seasontotal))
Fantasyagg<-unique(Fantasyagg)

Output<-subset(Fantasyagg, select=c(Team, Year, W))
Output<-unique(Output)

Aggpoints<-0
Outputindex<-1
for (i in 1:nrow(Fantasyagg)){
  year<-Fantasyagg$Year[i]
  points<-Fantasyagg$seasontotal[i]
  nextyear<-(Fantasyagg$Year[i+1])
  team<-Fantasyagg$Team[i]
  if (year==nextyear && i<=(nrow(Fantasyagg)-1)){
    Aggpoints<-points+Aggpoints
    Fantasyagg$Aggpoints[i]<-Aggpoints
  } else if (year!=nextyear && i<=(nrow(Fantasyagg))-1){
    Aggpoints<-points+Aggpoints
    Fantasyagg$Aggpoints[i]<-Aggpoints
    Output$TotalScore[Outputindex]<-Aggpoints
    Aggpoints<-0
    Outputindex<-Outputindex+1
  } else {
    Aggpoints<-points+Aggpoints
    Fantasyagg$Aggpoints[i]<-Aggpoints
    Output$TotalScore[Outputindex]<-Aggpoints
  }
}

Output$Yearz<-as.factor(Output$Year)
Output<-subset(Output, select=-c(Year, W))
colnames(Output)[3]<-"Year"




write.csv(Output, "C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Team Aggregate Scores/Fantasy Aggregate Scores.csv", row.names=FALSE)
