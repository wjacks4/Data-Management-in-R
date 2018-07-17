library(xlsx)
library(tidyverse)
library(plyr)

newsnaps<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Snap Data/2012.csv", stringsAsFactors = FALSE)

newsnaps$fullname<-word(string=newsnaps$Player, start=2, end=2, sep="-")

for(i in 1:nrow(newsnaps)){
  namelength<-nchar(newsnaps$fullname[i])
  #print(namelength)
  newsnaps$namelength[i]<-namelength
  
}

for (i in 1:nrow(newsnaps)){
  stophere<-newsnaps$namelength[i]
  nameindex<-newsnaps$fullname[i]
  #print(stophere)
  #print(nameindex)
  newsnaps$lastname[i]<-substr(nameindex, start=3, stop=stophere)
}

newsnaps$Year<-2012

newsnaps$Team<-ifelse(newsnaps$Team=="LARM/STL", "STL", newsnaps$Team)

newsnaps<-subset(newsnaps, select=-c(Player, Off.Snaps, Off.Snap.Pct, Def.Snaps, Def.Snap.Pct, ST.Snaps, ST.Snap.Pct, X, fullname, namelength))
newsnaps$lastname<-gsub("\\.", "", newsnaps$lastname)

write.csv(newsnaps, "C:/Users/whjac/Downloads/Term Paper Data/Snap Data/2012cleaned.csv", row.names=FALSE)