library(xlsx)
library(tidyverse)
library(plyr)

teams<-c("crd", "atl", "rav", "buf", "car", "chi", "cin", "cle", "dal", "den", "det", "gnb", "htx", "clt", "jax", "kan", "mia", "min", "nwe", "nor", "nyg", "nyj",
       "rai", "phi", "pit", "sdg", "sfo", "sea", "ram", "tam", "oti", "was")

teamstest<-c("crd", "atl")

years<-c("2011", "2012", "2013", "2014", "2015", "2016")

weeks<-c(1:15)

injuryaccess<-"C:/Users/whjac/Downloads/Term Paper Data/Injury Data/newdata/teamyear.csv"

combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Injury Data/newdata/atl2011.csv", stringsAsFactors = FALSE)

delete<-nrow(combined)
combined<-combined[-c(1:delete),]
combined<-subset(combined, select=c(1:17))
colnames(combined)[2]<-"Player"
for (week in weeks){
  colnames(combined)[week+2]<-week
}

for (eachyear in years){
  for (eachteam in teams){
    access<-gsub("year", eachyear, injuryaccess)
    access<-gsub("team", eachteam, access)
    access<-read.csv(access, stringsAsFactors = FALSE)
    access<-subset(access, select=c(1:17))
    #print(access[2])
    
    colnames(access)[2]<-"Player"
    
    for (week in weeks){
      colnames(access)[week+2]<-week
    }
    
    dimension<-nrow(access)
    Year<-rep(eachyear, len=dimension)
    Team<-rep(eachteam, len=dimension)
    access<-cbind(access, Year, Team)
    combined<-rbind.fill(combined, access)
  }
}

combined$Team2<-as.character(combined$Team)
combined$Team<-combined$Team2
combined<-subset(combined, select=-Team2)

combined$Year2<-as.numeric(as.character(combined$Year))
combined<-subset(combined, select=-Year)
colnames(combined)[19]<-"Year"

combined$Team<-ifelse(combined$Team=="crd", "ARI", combined$Team)
combined$Team<-ifelse(combined$Team=="rav", "BAL", combined$Team)
combined$Team<-ifelse(combined$Team=="gnb", "GB", combined$Team)
combined$Team<-ifelse(combined$Team=="htx", "HOU", combined$Team)
combined$Team<-ifelse(combined$Team=="clt", "IND", combined$Team)
combined$Team<-ifelse(combined$Team=="kan", "KC", combined$Team)
combined$Team<-ifelse(combined$Team=="nwe", "NE", combined$Team)
combined$Team<-ifelse(combined$Team=="nor", "NO", combined$Team)
combined$Team<-ifelse(combined$Team=="rai", "OAK", combined$Team)
combined$Team<-ifelse(combined$Team=="sdg", "SD", combined$Team)
combined$Team<-ifelse(combined$Team=="sfo", "SF", combined$Team)
combined$Team<-ifelse(combined$Team=="ram", "STL", combined$Team)
combined$Team<-ifelse(combined$Team=="tam", "TB", combined$Team)
combined$Team<-ifelse(combined$Team=="oti", "TEN", combined$Team)

combined$Team<-toupper(combined$Team)
combined<-subset(combined, select=-1)

reformat<-as.data.frame(combined$Player)
reformat$bla<-"bla"
colnames(reformat)[1]<-"Player"
delete<-(nrow(reformat)-1)
reformat<-reformat[-c(1:delete),]
reformat<-subset(reformat, select=-c(bla))

testcombo<-combined[c(1:10),]

injuryagg<-as.data.frame(combined[1,2])
injuryagg$Injury.listing<-" "
injuryagg$Week<-" "
injuryagg$Player<-" "
injuryagg$Year<- " "
injuryagg$Team<- " "
colnames(injuryagg)[1]<-"delete"
injuryagg<-injuryagg[-c(1),]

for (i in (1:nrow(combined))){
  for (c in (2:16)){
    index<-(c-1)
    outputindex<-(((15*(i-1))+1)+index)
    players<-combined$Player[i]
    years<-combined$Year[i]
    teams<-combined$Team[i]
    injurylisting<-combined[i,c]
    #print(injurylisting)
    injuryagg[(outputindex),2]<-injurylisting
    injuryagg[(outputindex),3]<-index
    injuryagg[(outputindex),4]<-players
    injuryagg[(outputindex),5]<-years
    injuryagg[(outputindex),6]<-teams
  }
}

injuryagg<-injuryagg[injuryagg$Injury.listing!="",]

injurytypes<-unique(injuryaggtest$Injury.listing)

injuryagg$Injury.listing<-ifelse(injuryagg$Injury.listing=="Q", "Questionable", injuryagg$Injury.listing)
injuryagg$Injury.listing<-ifelse(injuryagg$Injury.listing=="P", "Probable", injuryagg$Injury.listing)
injuryagg$Injury.listing<-ifelse(injuryagg$Injury.listing=="O", "Out", injuryagg$Injury.listing)
injuryagg$Injury.listing<-ifelse(injuryagg$Injury.listing=="IR", "Injured Reserve", injuryagg$Injury.listing)
injuryagg$Injury.listing<-ifelse(injuryagg$Injury.listing=="PUP", "Physically Unable to Play", injuryagg$Injury.listing)

injuryagg<-subset(injuryagg, select=-c(delete))
injuryagg<-injuryagg[-1,]

write.csv(injuryagg, "C:/Users/whjac/Downloads/Term Paper Data/Injury Data/FINAL FULL.csv", row.names=FALSE)
