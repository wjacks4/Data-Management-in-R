library(xlsx)
library(tidyverse)
library(plyr)

teams<-c("crd", "atl", "rav", "buf", "car", "chi", "cin", "cle", "dal", "den", "det", "gnb", "htx", "clt", "jax", "kan", "mia", "min", "nwe", "nor", "nyg", "nyj",
         "rai", "phi", "pit", "sdg", "sfo", "sea", "ram", "tam", "oti", "was")

teamstest<-c("crd", "atl")

years<-c("2011", "2012", "2013", "2014", "2015", "2016")

weeks<-c(1:15)

expaccess<-"C:/Users/whjac/Downloads/Term Paper Data/ExperienceData/teamyear.csv"

combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/ExperienceData/atl2011.csv", stringsAsFactors = FALSE)
combined$Year<-"2011"
combined$Team<-"atl"
colnames(combined)[2]<-"Position"
colnames(combined)[3]<-"Player"

for (eachyear in years){
  for (eachteam in teams){
    access<-gsub("year", eachyear, expaccess)
    access<-gsub("team", eachteam, access)
    access<-read.csv(access, stringsAsFactors = FALSE)
    #print(access[2])
    
    colnames(access)[2]<-"Position"
    colnames(access)[3]<-"Player"
    
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
combined$Year<-combined$Year2
combined<-subset(combined, select=-Year2)


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

combined<-subset(combined, !is.na(combined$Age))

#CHANGE POSITIONS#
qboutputcap<-subset(combined, combined$Position=="QB")
qboutputcap$FranchisePosition<-"QB"
combined<-subset(combined, Position!="QB")

dtoutputcap<-subset(combined, Position=="DT" | Position=="NT" | Position=="LDT" | Position=="RDT")
dtoutputcap$FranchisePosition<-"DT"
combined<-subset(combined, Position!="DT" & Position!="NT" & Position!="LDT" & Position!="RDT")

deoutputcap<-subset(combined, Position=="DE" | Position=="DL")
deoutputcap$FranchisePosition<-"DE"
combined<-subset(combined, Position!="DE" & Position!="DL" )

soutputcap<-subset(combined, Position=="FS" | Position=="SS" | Position=="S")
soutputcap$FranchisePosition<-"S"
combined<-subset(combined, Position!="FS" & Position!="SS" & Position!="S")

cboutputcap<-subset(combined, Position=="CB"| Position=="RCB" | Position=="LCB" | Position=="DB")
cboutputcap$FranchisePosition<-"CB"
combined<-subset(combined, Position!="CB" & Position!="RCB" & Position!="LCB" & Position!="DB")

lboutputcap<-subset(combined, Position=="LB" | Position=="ILB"| Position=="OLB" | Position=="LOLB" | Position=="ROLB" | Position=="MLB" | Position=="RILB" | Position=="LILB")
lboutputcap$FranchisePosition<-"LB"
combined<-subset(combined, Position!="LB" & Position!="ILB" & Position!="OLB" & Position!="LOLB" & Position!="ROLB" & Position!="MLB" & Position!="RILB" & Position!="LILB")

wroutputcap<-subset(combined, Position=="WR")
wroutputcap$FranchisePosition<-"WR"
combined<-subset(combined, Position!="WR")

rboutputcap<-subset(combined, Position=="RB" | Position=="FB")
rboutputcap$FranchisePosition<-"RB"
combined<-subset(combined, Position!="RB" & Position!="FB")

teoutputcap<-subset(combined, Position=="TE")
teoutputcap$FranchisePosition<-"TE"
combined<-subset(combined, Position!="TE")

stoutputcap<-subset(combined, Position=="P" | Position=="K")
stoutputcap$FranchisePosition<-"ST"
combined<-subset(combined, Position!="P" & Position!="K")

oloutputcap<-subset(combined, Position=="G" | Position=="C" | Position=="RT" | Position=="LT" | Position=="T" | Position=="LG" | Position=="RG")
oloutputcap$FranchisePosition<-"OL"
combined<-subset(combined, Position!="G" & Position!="C" & Position!="RT" & Position!="LT" & Position!="T" & Position!="LG" & Position!="RG")

combined<-rbind(qboutputcap, dtoutputcap, deoutputcap, cboutputcap, soutputcap, lboutputcap, wroutputcap, rboutputcap, teoutputcap, stoutputcap, oloutputcap)

colnames(combined)[5]<-"Experience2"
combined$Experience2<-ifelse(combined$Experience2=="Rook", "0", combined$Experience2)
combined$Experience<-as.numeric(combined$Experience2)

combined<-subset(combined, select=c(Player, Age, Experience, Team, FranchisePosition, Year))



write.csv(combined, "C:/Users/whjac/Downloads/Term Paper Data/ExperienceData/Combined/Combined.csv", row.names = FALSE)
