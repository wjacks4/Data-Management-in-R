library(xlsx)
library(tidyverse)
library(plyr)

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LAR", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")


teams<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/BASIC DATA/Team List.csv")
profiles<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Kernel Data/playerprofiles.csv")


#BASIC CLEANING
profiles$Player<-profiles$name
profiles<-subset(profiles, select=-name)

profiles$draft_team<-as.character(profiles$draft_team)

teams2<-profiles$draft_team
teams2<-as.data.frame(teams2)
teams2<-distinct(teams2)
teams2<-sort(teams2$teams2, decreasing = FALSE)
teams2<-as.data.frame(teams2)

teams2$Team<-teams2$teams2
teams2<-subset(teams2, select=-teams2)

teams<-merge(teams, teams2, by="Team")
teams$Team<-as.character(teams$Team)


#TEAM ABBREVIATIONS
for (eachteam in teams$Team){
  for (eachobs in profiles$draft_team){
    if (eachobs==eachteam){
      shrtid<-which((teams$Team)==eachteam)
      obserid<-which((profiles$draft_team)==eachteam)
      profiles$abbrev<-teamabbrev[shrtid]
    }
  }
}

for (eachteam in teams$Team){
  for (eachobs in profiles$draft_team){
    if (eachobs==eachteam){
      shrtid<-which((teams$Team)==eachteam)
      obserid<-which((profiles$draft_team)==eachteam)
      profiles$abbrev[obserid]<-teamabbrev[shrtid]
    } 
  }
}

for (eachobs in profiles$draft_team){
  if (eachobs == ""){
    obserid<-which((profiles$draft_team)=="")
    profiles$abbrev[obserid]<-("")
  }
}

profiles$draft_team<-profiles$abbrev
profiles<-subset(profiles, select=-abbrev)

write.csv(profiles, "C:/Users/whjac/Downloads/Term Paper Data/Kernel Data/profilescleaned.csv")

