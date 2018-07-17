library(xlsx)
library(tidyverse)
library(plyr)

totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUSPLUS.csv", stringsAsFactors = FALSE)
teamcapscombined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CleanedCapStatsPLUS.csv", stringsAsFactors = FALSE)

#CHANGE TEAMS TO TEAM ABBREVIATIONS
teams<-c("arizona-cardinals","atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns",
         "dallas-cowboys", "denver-broncos", "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars", 
         "kansas-city-chiefs", "los-angeles-rams", "miami-dolphins", "minnesota-vikings", "new-england-patriots", "new-orleans-saints",
         "new-york-giants", "new-york-jets", "oakland-raiders", "philadelphia-eagles", "pittsburgh-steelers", "san-diego-chargers", "san-francisco-49ers", "seattle-seahawks",
         "tampa-bay-buccaneers", "tennessee-titans", "washington-redskins")

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "TB", "TEN", "WAS")

years<-c(2011, 2012, 2013,2014,2015,2016)

totalcaphits$Team<-toupper(totalcaphits$Team)

##CHANGE FULL TEAM NAMES TO ABBREVIATIONS

#TEAM CAP DATA#
teamcapscombined$Team<-gsub("Los Angeles Rams", "St. Louis Rams", teamcapscombined$Team)
teamcapscombined$Team<-tolower(teamcapscombined$Team)
teams2<-teamcapscombined$Team[1:32]

for (eachteam in teams2){
  for (eachobs in teamcapscombined$Team){
    if (eachobs==eachteam){
      shrtid<-which((teams2)==eachteam)
      obserid<-which((teamcapscombined$Team)==eachteam)
      teamcapscombined$abbrev<-teamabbrev[shrtid]
    }
  }
}

for (eachteam in teams2){
  for (eachobs in teamcapscombined$Team){
    if (eachobs==eachteam){
      shrtid<-which((teams2)==eachteam)
      obserid<-which((teamcapscombined$Team)==eachteam)
      teamcapscombined$abbrev[obserid]<-teamabbrev[shrtid]
    }
  }
}

teamcapscombined$Team<-teamcapscombined$abbrev
teamcapscombined<-subset(teamcapscombined, select=-abbrev)


#INITIAL MERGE#
combo<-merge(totalcaphits, teamcapscombined, by=c("Team", "Year"), all.x=TRUE)

#INITIAL CLEANING#
combo$Cap.Hit<-as.numeric(combo$Cap.Hit)

combo$percentage<-(combo$Cap.Hit/combo$Adjusted.cap)
combo<-unique(combo)
combo$samplecaps<-''

#REMOVE MISSING CAP OBSERVATIONS TO BUILD AGGREGATE#
combo<-subset(combo, !is.na(combo$Cap.Hit))

deadcombo<-subset(combo, combo$Dead.Cap!="-")

#BUILD AGGREGATE DEADCAP INFORMATION#
deadcombo$deadagg<-NA
deadcombo$deadagg<-as.numeric(deadcombo$deadagg)
deadagg=0
x=0
for(eachteam in teamabbrev){
  for(eachyear in years){
    for(i in 1:nrow(deadcombo)){
      if(deadcombo$Team[i]==eachteam && deadcombo$Year[i]==eachyear){
        deadcombo$percentage[i]<-deadcombo$Cap.Hit[i]/deadcombo$Adjusted.cap
        deadcombo$deadagg[i]<-deadcombo$Cap.Hit[i]+deadagg
        deadcombo$aggpercentage[i]<-(deadcombo$deadagg[i]/deadcombo$Adjusted.cap[i])
        deadagg=(deadcombo$Cap.Hit[i]+deadagg)
      } else {
        deadagg=0
      }
    }
  }
}

deadcombo$deadsamplecaps<-NA
for (i in 1:nrow(deadcombo)){
  deadagg<-deadcombo$deadagg[i]
  if (deadcombo$Year[i+1]!=deadcombo$Year[i] && i<=(nrow(deadcombo)-1)){
    deadcombo$deadsamplecaps[i]<-deadagg
  } else if (i==(nrow(deadcombo))){
    deadcombo$deadsamplecaps[i]<-deadcombo$deadagg[i]
  }
}

deadsamplecaps<-subset(deadcombo, !is.na(deadcombo$deadsamplecaps))
deadsamplecaps<-subset(deadsamplecaps, select=c(Team, Year, deadsamplecaps))

deadsamplecaps$deadsamplecaps1<-as.character(deadsamplecaps$deadsamplecaps)
deadsamplecaps<-subset(deadsamplecaps, select=-c(deadsamplecaps1))
#BUILD AGGREGATE CAP INFORMATION#
agg=0
x=0
for(eachteam in teamabbrev){
  for(eachyear in years){
    for(i in 1:nrow(combo)){
      if(combo$Team[i]==eachteam && combo$Year[i]==eachyear){
        combo$percentage[i]<-combo$Cap.Hit[i]/combo$Adjusted.cap
        combo$agg[i]<-combo$Cap.Hit[i]+agg
        combo$aggpercentage[i]<-(combo$agg[i]/combo$Adjusted.cap[i])
        agg=(combo$Cap.Hit[i]+agg)
      } else {
        agg=0
      }
    }
  }
}

for (i in 1:nrow(combo)){
  if (combo$Year[i+1]!=combo$Year[i] && i<=(nrow(combo)-1)){
    combo$samplecaps[i]<-combo$agg[i]
  } else if (i==(nrow(combo))){
    combo$samplecaps[i]<-combo$agg[i]
  }
}

teamcapsamples<-subset(combo, combo$samplecaps!='')
sampleskeep<-c("Team", "Year", "samplecaps")
teamcapsamples<-subset(teamcapsamples, select=sampleskeep)

teamcapsamples<-merge(teamcapsamples, deadsamplecaps, by=c("Team", "Year"))

write.csv(teamcapsamples, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcapsamplesPLUS.csv", row.names = FALSE)
write.csv(combo, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/aggregatecaphitsPLUS.csv")


      
      
      
      
      
  