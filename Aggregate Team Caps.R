library(xlsx)
library(tidyverse)
library(plyr)

totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUS.csv")
teamcapscombined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CleanedCapStats.csv")

#CHANGE TEAMS TO TEAM ABBREVIATIONS
teams<-c("arizona-cardinals","atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns",
         "dallas-cowboys", "denver-broncos", "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars", 
         "kansas-city-chiefs", "los-angeles-chargers", "los-angeles-rams", "miami-dolphins", "minnesota-vikings", "new-england-patriots", "new-orleans-saints",
         "new-york-giants", "new-york-jets", "oakland-raiders", "philadelphia-eagles", "pittsburgh-steelers", "san-francisco-49ers", "seattle-seahawks",
         "tampa-bay-buccaneers", "tennessee-titans", "washington-redskins")

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "LAC", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")

years<-c(2013,2014,2015,2016)

##CHANGE FULL TEAM NAMES TO ABBREVIATIONS

#TEAM CAP DATA#
teamcapscombined<-teamcapscombined[order(teamcapscombined$Year, teamcapscombined$Team),]

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
combo<-subset(combo, select=-c(X))

#INITIAL CLEANING#
#combo<-combo[!(combo$Position==''),]

combo$percentage<-(combo$Cap.Dollars/combo$Adjusted.cap)
combo<-unique(combo)
combo$samplecaps<-''

#REMOVE MISSING#
combo<-subset(combo, !is.na(combo$Cap.Dollars))

#BUILD AGGREGATE CAP INFORMATION#
agg=0
x=0
for(eachteam in teamabbrev){
  for(eachyear in years){
    for(i in 1:nrow(combo)){
      if(combo$Team[i]==eachteam && combo$Year[i]==eachyear){
        combo$percentage[i]<-combo$Cap.Dollars[i]/combo$Adjusted.cap
        combo$agg[i]<-combo$Cap.Dollars[i]+agg
        combo$aggpercentage[i]<-(combo$agg[i]/combo$Adjusted.cap[i])
        agg=(combo$Cap.Dollars[i]+agg)
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

write.csv(teamcapsamples, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcapsamples.csv", row.names = FALSE)
write.csv(combo, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/aggregatecaphits.csv")


      
      
      
      
      
  