library(xlsx)
library(tidyverse)
library(plyr)

teams<-c("arizona-cardinals","atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns",
         "dallas-cowboys", "denver-broncos", "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars", 
         "kansas-city-chiefs", "los-angeles-chargers", "los-angeles-rams", "miami-dolphins", "minnesota-vikings", "new-england-patriots", "new-orleans-saints",
         "new-york-giants", "new-york-jets", "oakland-raiders", "philadelphia-eagles", "pittsburgh-steelers", "san-francisco-49ers", "seattle-seahawks",
         "tampa-bay-buccaneers", "tennessee-titans", "washington-redskins")

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "LAC", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SF", "SEA", "TB", "TEN", "WAS")

totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombined.csv")
rosters<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Rosters/Combined/combined.csv")

rosters$Team<-as.character(rosters$Team)
rosters$Player<-as.character(rosters$Player)

totalcaphits$Player<-as.character(totalcaphits$Player)

for (eachteam in teams){
  for (eachobs in totalcaphits$Team){
    if (eachobs==eachteam){
      shrtid<-which((teams)==eachteam)
      obserid<-which((totalcaphits$Test)==eachteam)
      totalcaphits$abbrev<-teamabbrev[shrtid]
    }
  }
}

for (eachteam in teams){
  for (eachobs in totalcaphits$Team){
    if (eachobs==eachteam){
      shrtid<-which((teams)==eachteam)
      obserid<-which((totalcaphits$Team)==eachteam)
      totalcaphits$abbrev[obserid]<-teamabbrev[shrtid]
    }
  }
}

totalcaphits$Team<-totalcaphits$abbrev
totalcaphits<-subset(totalcaphits, select=-abbrev)

missing<-merge(rosters, totalcaphits, by=c("Player", "Team", "Year"),all.x=TRUE)