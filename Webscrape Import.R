library(xlsx)
library(tidyverse)
library(plyr)

years<-c("2013","2014","2015","2016")
         
teams<-c("arizona-cardinals","atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns",
          "dallas-cowboys", "denver-broncos", "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars", 
          "kansas-city-chiefs", "los-angeles-rams", "miami-dolphins", "minnesota-vikings", "new-england-patriots", "new-orleans-saints",
          "new-york-giants", "new-york-jets", "oakland-raiders", "philadelphia-eagles", "pittsburgh-steelers", "san-diego-chargers", "san-francisco-49ers", "seattle-seahawks",
          "st.-louis-rams", "tampa-bay-buccaneers", "tennessee-titans", "washington-redskins")

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "STL", "TB", "TEN", "WAS")

positions<-c("center","cornerback","defensive-end", "defensive-tackle", "fullback", "guard", "inside-linebacker", "kicker", "long-snapper", "outside-linebacker",
            "punter", "quarterback", "running-back", "safety", "tackle", "tight-end", "wide-receiver")

aggdata="C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hit by position/yearteampositionzdata.csv"

combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hit by position/2013arizona-cardinalscenterdata.csv")

for (eachyear in years){
    for (eachteam in teams){
        for (eachposition in positions){
            access<-gsub("year", eachyear, aggdata)
            access<-gsub("team", eachteam, access)
            access<-gsub("positionz", eachposition, access)
            #print(access)
            access<-read.csv(access)
            combined<-rbind.fill(combined,access)
        }
    }
}

combined$Player2<-combined$Player
combined$lastname<-word(string=combined$Player2, start = 2, end = 2, sep = " ")
combined$firstname<-word(string=combined$Player2, start=1, end=1, sep=" ")
combined$screwy<-word(string=combined$Player2, start=1, end=1, sep=" ")

for (fubar in combined$screwy){
  fubarid<-which(combined$screwy==fubar)
  combined$firstname[fubarid]<-gsub(combined$lastname[fubarid], '', fubar)
}

combined$Player<-paste(combined$firstname, combined$lastname)

#combined$Player2<-gsub('([[:upper:]])',' \\1',combined$Player2)
#combined$Player<-gsub('  ', ' ',combined$Player)

namelength<-sapply(gregexpr("\\W+",combined$Player), length)

delete<-(which(combined$Player=="NA NA"))
combined<-combined[-c(delete),]

combined<-subset(combined, select=-firstname)
combined<-subset(combined, select=-screwy)
combined<-subset(combined, select=-Player2)
combined<-subset(combined, select=-lastname)

totalcaphits<-combined

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

totalcaphits<-subset(totalcaphits, select=-c(Rank, X, X..of..Cap, Unnamed..5, Unnamed..6, Unnamed..7, Unnamed..8, Unnamed..9))

totalcaphits$Cap.Dollars<-gsub("\\$", "", totalcaphits$Cap.Dollars)
totalcaphits$Cap.Dollars<-gsub(",", "", totalcaphits$Cap.Dollars)

totalcaphits$Team<-ifelse(totalcaphits$Team=="STL", "LAR", totalcaphits$Team)

write.csv(totalcaphits,"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombined.csv", row.names=FALSE)
totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombined.csv")



