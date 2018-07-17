library(xlsx)
library(tidyverse)
library(plyr)

years<-c("2011","2012","2013","2014","2015","2016")

teams<-c("arizona-cardinals","atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns",
             "dallas-cowboys", "denver-broncos", "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars", 
             "kansas-city-chiefs", "los-angeles-rams", "miami-dolphins", "minnesota-vikings", "new-england-patriots", "new-orleans-saints",
             "new-york-giants", "new-york-jets", "oakland-raiders", "philadelphia-eagles", "pittsburgh-steelers", "san-diego-chargers", "san-francisco-49ers", "seattle-seahawks",
             "st.-louis-rams", "tampa-bay-buccaneers", "tennessee-titans", "washington-redskins")

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "STL", "TB", "TEN", "WAS")

positions<-c("center","cornerback","defensive-end", "defensive-tackle", "fullback", "guard", "inside-linebacker", "kicker", "long-snapper", "outside-linebacker",
             "punter", "quarterback", "running-back", "safety", "tackle", "tight-end", "wide-receiver")

#ACTIVE CAP IMPORT#

activecap<-"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/Active Rosters/yearteam.csv"

combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/Active Rosters/2011arizona-cardinals.csv")

delete<-nrow(combined)
combined<-combined[-c(1:delete),]
for (eachyear in years){
  for (eachteam in teams){
    access<-gsub("year", eachyear, activecap)
    access<-gsub("team", eachteam, access)
    access<-read.csv(access)
    
    #print(access[2])
    
    colnames(access)[2]<-"Player"
    
    dimension<-nrow(access)
    Year<-rep(eachyear, len=dimension)
    Team<-rep(eachteam, len=dimension)
    access<-cbind(access, Year, Team)
    combined<-rbind.fill(combined, access)
  }
}

activecaptotal<-combined

activecaptotal$Player2<-activecaptotal$Player
activecaptotal$firstname<-word(string=activecaptotal$Player2, start = 2, end = 2, sep = " ")
activecaptotal$lastname<-word(string=activecaptotal$Player2, start=1, end=1, sep=" ")

activecaptotal$Player<-paste(activecaptotal$firstname, activecaptotal$lastname)

#TOTAL CAP IMPORT#

totalcap<-"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/year Cap Totalsteam.csv"

combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/2011 Cap Totalsarizona-cardinals.csv")
  
for (eachyear in years){
  for(eachteam in teams){
    access<-gsub("year", eachyear, totalcap)
    access<-gsub("team", eachteam, access)
    access<-read.csv(access)
    
    colnames(access)[2]<-"Player"
    
    dimension<-nrow(access)
    Year<-rep(eachyear, len=dimension)
    Team<-rep(eachteam, len=dimension)
    access<-cbind(access, Year, Team)
    combined<-rbind.fill(combined, access)
  }
}

teamcaptotal<-combined

teamcaptotal<-subset(teamcaptotal, !is.na(teamcaptotal$Team))

teamcaptotal<-subset(teamcaptotal, teamcaptotal$Player=="2011 NFL Salary Cap" | teamcaptotal$Player=="2012 NFL Salary Cap" |
                       teamcaptotal$Player=="2010 Rollover Cap" | teamcaptotal$Player=="2011 Rollover Cap" | teamcaptotal$Player=="Adjustment" |
                       teamcaptotal$Player=="Adjusted Salary Cap")

teamcaptotal$Year<-as.numeric(as.character(teamcaptotal$Year))
teamcaptotal<-subset(teamcaptotal, Year<=2012)

teamcaptotal$Base.Salary<-gsub("\\$", "", teamcaptotal$Base.Salary)
teamcaptotal$Base.Salary<-gsub(",", "", teamcaptotal$Base.Salary)

caps<-subset(teamcaptotal, teamcaptotal$Player=="2011 NFL Salary Cap" | teamcaptotal$Player=="2012 NFL Salary Cap")
caps<-subset(caps, select=c(Base.Salary, Year))
caps<-unique(caps)
colnames(caps)[1]<-"Cap"

carryover<-subset(teamcaptotal, teamcaptotal$Player=="2010 Rollover Cap" | teamcaptotal$Player=="2011 Rollover Cap")
carryover<-subset(carryover, select=c(Base.Salary, Year, Team))
colnames(carryover)[1]<-"Carryover"

adjusted<-subset(teamcaptotal, teamcaptotal$Player=="Adjusted Salary Cap")
adjusted<-subset(adjusted, select=c(Base.Salary, Year, Team))
colnames(adjusted)[1]<-"Adjusted.cap"

adjustments<-subset(teamcaptotal, teamcaptotal$Player=="Adjustment")
adjustments<-subset(adjustments, select=c(Base.Salary, Year, Team))
colnames(adjustments)[1]<-"Adjustments"

newteamcapinfo<-merge(caps, carryover, by=c("Year"))
newteamcapinfo<-merge(newteamcapinfo, adjusted, by=c("Year", "Team"))
newteamcapinfo<-merge(newteamcapinfo, adjustments, by=c("Year", "Team"))

newteamcapinfo$Team<-gsub("-", " ", newteamcapinfo$Team)

#DEAD CAP IMPORT#

  #BUILD LIST OF ELEMENTS FOR FUTURE EXCLUSION#
  keytotal<-as.data.frame("2011 arizona-cardinals")
  colnames(keytotal)[1]<-"key"
  keytotal$Team<-"arizona-cardinals"
  keytotal$Year<-"2011"
  
  
  for (eachyear in years){
    for(eachteam in teams){
      key$key<-paste(eachyear, eachteam, sep=" ")
      key$Year<-eachyear
      key$Team<-eachteam
      key<-as.data.frame(key)
      keytotal<-rbind(keytotal, key)
    }
  }
  keytotal<-keytotal[-1,]
  
  keytotal<-lapply(keytotal, function(x) as.character(gsub(" ", " Dead Cap", x)))
  keytotal<-as.data.frame(keytotal)
  
  #EXCLUDE MISSING ELEMENTS#
  keytotal<-keytotal[-(which(keytotal$key=="2011 Dead Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Dead Capsan-diego-chargers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2012 Dead Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2013 Dead Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2014 Dead Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2014 Dead Capsan-diego-chargers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2015 Dead Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2016 Dead Capst.-louis-rams")),]

deadcap<-"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/observation.csv"

combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/2011 Dead Caparizona-cardinals.csv")

for (i in 1:nrow(keytotal)){
      eachkey<-keytotal$key[i]
      eachyear<-keytotal$Year[i]
      eachteam<-keytotal$Team[i]
      access<-gsub("observation", eachkey, deadcap)
      #print(access)
      access<-read.csv(access)
    
      colnames(access)[2]<-"Player"
      
      dimension<-nrow(access)
      Year<-rep(eachyear, len=dimension)
      Team<-rep(eachteam, len=dimension)
      access<-cbind(access, Year, Team)
      combined<-rbind.fill(combined, access)
}

deadcaptotal<-combined

deadcaptotal<-deadcaptotal[!is.na(deadcaptotal$Player),]

deadcaptotal$Player2<-deadcaptotal$Player
deadcaptotal$lastname<-word(string=deadcaptotal$Player2, start = 2, end = 2, sep = " ")
deadcaptotal$firstname<-word(string=deadcaptotal$Player2, start=1, end=1, sep=" ")

for (i in 1:nrow(deadcaptotal)){
  replace<-deadcaptotal$lastname[i]
  #print(replace)
  deadcaptotal$firstname[i]<-gsub(replace, "", deadcaptotal$firstname[i])
}

deadcaptotal$Player<-paste(deadcaptotal$firstname, deadcaptotal$lastname)


#INJURED RESERVE CAP IMPORT#

  #BUILD LIST OF ELEMENTS FOR FUTURE EXCLUSION#
  key<-as.data.frame("2011 arizona-cardinals")
  colnames(key)[1]<-"key"

  keytotal<-as.data.frame("2011 arizona-cardinals")
  colnames(keytotal)[1]<-"key"
  keytotal$Team<-"arizona-cardinals"
  keytotal$Year<-"2011"
  
  for (eachyear in years){
    for(eachteam in teams){
      key$key<-paste(eachyear, eachteam, sep=" ")
      key$Year<-eachyear
      key$Team<-eachteam
      key<-as.data.frame(key)
      keytotal<-rbind(keytotal, key)
    }
  }
  
  keytotal<-keytotal[-1,]
  
  keytotal<-lapply(keytotal, function(x) as.character(gsub(" ", " Injured Reserve Cap", x)))
  keytotal<-as.data.frame(keytotal)
  
  #EXCLUDE MISSING OBSERVATIONS#
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Caparizona-cardinals")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capatlanta-falcons")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capcarolina-panthers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capchicago-bears")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capcleveland-browns")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capdallas-cowboys")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capdenver-broncos")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capgreen-bay-packers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capnew-york-giants")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capnew-york-jets")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capphiladelphia-eagles")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capsan-diego-chargers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Capsan-francisco-49ers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Captampa-bay-buccaneers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2011 Injured Reserve Captennessee-titans")),]
  keytotal<-keytotal[-(which(keytotal$key=="2012 Injured Reserve Caparizona-cardinals")),]
  keytotal<-keytotal[-(which(keytotal$key=="2012 Injured Reserve Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2013 Injured Reserve Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2014 Injured Reserve Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2014 Injured Reserve Capsan-diego-chargers")),]
  keytotal<-keytotal[-(which(keytotal$key=="2015 Injured Reserve Caplos-angeles-rams")),]
  keytotal<-keytotal[-(which(keytotal$key=="2016 Injured Reserve Capst.-louis-rams")),]
  
IRcap<-"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/observation.csv"
  
combined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Total Cap Hits/2011 Injured Reserve Capbaltimore-ravens.csv")


  
for (i in 1:nrow(keytotal)){
  eachkey<-keytotal$key[i]
  eachyear<-keytotal$Year[i]
  eachteam<-keytotal$Team[i]
  access<-gsub("observation", eachkey, IRcap)
  #print(access)
  access<-read.csv(access)
    
  colnames(access)[2]<-"Player"
    
  dimension<-nrow(access)
  Year<-rep(eachyear, len=dimension)
  Team<-rep(eachteam, len=dimension)
  access<-cbind(access, Year, Team)
  combined<-rbind.fill(combined, access)
}
  
IRcaptotal<-combined
  
IRcaptotal<-IRcaptotal[!is.na(IRcaptotal$Player),]

IRcaptotal$Player2<-IRcaptotal$Player
IRcaptotal$lastname<-word(string=IRcaptotal$Player2, start = 2, end = 2, sep = " ")
IRcaptotal$firstname<-word(string=IRcaptotal$Player2, start=1, end=1, sep=" ")

for (i in 1:nrow(IRcaptotal)){
  replace<-IRcaptotal$lastname[i]
  #print(replace)
  IRcaptotal$firstname[i]<-gsub(replace, "", IRcaptotal$firstname[i])
}

IRcaptotal$Player<-paste(IRcaptotal$firstname, IRcaptotal$lastname)



deadcaptotal$Deadcap<-1
deadcaptotal$IR<-0
IRcaptotal$IR<-1
IRcaptotal$Deadcap<-0
activecaptotal$Deadcap<-0
activecaptotal$IR<-0

totalcombinedcap<-rbind.fill(activecaptotal, deadcaptotal, IRcaptotal)

#ifelse(totalcombinedcap$Team=="St. Louis Rams", "Los Angeles Rams", totalcombinedcap$Team)

combined<-totalcombinedcap

totalcaphits<-combined

for (eachteam in teams){
  for (eachobs in totalcaphits$Team){
    if (eachobs==eachteam){
      shrtid<-which((teams)==eachteam)
      obserid<-which((totalcaphits$Team)==eachteam)
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


#REFORMAT MONEY FIGURES#
totalcaphits$Cap.Hit<-gsub("\\$", "", totalcaphits$Cap.Hit)
totalcaphits$Cap.Hit<-gsub(",", "", totalcaphits$Cap.Hit)

totalcaphits$Base.Salary<-gsub("\\$", "", totalcaphits$Base.Salary)
totalcaphits$Base.Salary<-gsub(",", "", totalcaphits$Base.Salary)

totalcaphits$Signing.Bonus<-gsub("\\$", "", totalcaphits$Signing.Bonus)
totalcaphits$Signing.Bonus<-gsub(",", "", totalcaphits$Signing.Bonus)

totalcaphits$Roster.Bonus<-gsub("\\$", "", totalcaphits$Roster.Bonus)
totalcaphits$Roster.Bonus<-gsub(",", "", totalcaphits$Roster.Bonus)

totalcaphits$Option.Bonus<-gsub("\\$", "", totalcaphits$Option.Bonus)
totalcaphits$Option.Bonus<-gsub(",", "", totalcaphits$Option.Bonus)

totalcaphits$Workout.Bonus<-gsub("\\$", "", totalcaphits$Workout.Bonus)
totalcaphits$Workout.Bonus<-gsub(",", "", totalcaphits$Workout.Bonus)

totalcaphits$Restruc..Bonus<-gsub("\\$", "", totalcaphits$Restruc..Bonus)
totalcaphits$Restruc..Bonus<-gsub(",", "", totalcaphits$Restruc..Bonus)

totalcaphits$Restruc..Bonus<-gsub("\\$", "", totalcaphits$Restruc..Bonus)
totalcaphits$Restruc..Bonus<-gsub(",", "", totalcaphits$Restruc..Bonus)

totalcaphits$Misc.<-gsub("\\$", "", totalcaphits$Misc.)
totalcaphits$Misc.<-gsub(",", "", totalcaphits$Misc.)

totalcaphits$Dead.Cap<-gsub("\\$", "", totalcaphits$Dead.Cap)
totalcaphits$Dead.Cap<-gsub(",", "", totalcaphits$Dead.Cap)
totalcaphits$Dead.Cap<-gsub("\\)", "", totalcaphits$Dead.Cap)
totalcaphits$Dead.Cap<-gsub("\\(", "", totalcaphits$Dead.Cap)


totalcaphits$Team<-ifelse(totalcaphits$Team=="STL", "LAR", totalcaphits$Team)

totalcaphits<-subset(totalcaphits, select=-c(Active.Players..63., X, Cap.., Player..14., Player..4., Player2, firstname, lastname, abbrev))

colnames(totalcaphits)[1]<-"Position"

write.csv(totalcaphits,"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUSPLUS.csv", row.names=FALSE)
write.csv(newteamcapinfo, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/NewCapStats.csv", row.names=FALSE)
totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUSPLUS.csv")
