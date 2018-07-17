library(xlsx)
library(tidyverse)
library(plyr)

fantasyscores<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/Combined/FULLCOMBINED.csv", stringsAsFactors  = FALSE)
fantasyscores<-subset(fantasyscores, fantasyscores$Player!="")
#BRANDON MARSHALL FIX#
fantasyscores$Player<-ifelse(fantasyscores$Player=="Brandon Marshall" & fantasyscores$Team=="DEN" & fantasyscores$Year>=2012,
                             "Brandon Marshall LB", fantasyscores$Player)
fantasyscores<-fantasyscores[order(fantasyscores$Year, fantasyscores$Player, fantasyscores$Week),]

years<-c(2011, 2012, 2013, 2014, 2015, 2016)

weeks<-c(1:15)



#BUILD AGGREGATE CAP INFORMATION#
agg=0
playerindex<-fantasyscores[1,1]
for(i in 1:nrow(fantasyscores)){
  if(fantasyscores$Player[i]==playerindex && i!=(nrow(fantasyscores)-1)){
    fantasyscores$agg[i]<-fantasyscores$Points[i]+agg
    fantasyscores$average[i]<-(fantasyscores$agg[i]/15)
    agg=(fantasyscores$Points[i]+agg)
  } else {
    agg=0
    playerindex<-fantasyscores[i,1]
  }
}


fantasyscores$seasontotal<-NA
for (i in 1:nrow(fantasyscores)){
  if (fantasyscores$Player[i+1]!=fantasyscores$Player[i] && i<=(nrow(fantasyscores)-1)){
    fantasyscores$seasontotal[i]<-fantasyscores$agg[i]
  } else if (i==(nrow(fantasyscores))){
    fantasyscores$seasontotal[i]<-fantasyscores$agg[i]
  }
}

fantasyseasonscores<-subset(fantasyscores, !is.na(fantasyscores$seasontotal))
fantasyseasonscores<-subset(fantasyseasonscores, select=c(Player, Year, seasontotal))

fantasyscores<-merge(fantasyscores, fantasyseasonscores, by=c("Player", "Year"), all.x = TRUE)

fantasyscores<-subset(fantasyscores, select=-c(seasontotal.x, agg, average))

colnames(fantasyscores)[7]<-"seasontotal"

##################################################NEW SCORING SYSTEM FOR OL AND P#####################################################################

totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUSPLUS.csv", stringsAsFactors = FALSE)

snapdata<-read.csv("file:///C:/Users/whjac/Downloads/Term Paper Data/Snap Data/Combined.csv", stringsAsFactors = FALSE)

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "TB", "TEN", "WAS")

stataccess<-("C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/By Position/position.csv")

positions<-c("Quarterbacks","Defensive Line", "Defensive Backs", "Linebackers", "Wide Receivers",
             "Running Backs", "Offensive Line FOR REAL", "Tight Ends", "Punters", "Kickers")

for (position in positions){
  positionstats<-gsub("position", position, stataccess)
  if (position=="Offensive Line FOR REAL"){
    oloutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Punters"){
    poutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Quarterbacks") {
    qboutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  }
}

#CHANGE LAC TO SD & STL TO LAR
qboutput$Team<-ifelse(qboutput$Team=="STL", "LAR", qboutput$Team)

poutput$Team<-ifelse(poutput$Team=="STL", "LAR", qboutput$Team)


#############OFFENSIVE LINE SCORING####################################
oloutput$Team<-ifelse(oloutput$Team=="St. Louis Rams", "Los Angeles Rams", oloutput$Team)
oloutput<-oloutput[with(oloutput, order(Year, Team)),]

olteams<-oloutput$Team[1:32]

for (eachteam in olteams){
  for (eachobs in oloutput$Team){
    if (eachobs==eachteam){
      shrtid<-which((olteams)==eachteam)
      obserid<-which((oloutput$Team)==eachteam)
      oloutput$abbrev<-teamabbrev[shrtid]
    }
  }
}

for (eachteam in olteams){
  for (eachobs in oloutput$Team){
    if (eachobs==eachteam){
      shrtid<-which((olteams)==eachteam)
      obserid<-which((oloutput$Team)==eachteam)
      oloutput$abbrev[obserid]<-teamabbrev[shrtid]
    }
  }
}

oloutput$Team<-oloutput$abbrev
oloutput<-subset(oloutput, select=-abbrev)

qboutput<-qboutput[order(qboutput$Team, qboutput$Year, qboutput$Player),]
qboutput$Yards<-gsub(",", "", qboutput$Yards)
qboutput$Yards<-as.numeric(qboutput$Yards)

qboutput$ydsagg<-0
qboutput$tdagg<-0
qboutput$intagg<-0
qboutput$ydstotal<-NA
qboutput$tdtotal<-NA
qboutput$inttotal<-NA
for (i in 1:nrow(qboutput)){
  year<-qboutput$Year[i]
  yds<-qboutput$Yards[i]
  td<-qboutput$TD[i]
  int<-qboutput$INT[i]
  if (year==qboutput$Year[i+1] && i!=nrow(qboutput)){
    ydsagg<-ydsagg+yds
    tdagg<-tdagg+td
    intagg<-intagg+int
    qboutput$ydsagg[i]<-ydsagg
    qboutput$tdagg[i]<-tdagg
    qboutput$intagg[i]<-intagg
  } else if (year!=qboutput$Year[i+1]  && i!=nrow(qboutput)){
    ydsagg<-ydsagg+yds
    tdagg<-tdagg+td
    intagg<-intagg+int
    qboutput$ydsagg[i]<-ydsagg
    qboutput$tdagg[i]<-tdagg
    qboutput$intagg[i]<-intagg
    qboutput$ydstotal[i]<-ydsagg
    qboutput$tdtotal[i]<-tdagg
    qboutput$inttotal[i]<-intagg
    ydsagg<-0
    tdagg<-0
    intagg<-0
  } else{
    ydsagg<-ydsagg+yds
    tdagg<-tdagg+td
    intagg<-intagg+int
    qboutput$ydsagg[i]<-ydsagg
    qboutput$tdagg[i]<-tdagg
    qboutput$intagg[i]<-intagg
    qboutput$ydstotal[i]<-ydsagg
    qboutput$tdtotal[i]<-tdagg
    qboutput$inttotal[i]<-intagg
  }
}

qboutput<-subset(qboutput, !is.na(qboutput$ydstotal))

oloutput<-merge(oloutput, qboutput, by=c("Team", "Year"))

oloutput<-subset(oloutput, select=-c(Player, Position))

olcaphits<-totalcaphits[which(totalcaphits$Position=="G" | totalcaphits$Position=="RT" | totalcaphits$Position=="LT" | 
                                totalcaphits$Position=="C" | totalcaphits$Position=="T"),]

oloutputcap<-merge(oloutput, olcaphits, by=c("Team", "Year"), all.x = TRUE)

oloutputcap$lastname<-word(string=oloutputcap$Player, start=2, end=2, sep=" ")

snapdata$Team<-ifelse(snapdata$Team=="STL", "LAR", snapdata$Team)

olsnapdata<-snapdata[which(snapdata$Position=="OL"),]
olsnapdata<-subset(olsnapdata, select=-c(Position))

oltotalwsnap<-merge(oloutputcap, olsnapdata, by=c("lastname", "Team", "Year"), all.x=TRUE)
oltotalwsnap<-subset(oltotalwsnap, select=-c(lastname))

olsnapsample<-subset(oltotalwsnap, oltotalwsnap$Year!=2011)

olsnapsample$Total.Snaps<-as.numeric(olsnapsample$Total.Snaps)
olsnapsample$Total.Snaps[is.na(olsnapsample$Total.Snaps)]<-0

olsnapsample<-subset(olsnapsample, select=c(Total.Snaps, Team, Player, Year, Week))

olsnapsample<-olsnapsample[order(olsnapsample$Team, olsnapsample$Player,  olsnapsample$Year, olsnapsample$Week),]

olsnapsample$AggSnaps<-0
olsnapsample$SeasonTotalSnaps<-NA
aggsnaps<-0
for (i in 1:nrow(olsnapsample)){
  team<-olsnapsample$Team[i]
  year<-olsnapsample$Year[i]
  player<-olsnapsample$Player[i]
  snaps<-olsnapsample$Total.Snaps[i]
  nextyear<-olsnapsample$Year[i+1]
  if (year == nextyear && i!=nrow(olsnapsample)){
    aggsnaps<-snaps + aggsnaps
    olsnapsample$AggSnaps[i]<-aggsnaps
  } else if (year!= nextyear && i!=nrow(olsnapsample)){
    aggsnaps<-snaps+aggsnaps
    olsnapsample$AggSnaps[i]<-aggsnaps
    olsnapsample$SeasonTotalSnaps[i]<-aggsnaps
    aggsnaps<-0
  } else {
    aggsnaps<-snaps+aggsnaps
    olsnapsample$AggSnaps[i]<-aggsnaps
    olsnapsample$SeasonTotalSnaps[i]<-aggsnaps
  }
}

olsnapsample<-subset(olsnapsample, !is.na(olsnapsample$SeasonTotalSnaps))
olsnapsample<-subset(olsnapsample, select=c(Player, Team, Year, SeasonTotalSnaps))

olsnapsample<-olsnapsample[order(olsnapsample$Team,  olsnapsample$Year, olsnapsample$Player),]

olsnapsample$aggteamsnaps<-0
olsnapsample$totalteamsnaps<-NA
aggteamsnaps<-0
for (i in 1:nrow(olsnapsample)){
  year<-olsnapsample$Year[i]
  snaps<-olsnapsample$SeasonTotalSnaps[i]
  if (year == olsnapsample$Year[i+1] && i!=nrow(olsnapsample)){
    aggteamsnaps<-aggteamsnaps+snaps
    olsnapsample$aggteamsnaps[i]<-aggteamsnaps
  } else if (year!= olsnapsample$Year[i+1] && i!=nrow(olsnapsample)){
    aggteamsnaps<-aggteamsnaps+snaps
    olsnapsample$aggteamsnaps[i]<-aggteamsnaps
    olsnapsample$totalteamsnaps[i]<-aggteamsnaps
    aggteamsnaps<-0
  } else{
    aggteamsnaps<-aggteamsnaps+snaps
    olsnapsample$aggteamsnaps[i]<-aggteamsnaps
   olsnapsample$totalteamsnaps[i]<-aggteamsnaps
  }
}

teamsnapcounts<-subset(olsnapsample, !is.na(olsnapsample$totalteamsnaps))
teamsnapcounts<-subset(teamsnapcounts, select=c(Team, Year, totalteamsnaps))

olsnapsample<-subset(olsnapsample, select=-c(aggteamsnaps, totalteamsnaps))

olsnapsample<-merge(olsnapsample, teamsnapcounts, by=c("Team", "Year"))
olsnapsample$contribution<-0

olsnapsample$contribution<-(olsnapsample$SeasonTotalSnaps/olsnapsample$totalteamsnaps)*5

oltotal<-merge(oltotalwsnap, olsnapsample, by=c("Player", "Team", "Year"), all.x=TRUE)

oltotal<-subset(oltotal, select=-c(Total.Snaps, Week, Started))
oltotal<-unique(oltotal)

oltotal$Yds<-gsub(",", "", oltotal$Yds)
oltotal$Yds<-as.numeric(oltotal$Yds)

olscoring<-function(firstdowns, tenplus, rushtd, passtd, power, rushyds, passyds, negrush, sack, qbhit, contribution){
  abs((contribution*(firstdowns + tenplus + rushtd + power + (rushyds/50) + (passyds/100) - negrush - sack - qbhit)))
}

olleft<-subset(oltotal, oltotal$Position=="LT")
olright<-subset(oltotal, oltotal$Position=="RT")
olmiddle<-subset(oltotal, oltotal$Position!="RT" & oltotal$Position!="LT")

olleft$seasontotal<-olscoring(olleft$First.Left, olleft$Ten.Left, olleft$TDs, olleft$tdtotal, olleft$Power.Left,
                         olleft$Yds, olleft$ydstotal, olleft$Neg.Left, olleft$Sacks, olleft$QB.Hits, olleft$contribution)

olright$seasontotal<-olscoring(olright$First.Right, olright$Ten.Right, olright$TDs, olright$tdtotal, olright$Power.Right,
                          olright$Yds, olright$ydstotal, olright$Neg.Right, olright$Sacks, olright$QB.Hits, olright$contribution)

olmiddle$seasontotal<-olscoring(olmiddle$First.Center, olmiddle$Ten.Center, olmiddle$TDs, olmiddle$tdtotal, olmiddle$Power.Center,
                           olmiddle$Yds, olmiddle$ydstotal, olmiddle$Neg.Center, olmiddle$Sacks, olmiddle$QB.Hits, olmiddle$contribution)

oloutput<-rbind(olleft, olright, olmiddle)

oloutput<-subset(oloutput, select=c("Player", "Team", "Year", "seasontotal", "Position"),)
oloutput<-subset(oloutput, !is.na(oloutput$seasontotal))

oloutput$Points<-NA
oloutput$Week<-NA
###############################################PUNTER SCORING########################################################
pscoring<-function(Net, Blocked, Inside20, Touchbacks, Faircatches){
  ((Net/50)-(2*Blocked)+Inside20-Touchbacks+(.5*Faircatches))
}

poutput$seasontotal<-pscoring(poutput$Net, poutput$Blocked.Punts, poutput$Inside.20, poutput$Touchbacks, poutput$Fair.Catch)

poutput<-subset(poutput, select=c("Player", "Team", "Year", "seasontotal", "Position"),)

poutput$Points<-NA
poutput$Week<-NA

write.csv(fantasyscores, "C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/Combined/FULLCOMBINED+avg.csv", row.names=FALSE)
write.csv(oloutput, "C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Punter + OL Fantasy Scores/OL.csv", row.names=FALSE)
write.csv(poutput, "C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Punter + OL Fantasy Scores/P.csv", row.names=FALSE)