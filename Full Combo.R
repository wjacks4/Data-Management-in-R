library(xlsx)
library(tidyverse)
library(plyr)

totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUS.csv", stringsAsFactors = FALSE)
teamcapscombined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CleanedCapStats.csv", stringsAsFactors = FALSE)
injurydata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Injury Data/FINAL FULL.csv")
snapdata<-read.csv("file:///C:/Users/whjac/Downloads/Term Paper Data/Snap Data/Combined.csv", stringsAsFactors = FALSE)
contractsyears<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Yearly Contract Information.csv", stringsAsFactors = FALSE)
rosters<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Rosters/Combined/combined.csv")
profiles<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Kernel Data/profilescleaned.csv", stringsAsFactors = FALSE)
aggcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/aggregatecaphits.csv")
franchisedata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/BASIC DATA/Franchise Tag Costs.csv")
teamcapsamples<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcapsamples.csv")
winlossdata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/BASIC DATA/Win Loss Data.csv")
fantasyscores<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/Combined/FULLCOMBINED+avg.csv", stringsAsFactors  = FALSE)
fantasyscoresagg<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Team Aggregate Scores/Fantasy Aggregate Scores.csv", stringsAsFactors= FALSE)

#CHANGE TEAMS TO TEAM ABBREVIATIONS
teams<-c("arizona-cardinals","atlanta-falcons", "baltimore-ravens", "buffalo-bills", "carolina-panthers", "chicago-bears", "cincinnati-bengals", "cleveland-browns",
         "dallas-cowboys", "denver-broncos", "detroit-lions", "green-bay-packers", "houston-texans", "indianapolis-colts", "jacksonville-jaguars", 
         "kansas-city-chiefs", "los-angeles-rams", "miami-dolphins", "minnesota-vikings", "new-england-patriots", "new-orleans-saints",
         "new-york-giants", "new-york-jets", "oakland-raiders", "philadelphia-eagles", "pittsburgh-steelers", "san-diego-chargers", "san-francisco-49ers", "seattle-seahawks",
         "tampa-bay-buccaneers", "tennessee-titans", "washington-redskins")

teamabbrev<-c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LAR", "MIA", "MIN",
              "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SF", "SEA", "TB", "TEN", "WAS")


#WRITE POSITION OUTPUT DATASETS
stataccess<-("C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/By Position/position.csv")

positions<-c("Quarterbacks","Defensive Line", "Defensive Backs", "Linebackers", "Wide Receivers",
             "Running Backs", "Offensive Line", "Tight Ends", "Punters", "Kickers")

for (position in positions){
  positionstats<-gsub("position", position, stataccess)
  if (position=="Quarterbacks"){
    qboutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Defensive Line"){
    dloutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Defensive Backs"){
    dboutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Linebackers"){
    lboutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Wide Receivers"){
    wroutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Running Backs"){
    rboutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Offensive Line"){
    oloutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Tight Ends"){
    teoutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else if (position=="Punters"){
    poutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  } else {
    koutput<-read.csv(positionstats, stringsAsFactors = FALSE)
  }
}

#CHANGE LAC TO SD & STL TO LAR#
totaloutput<-rbind.fill(qboutput, dloutput, dboutput, lboutput, wroutput, rboutput, oloutput, teoutput, poutput, koutput)

totaloutput$Team2<-as.character(totaloutput$Team)
totaloutput$Team<-totaloutput$Team2
totaloutput<-subset(totaloutput, select=-Team2)

totaloutput$Position2<-as.character(totaloutput$Position)
totaloutput$Position<-totaloutput$Position2
totaloutput<-subset(totaloutput, select=-Position2)

totaloutput$Team<-ifelse(totaloutput$Team=="LAC", "SD", totaloutput$Team)
totaloutput$Team<-ifelse(totaloutput$Team=="STL", "LAR", totaloutput$Team)

qboutput<-subset(totaloutput, Position=="QB")
totaloutput<-subset(totaloutput, Position!="QB")

dloutput<-subset(totaloutput, Position=="DT" | Position=="NT" | Position=="DE" | Position=="DL")
totaloutput<-subset(totaloutput, Position!="DT" & Position!="NT" & Position!="DE" & Position!="DL")

dboutput<-subset(totaloutput, Position=="FS" | Position=="SS" | Position=="S" | Position=="CB" | Position=="DB")
totaloutput<-subset(totaloutput, Position!="FS" & Position!="SS" & Position!="S" & Position!="CB" & Position!="DB")

lboutput<-subset(totaloutput, Position=="LB" | Position=="ILB"| Position=="OLB")
totaloutput<-subset(totaloutput, Position!="LB" & Position!="ILB" & Position!="OLB")

wroutput<-subset(totaloutput, Position=="WR")
totaloutput<-subset(totaloutput, Position!="WR")

rboutput<-subset(totaloutput, Position=="RB" | Position=="FB")
totaloutput<-subset(totaloutput, Position!="RB" & Position!="FB")

teoutput<-subset(totaloutput, Position=="TE")
totaloutput<-subset(totaloutput, Position!="TE")

poutput<-subset(totaloutput, Position=="P")
totaloutput<-subset(totaloutput, Position!="P")

koutput<-subset(totaloutput, Position=="K")
totaloutput<-subset(totaloutput, Position!="K")

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


#SEPARATE BADLY FORMATTED NAMES IN QB / PUNTER OUTPUT DATASET - 
qboutput$Player<-as.character(qboutput$Player)
namelength<-nchar(qboutput$Player, type = "chars", allowNA = FALSE)
qboutput$lastname<-word(string=qboutput$Player, start=3, end=(namelength+1),sep="")

for(punter in poutput$Player){
  poutput$Player<-as.character(poutput$Player)
  namelength<-nchar(poutput$Player, type="chars", allowNA = FALSE)
  poutput$Name<-word(string=poutput$Player, start=1, end=(namelength-2),sep="")
}

poutput$Player<-poutput$Name
poutput<-subset(poutput, select=-c(Name))

#ExTRACT LAST NAME FROM TOTAL CAP HITS TO USE FOR MERGING W/QUARTERBACKS
totalcaphits$Player<-as.character(totalcaphits$Player)
totalcaphits$lastname<-word(string=totalcaphits$Player, start=2, end=2, sep=" ")


#MERGE OUTPUT & cAP DATASETS
#CHANGE ALL STL TO LAR IN CAP HITS
totalcaphits$team<-ifelse(totalcaphits$Team=="STL", "LAR", totalcaphits$Team)

qbcaphits<-totalcaphits[which(totalcaphits$Position=="QB"),]
qboutputcap<-merge(qbcaphits, qboutput, by=c("lastname","Team", "Year"), all.x=TRUE)
qboutputcap$Player<-qboutputcap$Player.x
qboutputcap<-subset(qboutputcap, select=-c(Player.x, Player.y))

dlcaphits<-totalcaphits[which(totalcaphits$Position=="DE" | totalcaphits$Position=="DT"),]
dloutputcap<-merge(dlcaphits, dloutput, by=c("Player", "Team", "Year"), all.x=TRUE)

dbcaphits<-totalcaphits[which(totalcaphits$Position=="CB" | totalcaphits$Position=="S" | totalcaphits$Position=="FS" | totalcaphits$Position=="SS"),]
dboutputcap<-merge(dbcaphits, dboutput, by=c("Player", "Team", "Year"), all.x=TRUE)

lbcaphits<-totalcaphits[which(totalcaphits$Position=="ILB" | totalcaphits$Position=="LB" | totalcaphits$Position=="OLB"),]
lboutputcap<-merge(lbcaphits, lboutput, by=c("Player", "Team", "Year"), all.x=TRUE)

pcaphits<-totalcaphits[which(totalcaphits$Position=="P"),]
poutputcap<-merge(pcaphits, poutput, by=c("Player", "Team", "Year"), all.x=TRUE)

rbcaphits<-totalcaphits[which(totalcaphits$Position=="RB" | totalcaphits$Position=="FB"),]
rboutputcap<-merge(rbcaphits, rboutput, by=c("Player", "Team", "Year"), all.x=TRUE)

tecaphits<-totalcaphits[which(totalcaphits$Position=="TE"),]
teoutputcap<-merge(tecaphits, teoutput, by=c("Player", "Team", "Year"), all.x=TRUE)

wrcaphits<-totalcaphits[which(totalcaphits$Position=="WR"),]
wroutputcap<-merge(wrcaphits, wroutput, by=c("Player", "Team", "Year"), all.x=TRUE)

kcaphits<-totalcaphits[which(totalcaphits$Position=="K"),]
koutputcap<-merge(kcaphits, koutput, by=c("Player", "Team", "Year"), all.x=TRUE)

olcaphits<-totalcaphits[which(totalcaphits$Position=="G" | totalcaphits$Position=="RT" | totalcaphits$Position=="LT" | totalcaphits$Position=="C" | totalcaphits$Position=="T"),]
oloutputcap<-merge(olcaphits, oloutput, by=c("Team", "Year"), all.x=TRUE)

#FIX VARIABLE NAMES FOR FANTASY MERGE#
totaloutputcap<-rbind.fill(qboutputcap, dboutputcap, dloutputcap, lboutputcap, poutputcap, rboutputcap, teoutputcap, wroutputcap, koutputcap, oloutputcap)

fantasyscores$Position<-toupper(fantasyscores$Position)

totaloutputcap$Position<-totaloutputcap$Position.x

#CHANGE POSITIONS FOR FANTASY MERGE#
fantasyscores$FantasyMergePosition<-fantasyscores$Position

qboutputcap<-subset(totaloutputcap, totaloutputcap$Position=="QB")
qboutputcap$FantasyMergePosition<-"QB"
totaloutputcap<-subset(totaloutputcap, Position!="QB")

dloutputcap<-subset(totaloutputcap, Position=="DT" | Position=="NT" | Position=="DE" | Position=="DL")
dloutputcap$FantasyMergePosition<-"DL"
totaloutputcap<-subset(totaloutputcap, Position!="DT" & Position!="NT" & Position!="DE" & Position!="DL")

dboutputcap<-subset(totaloutputcap, Position=="FS" | Position=="SS" | Position=="S" | Position=="CB" | Position=="DB")
dboutputcap$FantasyMergePosition<-"DB"
totaloutputcap<-subset(totaloutputcap, Position!="FS" & Position!="SS" & Position!="S" & Position!="CB" & Position!="DB")

lboutputcap<-subset(totaloutputcap, Position=="LB" | Position=="ILB"| Position=="OLB")
lboutputcap$FantasyMergePosition<-"LB"
totaloutputcap<-subset(totaloutputcap, Position!="LB" & Position!="ILB" & Position!="OLB")

wroutputcap<-subset(totaloutputcap, Position=="WR")
wroutputcap$FantasyMergePosition<-"WR"
totaloutputcap<-subset(totaloutputcap, Position!="WR")

rboutputcap<-subset(totaloutputcap, Position=="RB" | Position=="FB")
rboutputcap$FantasyMergePosition<-"RB"
totaloutputcap<-subset(totaloutputcap, Position!="RB" & Position!="FB")

teoutputcap<-subset(totaloutputcap, Position=="TE")
teoutputcap$FantasyMergePosition<-"TE"
totaloutputcap<-subset(totaloutputcap, Position!="TE")

poutputcap<-subset(totaloutputcap, Position=="P")
poutputcap$FantasyMergePosition<-"P"
totaloutputcap<-subset(totaloutputcap, Position!="P")

koutputcap<-subset(totaloutputcap, Position=="K")
koutputcap$FantasyMergePosition<-"K"
totaloutputcap<-subset(totaloutputcap, Position!="K")

oloutputcap<-subset(totaloutputcap, Position=="G" | Position=="C" | Position=="RT" | Position=="LT" | Position=="T")
oloutputcap$FantasyMergePosition<-"OL"
totaloutputcap<-subset(totaloutputcap, Position!="G" & Position!="C" & Position!="RT" & Position!="LT" & Position!="T")


#FANTASY MERGE#
qboutputcapF<-merge(qboutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

dloutputcapF<-merge(dloutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

dboutputcapF<-merge(dboutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

lboutputcapF<-merge(lboutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

poutputcapF<-merge(poutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

rboutputcapF<-merge(rboutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

teoutputcapF<-merge(teoutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

wroutputcapF<-merge(wroutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

koutputcapF<-merge(koutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

oloutputcapF<-merge(oloutputcap, fantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

#FANTASY SCORING FOR MISSING OBSERVATIONS#
Offensescoring<-function(pyds, ptd, rtd, rectd, int, rec, recyds, rshyds, fum){
  (pyds/25)+(ptd*4)-(int*2)+(rshyds/10)+(rtd*6)+(recyds/10)-(fum*2)+(rectd*6)
}

rboutputcapF$replacescore<-0
rboutputcapF$replacescore<-Offensescoring(0,0,rboutputcapF$TD,0,0,0,0,rboutputcapF$Yds,0)
rboutputcapF$seasontotal<-ifelse(is.na(rboutputcapF$seasontotal), rboutputcapF$replacescore, rboutputcapF$seasontotal)
rboutputcapF<-subset(rboutputcapF, select=-c(replacescore))

qboutputcapF$replacescore<-0
qboutputcapF$Yards<-as.numeric(qboutputcapF$Yards)
qboutputcapF$replacescore<-Offensescoring(qboutputcapF$Yards, qboutputcapF$TD, 0, 0, qboutputcapF$INT, 0, 0, 0, qboutputcapF$FL)
qboutputcapF$seasontotal<-ifelse(is.na(qboutputcapF$seasontotal), qboutputcapF$replacescore, qboutputcapF$seasontotal)
qboutputcapF<-subset(qboutputcapF, select=-c(replacescore))

teoutputcapF$replacescore<-0
teoutputcapF$replacescore<-Offensescoring(0,0,0,teoutputcapF$TD, 0, 0,teoutputcapF$Yds,0 , 0)
teoutputcapF$seasontotal<-ifelse(is.na(teoutputcapF$seasontotal), teoutputcapF$replacescore, teoutputcapF$seasontotal)
teoutputcapF<-subset(teoutputcapF, select=-c(replacescore))

wroutputcapF$replacescore<-0
wroutputcapF$replacescore<-Offensescoring(0,0,0,wroutputcapF$TD, 0, 0,wroutputcapF$Yds,0 , 0)
wroutputcapF$seasontotal<-ifelse(is.na(wroutputcapF$seasontotal), wroutputcapF$replacescore, wroutputcapF$seasontotal)
wroutputcapF<-subset(wroutputcapF, select=-c(replacescore))


Defensescoring<-function(solo, asst, sck, sckyd, tfl, qbhit, def, int, ff, frec, dtd){
  (solo) + (asst*.5) + (sck*2) + (sckyd/10) + tfl + qbhit + def + (int*3) + (ff*3) + (frec*3) + (dtd*6)
}

dloutputcapF$replacescore<-0
dloutputcapF$replacescore<-Defensescoring(dloutputcapF$Solo, dloutputcapF$Asst, 0, dloutputcapF$TFL, 0, 0, 0, 0, 0, 0, 0)
dloutputcapF$seasontotal<-ifelse(is.na(dloutputcapF$seasontotal), dloutputcapF$replacescore, dloutputcapF$seasontotal)
dloutputcapF<-subset(dloutputcapF, select=-c(replacescore))

lboutputcapF$replacescore<-0
lboutputcapF$replacescore<-Defensescoring(lboutputcapF$Solo, lboutputcapF$Asst, lboutputcapF$Sck, lboutputcapF$Sck.Yds, lboutputcapF$TFL,
                                          0, 0, 0, 0, 0, 0)
lboutputcapF$seasontotal<-ifelse(is.na(lboutputcapF$seasontotal), lboutputcapF$replacescore, lboutputcapF$seasontotal)
lboutputcapF<-subset(lboutputcapF, select=-c(replacescore))

dboutputcapF$replacescore<-0
dboutputcapF$replacescore<-Defensescoring(dboutputcapF$Solo, dboutputcapF$Asst, dboutputcapF$Sck, 0, dboutputcapF$TFL,
                                          dboutputcapF$QB.Hits, dboutputcapF$Pass.Def, dboutputcapF$Int, dboutputcapF$FF, dboutputcapF$Fum.Rec, 0)
dboutputcapF$seasontotal<-ifelse(is.na(dboutputcapF$seasontotal), dboutputcapF$replacescore, dboutputcapF$seasontotal)
dboutputcapF<-subset(dboutputcapF, select=-c(replacescore))

Kickerscoring<-function(made, long){
  (made*6) + (long*6)
}

Kickerscoring(23, 0)

koutputcapF$replacescore<-0
koutputcapF$replacescore<-Kickerscoring(koutputcapF$FG.Made, 0)
koutputcapF$seasontotal<-ifelse(is.na(koutputcapF$seasontotal), koutputcapF$replacescore, koutputcapF$seasontotal)
koutputcapF<-subset(koutputcapF, select=-c(replacescore))

#COMBINE ALL DATASETS TO DO CLEANING# 
totaloutputcapF<-rbind(qboutputcapF, dloutputcapF, dboutputcapF, lboutputcapF, poutputcapF, rboutputcapF, teoutputcapF, wroutputcapF, koutputcapF, oloutputcapF)

totaloutputcapF$Team<-totaloutputcapF$Team.x
totaloutputcapF$Position<-totaloutputcapF$Position.x

qboutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="QB")
dloutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="DT" | totaloutputcapF$Position=="NT" | totaloutputcapF$Position=="DE" | totaloutputcapF$Position=="DL")
dboutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="FS" | totaloutputcapF$Position=="SS" | totaloutputcapF$Position=="S" | totaloutputcapF$Position=="CB" | totaloutputcapF$Position=="DB")
lboutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="LB" | totaloutputcapF$Position=="ILB"| totaloutputcapF$Position=="OLB")
wroutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="WR")
rboutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="RB" | totaloutputcapF$Position=="FB")
teoutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="TE")
poutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="P")
koutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="K")
oloutputcapF<-subset(totaloutputcapF, totaloutputcapF$Position=="G" | totaloutputcapF$Position=="C" | totaloutputcapF$Position=="RT" | totaloutputcapF$Position=="LT" | totaloutputcapF$Position=="T")

##CHANGE FULL TEAM CAP NAMES TO ABBREVIATIONS##
teamcapscombined$Team<-tolower(teamcapscombined$Team)
teamcapscombined$Team<-ifelse(teamcapscombined$Team=="st. louis rams", "los angeles rams", teamcapscombined$Team)
teamcapscombined<-teamcapscombined[with(teamcapscombined, order(Year, Team)),]

teams2<-teamcapscombined$Team[1:32]

#CAP DATA COMBINED
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
teamcapscombined<-subset(teamcapscombined, !is.na(teamcapscombined$Year))

##MERGE TEAM CAP INFO WITH POSITION CAP INFO
qboutputcapF<-merge(qboutputcapF, teamcapscombined, by=c("Team", "Year"))
dloutputcapF<-merge(dloutputcapF, teamcapscombined, by=c("Team", "Year"))
dboutputcapF<-merge(dboutputcapF, teamcapscombined, by=c("Team", "Year"))
lboutputcapF<-merge(lboutputcapF, teamcapscombined, by=c("Team", "Year"))
poutputcapF<-merge(poutputcapF, teamcapscombined, by=c("Team", "Year"))
rboutputcapF<-merge(rboutputcapF, teamcapscombined, by=c("Team", "Year"))
teoutputcapF<-merge(teoutputcapF, teamcapscombined, by=c("Team", "Year"))
wroutputcapF<-merge(wroutputcapF, teamcapscombined, by=c("Team", "Year"))
koutputcapF<-merge(koutputcapF, teamcapscombined, by=c("Team", "Year"))
oloutputcapF<-merge(oloutputcapF, teamcapscombined, by=c("Team", "Year"))

##CLEAN INJURY & SNAP DATA & COMBINE
snapdata$Team<-ifelse(snapdata$Team=="STL", "LAR", snapdata$Team)

qbsnapdata<-snapdata[which(snapdata$Position=="QB"),]
qbsnapdata<-subset(qbsnapdata, select=-c(Position))
qboutputcapF<-merge(qboutputcapF, qbsnapdata, by=c("lastname", "Team", "Year", "Week"),   all.x = TRUE, sort = TRUE)            
qbtotal<-merge(qboutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x = TRUE)

dlsnapdata<-snapdata[which(snapdata$Position=="DL"),]
dlsnapdata<-subset(dlsnapdata, select=-c(Position))
dloutputcapF<-merge(dloutputcapF, dlsnapdata, by=c("lastname", "Team", "Year", "Week"), all.x = TRUE)
dltotal<-merge(dloutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x = TRUE)

dbsnapdata<-snapdata[which(snapdata$Position=="DB"),]
dbsnapdata<-subset(dbsnapdata, select=-c(Position))
dboutputcapF<-merge(dboutputcapF, dbsnapdata, by=c("lastname", "Team", "Year", "Week"), all.x=TRUE)
dbtotal<-merge(dboutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x = TRUE)

lbsnapdata<-snapdata[which(snapdata$Position=="LB"),]
lbsnapdata<-subset(lbsnapdata, select=-c(Position))
lboutputcapF<-merge(lboutputcapF, lbsnapdata, by=c("lastname", "Team", "Year", "Week"), all.x = TRUE)
lbtotal<-merge(lboutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)

psnapdata<-snapdata[which(snapdata$Position=="ST"),]
psnapdata<-subset(psnapdata, select=-c(Position))
poutputcapF<-merge(poutputcapF, psnapdata, by=c("lastname", "Team", "Year"), all.x = TRUE)
poutputcapF$Week<-poutputcapF$Week.y
poutputcapF<-subset(poutputcapF, select=-c(Week.x, Week.y))
ptotal<-merge(poutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)

rbsnapdata<-snapdata[which(snapdata$Position=="RB"),]
rbsnapdata<-subset(rbsnapdata, select=-c(Position))
rboutputcapF<-merge(rboutputcapF, rbsnapdata, by=c("lastname", "Team", "Year", "Week"), all.x = TRUE)
rbtotal<-merge(rboutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)

tesnapdata<-snapdata[which(snapdata$Position=="TE"),]
tesnapdata<-subset(tesnapdata, select=-c(Position))
teoutputcapF<-merge(teoutputcapF, tesnapdata, by=c("lastname", "Team", "Year", "Week"), all.x=TRUE)
tetotal<-merge(teoutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)

wrsnapdata<-snapdata[which(snapdata$Position=="WR"),]
wrsnapdata<-subset(wrsnapdata, select=-c(Position))
wroutputcapF<-merge(wroutputcapF, wrsnapdata, by=c("lastname", "Team", "Year", "Week"), all.x = TRUE)
wrtotal<-merge(wroutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)

ksnapdata<-snapdata[which(snapdata$Position=="ST"),]
ksnapdata<-subset(ksnapdata, select=-c(Position))
koutputcapF<-merge(koutputcapF, ksnapdata, by=c("lastname", "Team", "Year", "Week"), all.x = TRUE)
ktotal<-merge(koutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)

olsnapdata<-snapdata[which(snapdata$Position=="OL"),]
olsnapdata<-subset(olsnapdata, select=-c(Position))
oloutputcapF<-merge(oloutputcapF, olsnapdata, by=c("lastname", "Team", "Year"), all.x=TRUE)
oloutputcapF$Week<-oloutputcapF$Week.y
oloutputcapF<-subset(oloutputcapF, select=-c(Week.x, Week.y))
oltotal<-merge(oloutputcapF, injurydata, by=c("Player", "Year", "Week"), all.x=TRUE)


##ADD CONTRACT INFO / DELETE DUPLICATE COLUMNS
#CHANGE STL TO LAR#
contractsyears$Team<-ifelse(contractsyears$Team=="STL", "LAR", contractsyears$Team)

#CLEAN CONTRACT INFO#
contractsyears<-subset(contractsyears, select=-c(X, Position))

qbtotal<-merge(qbtotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

dltotal<-merge(dltotal, contractsyears, by=c("Player", "Team", "Year"), all.x=TRUE)

dbtotal<-merge(dbtotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

lbtotal<-merge(lbtotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

ptotal<-merge(ptotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

rbtotal<-merge(rbtotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

tetotal<-merge(tetotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

wrtotal<-merge(wrtotal, contractsyears, by=c("Player", "Team", "Year"), all.x = TRUE)

ktotal<-merge(ktotal, contractsyears, by=c("Player", "Team", "Year"), all.x= TRUE)

oltotal<-merge(oltotal, contractsyears, by=c("Player", "Team", "Year"), all.x=TRUE)


#ADD PROFILE DATA
qbtotal<-merge(qbtotal, profiles, by=c("Player", "Position"), all.x = TRUE)

dltotal<-merge(dltotal, profiles, by=c("Player", "Position"), all.x = TRUE)

dbtotal<-merge(dbtotal, profiles, by=c("Player", "Position"), all.x = TRUE)

lbtotal<-merge(lbtotal, profiles, by=c("Player", "Position"), all.x = TRUE)

ptotal<-merge(ptotal, profiles, by=c("Player", "Position"), all.x = TRUE)

rbtotal<-merge(rbtotal, profiles, by=c("Player", "Position"), all.x = TRUE)

tetotal<-merge(tetotal, profiles, by=c("Player","Position"), all.x = TRUE)

wrtotal<-merge(wrtotal, profiles, by=c("Player", "Position"), all.x = TRUE)

ktotal<-merge(ktotal, profiles, by=c("Player","Position"), all.x = TRUE)

oltotal<-merge(oltotal, profiles, by=c("Player","Position"), all.x = TRUE)

#ADD FRANCHISE DATA#
totaltotal<-rbind(qbtotal, dltotal, dbtotal, lbtotal, rbtotal, tetotal, wrtotal, ktotal, ptotal, oltotal)

franchiseplayers<-c("Kirk Cousins", "Drew Brees", "Michael Vick", "Peyton Manning", "Randy Starks","Michael Jonhson", "Henry Melton",
                    "Greg Hardy","Jason Pierre-Paul", "Muhammad Wilkerson", "Calais Campbell","Cliff Avril", "Haloti Ngata", "Paul Soliai", 
                    "Jairus Byrd", "Eric Berry", "Josh Norman", "Trumaine Johnson", "Brent Grimes", "Tyvon Branch", "Dashon Goldson",
                    "Jason Worilds", "Anthony Spencer", "Justin Houston", "Von Miller", "Brian Orakpo", "Anthony Spencer", "David Harris", 
                    "Tamba Hali", "Chad Greenway", "Kamerion Wimbley", "Pat McAfee", "Steve Weatherford", "Ray Rice", "Matt Forte", "Jimmy Graham", 
                    "Charles Clay", "Fred Davis", "Marcedes Lewis", "Demaryius Thomas", "Dez Bryant", "Alshon Jeffery" ,"Dwayne Bowe", "Wes Welker", 
                    "Vincent Jackson", "Nick Folk", "Stephen Gostkowski", "Justin Tucker", "Mike Nugent", "Phil Dawson", "Phil Dawson", "Josh Scobee", 
                    "Connor Barth", "Branden Albert", "Ryan Clady", "Alex Mack", "Cordy Glenn", "Logan Mankins", "Ryan Kalil")
franchiseplayers<-as.data.frame(franchiseplayers)
colnames(franchiseplayers)[1]<-"Player"

franchiseplayers$Year<-c(2016, 2012, 2011, 2011, 2013, 2013, 2013, 2014, 2015, 2016, 2012, 2012, 2011, 2011, 2013, 2016, 2016, 2016, 2012, 2012,
                         2012, 2014, 2013, 2014, 2016, 2014, 2012, 2011, 2011, 2011, 2013, 2013, 2012, 2012, 2012, 2014, 2015, 2012, 2011, 2015, 
                         2015, 2016, 2012, 2012, 2011, 2014, 2015, 2016, 2012, 2012, 2011, 2012, 2012, 2013, 2013, 2014, 2016, 2011, 2011)

Tag<-c("NEFranchiseTag", "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "FranchiseTag", "FranchiseTag",
       "FranchiseTag", "FranchiseTag", "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "NEFranchiseTag", "NEFranchiseTag",
       "NEFranchiseTag", "NEFranchiseTag", "NEFranchiseTag", "FranchiseTagRescinded", "NEFranchiseTag", "FranchiseTag", "NEFranchiseTagDeclined",
       "NEFranchiseTag", "TransitionTag", "FranchiseTag", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "NEFranchiseTag", "FranchiseTag", 
       "FranchiseTagRescinded", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "FranchiseTagRescinded", "FranchiseTag", "FranchiseTagRescinded", 
       "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "TransitionTag", "FranchiseTag", "NEFranchiseTagDeclined", 
       "NEFranchiseTagDeclined", "FranchiseTagRescinded", "NEFranchiseTagDeclined", "FranchiseTag", "NEFranchiseTag", "FranchiseTag", "FranchiseTagRescinded", 
       "NEFranchiseTagDeclined", "NEFranchiseTagDeclined" ,"FranchiseTag", "FranchiseTag", "FranchiseTagRescinded", "NEFranchiseTagDeclined", 
       "NEFranchiseTagDeclined", "NEFranchiseTag", "FranchiseTag", "TransitionTag", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "NEFranchiseTagDeclined")

franchiseplayers<-cbind(franchiseplayers, Tag)

totaltotal<-merge(totaltotal, franchiseplayers, by=c("Player", "Year"), all.x = TRUE)

totaltotal$FranchiseTag<-0
totaltotal$TransitionTag<-0
totaltotal$NEFranchiseTag<-0
totaltotal$NEFranchiseTagDeclined<-0
totaltotal$FranchiseTagRescinded<-0
totaltotal$AnyTag<-0

totaltotal$Tag<-as.character(totaltotal$Tag)
totaltotal$Tag<-ifelse(is.na(totaltotal$Tag), "0", totaltotal$Tag)

totaltotal$FranchiseTag<-ifelse(totaltotal$Tag=="FranchiseTag", 1, 0)
totaltotal$NEFranchiseTag<-ifelse(totaltotal$Tag=="NEFranchiseTag", 1, 0)
totaltotal$FranchiseTagRescinded<-ifelse(totaltotal$Tag=="FranchiseTagRescinded", 1, 0)
totaltotal$NEFranchiseTagDeclined<-ifelse(totaltotal$Tag=="NEFranchiseTagDeclined", 1, 0)
totaltotal$TransitionTag<-ifelse(totaltotal$Tag=="TransitionTag", 1, 0)
totaltotal$AnyTag<-ifelse((totaltotal$FranchiseTag==1 | totaltotal$NEFranchiseTag==1 | totaltotal$FranchiseTagRescinded | 
                             totaltotal$NEFranchiseTagDeclined==1 | totaltotal$TransitionTag==1), 1, 0)

#ADD FRANCHISE INFO
franchisedata$Franchise.Tag.Price<-gsub(',', '' , franchisedata$Franchise.Tag.Price)
franchisedata$Franchise.Tag.Price<-gsub('\\$', '', franchisedata$Franchise.Tag.Price)

qbtotalFT<-subset(totaltotal, totaltotal$Position=="QB")
qbtotalFT$FranchisePosition<-"QB"

detotalFT<-subset(totaltotal, totaltotal$Position=="DE")
detotalFT$FranchisePosition<-"DE"

cbtotalFT<-subset(totaltotal, totaltotal$Position=="CB")
cbtotalFT$FranchisePosition<-"CB"

oltotalFT<-subset(totaltotal, totaltotal$Position=="LT" | totaltotal$Position=="RT" | totaltotal$Position=="C" | totaltotal$Position=="G")
oltotalFT$FranchisePosition<-"OL"

dttotalFT<-subset(totaltotal, totaltotal$Position=="DT")
dttotalFT$FranchisePosition<-"DT"

sttotalFT<-subset(totaltotal, totaltotal$Position=="P" | totaltotal$Position=="K")
sttotalFT$FranchisePosition<-"ST"

rbtotalFT<-subset(totaltotal, totaltotal$Position=="RB" | totaltotal$Position=="FB")
rbtotalFT$FranchisePosition<-"RB"

stotalFT<-subset(totaltotal, totaltotal$Position=="S" | totaltotal$Position=="SS" | totaltotal$Position=="FS")
stotalFT$FranchisePosition<-"S"

tetotalFT<-subset(totaltotal, totaltotal$Position=="TE")
tetotalFT$FranchisePosition<-"TE"

wrtotalFT<-subset(totaltotal, totaltotal$Position=="WR")
wrtotalFT$FranchisePosition<-"WR"

lbtotalFT<-subset(totaltotal, totaltotal$Position=="LB" | totaltotal$Position=="OLB" | totaltotal$Position=="ILB")
lbtotalFT$FranchisePosition<-"LB"

qbtotal<-merge(qbtotalFT, franchisedata, by=c("FranchisePosition", "Year"))
dttotal<-merge(dttotalFT, franchisedata, by=c("FranchisePosition", "Year"))
detotal<-merge(detotalFT, franchisedata, by=c("FranchisePosition", "Year"))
cbtotal<-merge(cbtotalFT, franchisedata, by=c("FranchisePosition", "Year"))
oltotal<-merge(oltotalFT, franchisedata, by=c("FranchisePosition", "Year"))
sttotal<-merge(sttotalFT, franchisedata, by=c("FranchisePosition", "Year"))
rbtotal<-merge(rbtotalFT, franchisedata, by=c("FranchisePosition", "Year"))
stotal<-merge(stotalFT, franchisedata, by=c("FranchisePosition", "Year"))
tetotal<-merge(tetotalFT, franchisedata, by=c("FranchisePosition", "Year"))
wrtotal<-merge(wrtotalFT, franchisedata, by=c("FranchisePosition", "Year"))
lbtotal<-merge(lbtotalFT, franchisedata, by=c("FranchisePosition", "Year"))


##ADD WIN-LOSS DATA##
winlossdata$Team<-gsub("\\*", "", winlossdata$Team)
winlossdata$Team<-gsub("\\+", "", winlossdata$Team)
winlossdata$Team<-ifelse(winlossdata$Team=="St. Louis Rams", "Los Angeles Rams", winlossdata$Team)
winlossdata<-winlossdata[with(winlossdata, order(Year, Team)),]

WLTeams<-winlossdata$Team[1:32]

for (eachteam in WLTeams){
  for (eachobs in winlossdata$Team){
    if (eachobs==eachteam){
      shrtid<-which((WLTeams)==eachteam)
      obserid<-which((winlossdata$Team)==eachteam)
      winlossdata$abbrev<-teamabbrev[shrtid]
    }
  }
}

for (eachteam in WLTeams){
  for (eachobs in winlossdata$Team){
    if (eachobs==eachteam){
      shrtid<-which((WLTeams)==eachteam)
      obserid<-which((winlossdata$Team)==eachteam)
      winlossdata$abbrev[obserid]<-teamabbrev[shrtid]
    }
  }
}

winlossdata$Team<-winlossdata$abbrev
winlossdata<-subset(winlossdata, select=-c(abbrev))

qbtotal<-merge(qbtotal, winlossdata, by=c("Team", "Year"))
dttotal<-merge(dttotal, winlossdata, by=c("Team", "Year"))
detotal<-merge(detotal, winlossdata, by=c("Team", "Year"))
cbtotal<-merge(cbtotal, winlossdata, by=c("Team", "Year"))
oltotal<-merge(oltotal, winlossdata, by=c("Team", "Year"))
sttotal<-merge(sttotal, winlossdata, by=c("Team", "Year"))
rbtotal<-merge(rbtotal, winlossdata, by=c("Team", "Year"))
stotal<-merge(stotal, winlossdata, by=c("Team", "Year"))
tetotal<-merge(tetotal, winlossdata, by=c("Team", "Year"))
wrtotal<-merge(wrtotal, winlossdata, by=c("Team", "Year"))
lbtotal<-merge(lbtotal, winlossdata, by=c("Team", "Year"))

#ADD FANTASY AGG DATA

qbtotal<-merge(qbtotal, fantasyscoresagg, by=c("Team", "Year"))
dttotal<-merge(dttotal, fantasyscoresagg, by=c("Team", "Year"))
detotal<-merge(detotal, fantasyscoresagg, by=c("Team", "Year"))
cbtotal<-merge(cbtotal, fantasyscoresagg, by=c("Team", "Year"))
oltotal<-merge(oltotal, fantasyscoresagg, by=c("Team", "Year"))
sttotal<-merge(sttotal, fantasyscoresagg, by=c("Team", "Year"))
rbtotal<-merge(rbtotal, fantasyscoresagg, by=c("Team", "Year"))
stotal<-merge(stotal, fantasyscoresagg, by=c("Team", "Year"))
tetotal<-merge(tetotal, fantasyscoresagg, by=c("Team", "Year"))
wrtotal<-merge(wrtotal, fantasyscoresagg, by=c("Team", "Year"))
lbtotal<-merge(lbtotal, fantasyscoresagg, by=c("Team", "Year"))

#ONE COMBO#
fullcombined<-rbind(qbtotal, dttotal, detotal, cbtotal, oltotal, sttotal, rbtotal, stotal, tetotal, wrtotal, lbtotal)

#ADD TEAM CAP SAMPLES#
fullcombined<-merge(fullcombined, teamcapsamples, by=c("Team", "Year"), all.x=TRUE)

#DROP UNUSED VARIABLES#
fullcombined<-subset(fullcombined, select = -c(Team.x, Position.x, team, Position.y, Position.x.1, Position.y.1,
                                               Team.y, X.1, X, death_date, current_team, height, hof_induction_year, player_id, current_salary))

#KEEP ONLY UNIQUE ENTRIES#
fullcombined<-unique(fullcombined)

#EXPORT DATASETS
write.csv(qbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/quarterbacks.csv")
write.csv(dttotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/defensive tackles.csv")
write.csv(detotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/defensive ends.csv")
write.csv(cbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/cornerbacks.csv")
write.csv(oltotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/offensive line.csv")
write.csv(sttotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/special teams.csv")
write.csv(rbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/running backs.csv")
write.csv(wrtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/wide receivers.csv")
write.csv(stotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/safeties.csv")
write.csv(tetotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/tight ends.csv")
write.csv(lbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/linebackers.csv")
write.csv(fullcombined, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/fullcombined.csv", row.names=FALSE)