library(xlsx)
library(tidyverse)
library(plyr)

totalcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcaphitscombinedPLUSPLUS.csv", stringsAsFactors = FALSE)
teamcapscombined<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CleanedCapStatsPLUS.csv", stringsAsFactors = FALSE)
injurydata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Injury Data/FINAL FULL.csv")
snapdata<-read.csv("file:///C:/Users/whjac/Downloads/Term Paper Data/Snap Data/Combined.csv", stringsAsFactors = FALSE)
contractsyears<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Yearly Contract Information.csv", stringsAsFactors = FALSE)
profiles<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Kernel Data/profilescleaned.csv", stringsAsFactors = FALSE)
aggcaphits<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/aggregatecaphitsPLUS.csv")
franchisedata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/BASIC DATA/Franchise Tag Costs.csv")
teamcapsamples<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/totalcapsamplesPLUS.csv")
winlossdata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/BASIC DATA/Win Loss Data.csv", stringsAsFactors = FALSE)
fantasyscores<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Individual Player Weekly Stats/Combined/FULLCOMBINED+avg.csv", stringsAsFactors  = FALSE)
fantasyscoresagg<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Team Aggregate Scores/Fantasy Aggregate Scores.csv", stringsAsFactors= FALSE)
olfantasyscores<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Punter + OL Fantasy Scores/OL.csv", stringsAsFactors= FALSE)
pfantasyscores<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Yearly Statistical Output/Punter + OL Fantasy Scores/P.csv", stringsAsFactors= FALSE)
probowldata<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Pro Bowl Rosters/Combined.csv", stringsAsFactors = FALSE)
winlines<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Betting Line Data/Preseason Win Total Lines.csv", stringsAsFactors = FALSE)
experience<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/ExperienceData/Combined/FOXcombined.csv", stringsAsFactors=FALSE)

probowldata<-subset(probowldata, select=-Experience)

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

totaloutput<-rbind.fill(qboutput, dloutput, dboutput, lboutput, wroutput, rboutput, oloutput, teoutput, poutput, koutput)

totaloutput$Player<-word(string=totaloutput$Player, start=1, end=2, sep=" ")

totaloutput<-subset(totaloutput, select=-Experience)
#CHANGE LAC TO SD & STL TO LAR#
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

oloutput<-subset(oloutput, select=-Experience)

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
totalcaphits$Team<-ifelse(totalcaphits$Team=="STL", "LAR", totalcaphits$Team)


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

totaloutputcap<-rbind.fill(qboutputcap, dboutputcap, dloutputcap, lboutputcap, poutputcap, rboutputcap, teoutputcap, wroutputcap, koutputcap, oloutputcap)

#PRO BOWL MERGE#
probowldata$Player<-gsub("\\+", "", probowldata$Player)
probowldata$Player<-gsub("\\%", "", probowldata$Player)
probowldata$Player<-gsub("Matt Slater", "Matthew Slater", probowldata$Player)
probowldata$Probowl<-1
probowlmerge<-subset(probowldata, select=c(Player, Team, Year, Probowl))
totaloutputcap<-merge(totaloutputcap, probowlmerge, by=c("Player", "Team", "Year"), all.x = TRUE)
totaloutputcap$Probowl<-ifelse(is.na(totaloutputcap$Probowl), 0, totaloutputcap$Probowl)

#FIX VARIABLE NAMES FOR FANTASY MERGE#

fantasyscores$Position<-toupper(fantasyscores$Position)

totaloutputcap$Position<-totaloutputcap$Position.x

#DROP DEAD CAP#
deadoutputcap<-subset(totaloutputcap, totaloutputcap$Deadcap==1)
totaloutputcap<-subset(totaloutputcap, totaloutputcap$Deadcap==0)

#CHANGE POSITIONS FOR FANTASY MERGE#
fantasyscores$FantasyMergePosition<-fantasyscores$Position
olfantasyscores$FantasyMergePosition<-"OL"
pfantasyscores$FantasyMergePosition<-"P"

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

##CLEAN INJURY & SNAP DATA & COMBINE
snapdata$Team<-ifelse(snapdata$Team=="STL", "LAR", snapdata$Team)
snapdata$Team<-ifelse(snapdata$Team=="JAC", "JAX", snapdata$Team)

snapdata$lastname<-gsub("\\.", "", snapdata$lastname)

qbsnapdata<-snapdata[which(snapdata$Position=="QB"),]
qbsnapdata<-subset(qbsnapdata, select=-c(Position))
qboutputcap<-merge(qboutputcap, qbsnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
qbmerge<-subset(qboutputcap, select=c(Player, Year, Team, Position))
qbmerge<-unique(qbmerge)
qbinjuries<-merge(injurydata, qbmerge, by=c("Player", "Year", "Team"))
qbtotal<-merge(qboutputcap, qbinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

dlsnapdata<-snapdata[which(snapdata$Position=="DL"),]
dlsnapdata<-subset(dlsnapdata, select=-c(Position))
dloutputcap<-merge(dloutputcap, dlsnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
dlmerge<-subset(dloutputcap, select=c(Player, Year, Team, Position))
dlmerge<-unique(dlmerge)
dlinjuries<-merge(injurydata, dlmerge, by=c("Player", "Year", "Team"))
dltotal<-merge(dloutputcap, dlinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

dbsnapdata<-snapdata[which(snapdata$Position=="DB"),]
dbsnapdata<-subset(dbsnapdata, select=-c(Position))
dboutputcap<-merge(dboutputcap, dbsnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
dbmerge<-subset(dboutputcap, select=c(Player, Year, Team, Position))
dbmerge<-unique(dbmerge)
dbinjuries<-merge(injurydata, dbmerge, by=c("Player", "Year", "Team"))
dbtotal<-merge(dboutputcap, dbinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

lbsnapdata<-snapdata[which(snapdata$Position=="LB"),]
lbsnapdata<-subset(lbsnapdata, select=-c(Position))
lboutputcap<-merge(lboutputcap, lbsnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
lbmerge<-subset(lboutputcap, select=c(Player, Year, Team, Position))
lbmerge<-unique(lbmerge)
lbinjuries<-merge(injurydata, lbmerge, by=c("Player", "Year", "Team"))
lbtotal<-merge(lboutputcap, lbinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

psnapdata<-snapdata[which(snapdata$Position=="ST"),]
psnapdata<-subset(psnapdata, select=-c(Position))
poutputcap<-merge(poutputcap, psnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
pmerge<-subset(poutputcap, select=c(Player, Year, Team, Position))
pmerge<-unique(pmerge)
pinjuries<-merge(injurydata, pmerge, by=c("Player", "Year", "Team"))
ptotal<-merge(poutputcap, pinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

rbsnapdata<-snapdata[which(snapdata$Position=="RB"),]
rbsnapdata<-subset(rbsnapdata, select=-c(Position))
rboutputcap<-merge(rboutputcap, rbsnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
rbmerge<-subset(rboutputcap, select=c(Player, Year, Team, Position))
rbmerge<-unique(rbmerge)
rbinjuries<-merge(injurydata, rbmerge, by=c("Player", "Year", "Team"))
rbtotal<-merge(rboutputcap, rbinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

tesnapdata<-snapdata[which(snapdata$Position=="TE"),]
tesnapdata<-subset(tesnapdata, select=-c(Position))
teoutputcap<-merge(teoutputcap, tesnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
temerge<-subset(teoutputcap, select=c(Player, Year, Team, Position))
temerge<-unique(temerge)
teinjuries<-merge(injurydata, temerge, by=c("Player", "Year", "Team"))
tetotal<-merge(teoutputcap, teinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

wrsnapdata<-snapdata[which(snapdata$Position=="WR"),]
wrsnapdata<-subset(wrsnapdata, select=-c(Position))
wroutputcap<-merge(wroutputcap, wrsnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
wrmerge<-subset(wroutputcap, select=c(Player, Year, Team, Position))
wrmerge<-unique(wrmerge)
wrinjuries<-merge(injurydata, wrmerge, by=c("Player", "Year", "Team"))
wrtotal<-merge(wroutputcap, wrinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

ksnapdata<-snapdata[which(snapdata$Position=="ST"),]
ksnapdata<-subset(ksnapdata, select=-c(Position))
koutputcap<-merge(koutputcap, ksnapdata, by=c("lastname", "Team", "Year"),   all.x = TRUE)
kmerge<-subset(koutputcap, select=c(Player, Year, Team, Position))
kmerge<-unique(kmerge)
kinjuries<-merge(injurydata, kmerge, by=c("Player", "Year", "Team"))
ktotal<-merge(koutputcap, kinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

olsnapdata<-snapdata[which(snapdata$Position=="OL"),]
olsnapdata<-subset(olsnapdata, select=-c(Position))
oloutputcap<-merge(oloutputcap, olsnapdata, by=c("lastname", "Team", "Year"), all.x=TRUE)
olmerge<-subset(oloutputcap, select=c(Player, Year, Team, Position))
olmerge<-unique(olmerge)
olinjuries<-merge(injurydata, olmerge, by=c("Player", "Year", "Team"))
oltotal<-merge(oloutputcap, olinjuries, by=c("Player", "Year", "Week", "Team", "Position"), all.x = TRUE)

qbtotal$FantasyMergePosition<-ifelse(is.na(qbtotal$FantasyMergePosition), "QB", qbtotal$FantasyMergePosition)
dltotal$FantasyMergePosition<-ifelse(is.na(dltotal$FantasyMergePosition), "DL", dltotal$FantasyMergePosition)
dbtotal$FantasyMergePosition<-ifelse(is.na(dbtotal$FantasyMergePosition),  "DB", dbtotal$FantasyMergePosition)
lbtotal$FantasyMergePosition<-ifelse(is.na(lbtotal$FantasyMergePosition),  "LB", lbtotal$FantasyMergePosition)
ptotal$FantasyMergePosition<-ifelse(is.na(ptotal$FantasyMergePosition),  "P", ptotal$FantasyMergePosition)
rbtotal$FantasyMergePosition<-ifelse(is.na(rbtotal$FantasyMergePosition),  "RB", rbtotal$FantasyMergePosition)
tetotal$FantasyMergePosition<-ifelse(is.na(tetotal$FantasyMergePosition),  "TE", tetotal$FantasyMergePosition)
wrtotal$FantasyMergePosition<-ifelse(is.na(wrtotal$FantasyMergePosition),  "WR", wrtotal$FantasyMergePosition)
ktotal$FantasyMergePosition<-ifelse(is.na(ktotal$FantasyMergePosition),  "K", ktotal$FantasyMergePosition)
oltotal$FantasyMergePosition<-ifelse(is.na(oltotal$FantasyMergePosition),   "OL", oltotal$FantasyMergePosition)


#FANTASY MERGE - ALL POSITION EXCEPT P & OL#
fantasyscoresbyweek<-subset(fantasyscores, select=-c(Position, seasontotal))
fantasyscoresbyseason<-subset(fantasyscores, select=c(Player, Year, Team, FantasyMergePosition, seasontotal))

qboutputcapF<-merge(qbtotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

dloutputcapF<-merge(dltotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

dboutputcapF<-merge(dbtotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

lboutputcapF<-merge(lbtotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

rboutputcapF<-merge(rbtotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

teoutputcapF<-merge(tetotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

wroutputcapF<-merge(wrtotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)

koutputcapF<-merge(ktotal, fantasyscoresbyweek, by=c("Player", "Year", "FantasyMergePosition", "Week"), all.x=TRUE)


qboutputcapF<-merge(qboutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

dloutputcapF<-merge(dloutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

dboutputcapF<-merge(dboutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

lboutputcapF<-merge(lboutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

rboutputcapF<-merge(rboutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

teoutputcapF<-merge(teoutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

wroutputcapF<-merge(wroutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

koutputcapF<-merge(koutputcapF, fantasyscoresbyseason, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)

#FANTASY MERGE - OL & P#
olfantasyscores<-subset(olfantasyscores, select=-c(Position, Week))
oloutputcapF<-merge(oltotal, olfantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)
oloutputcapF$Team<-NA

pfantasyscores<-subset(pfantasyscores, select=-c(Position, Week))
pfantasyscores$Player<-word(string=pfantasyscores$Player, start=1, end=2, sep=" ")
pfantasyscores$Player<-gsub(",", "", pfantasyscores$Player)
poutputcapF<-merge(ptotal, pfantasyscores, by=c("Player", "Year", "FantasyMergePosition"), all.x=TRUE)
poutputcapF$Team<-NA

#COMBINE ALL DATASETS TO DO CLEANING# 
totaloutputcapF<-rbind(qboutputcapF, dloutputcapF, dboutputcapF, lboutputcapF, poutputcapF, rboutputcapF,
                       teoutputcapF, wroutputcapF, koutputcapF, oloutputcapF)

totaloutputcapF$Team<-totaloutputcapF$Team.x

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

qbtotal<-qboutputcapF
dltotal<-dloutputcapF
dbtotal<-dboutputcapF
lbtotal<-lboutputcapF
ptotal<-poutputcapF
rbtotal<-rboutputcapF
tetotal<-teoutputcapF
wrtotal<-wroutputcapF
ktotal<-koutputcapF
oltotal<-oloutputcapF

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

franchiseplayers<-c("Kirk Cousins", "Drew Brees", "Michael Vick", "Peyton Manning", "Randy Starks","Michael Johnson", "Henry Melton",
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
                         2012, 2014, 2013, 2014, 2016, 2014, 2012, 2011, 2011, 2011, 2011, 2013, 2012, 2012, 2012, 2014, 2015, 2012, 2011, 2015, 
                         2015, 2016, 2012, 2012, 2011, 2014, 2015, 2016, 2012, 2012, 2011, 2012, 2012, 2013, 2013, 2014, 2016, 2011, 2011)

Tag<-c("NEFranchiseTag", "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "FranchiseTag", "FranchiseTag",
                        "FranchiseTag", "FranchiseTag", "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "NEFranchiseTag", "NEFranchiseTag",
                        "NEFranchiseTag", "NEFranchiseTag", "NEFranchiseTag", "FranchiseTagRescinded", "NEFranchiseTag", "FranchiseTag", "NEFranchiseTagDeclined",
                        "NEFranchiseTag", "TransitionTag", "FranchiseTag", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "NEFranchiseTag", "FranchiseTag", 
                        "FranchiseTagRescinded", "NEFranchiseTagDeclined" ,"FranchiseTagRescinded", "FranchiseTagRescinded", "FranchiseTag", "FranchiseTagRescinded", 
                        "NEFranchiseTagDeclined", "NEFranchiseTagDeclined", "FranchiseTagRescinded", "TransitionTag", "FranchiseTag", "NEFranchiseTagDeclined", 
                        "NEFranchiseTagDeclined", "FranchiseTagRescinded", "NEFranchiseTagDeclined", "FranchiseTag", "NEFranchiseTag", "FranchiseTag", "FranchiseTagRescinded", 
                        "NEFranchiseTagDeclined", "NEFranchiseTagDeclined" ,"FranchiseTag", "FranchiseTag", "FranchiseTag", "NEFranchiseTagDeclined", 
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

#ADD TEAM WIN LINES#
winlines<-subset(winlines, select=c(Team, Year, Line))
fullcombined<-merge(fullcombined, winlines, by=c("Team", "Year"), all.x=TRUE)

#ADD EXPERIENCE DATA#
exptest<-subset(experience, experience$Player=="Matt Forte")

fullcombined<-merge(fullcombined, experience, by=c("Team", "Year", "FranchisePosition", "Player"), all.x=TRUE)

fulldatav2NW<-subset(fulldatav2, select=-c(Week, Points, Total.Snaps, Injury.listing, Started))

#DROP UNUSED VARIABLES#
fullcombined<-subset(fullcombined, select = -c(Team.x, Position.x, Team.y, Position.y, 
                                               Team.y, X.1, X, death_date, current_team, height, hof_induction_year, player_id, current_salary))

#KEEP ONLY UNIQUE ENTRIES#
fullcombined<-unique(fullcombined)

#EXPORT DATASETS
write.csv(qbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/quarterbacksPLUS.csv")
write.csv(dttotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/defensive tacklesPLUS.csv")
write.csv(detotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/defensive endsPLUS.csv")
write.csv(cbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/cornerbacksPLUS.csv")
write.csv(oltotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/offensive linePLUS.csv")
write.csv(sttotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/special teamsPLUS.csv")
write.csv(rbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/running backsPLUS.csv")
write.csv(wrtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/wide receiversPLUS.csv")
write.csv(stotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/safetiesPLUS.csv")
write.csv(tetotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/tight endsPLUS.csv")
write.csv(wrtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/wide receiversPLUS.csv")
write.csv(lbtotal, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/linebackersPLUS.csv")
write.csv(fullcombined, "C:/Users/whjac/Downloads/Term Paper Data/TOTAL COMBINED DATASETS/fullcombinedPLUS.csv", row.names=FALSE)