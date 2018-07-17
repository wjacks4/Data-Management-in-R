library(xlsx)
library(tidyverse)

positioncap<- read.xlsx("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Player Earnings by type.xlsm", 2)

positioncap<-positioncap[order(positioncap$TEAM,positioncap$YEAR),]

dates<-word(string = positioncap$Dates, start = 1, end = 3, sep=" ")
dates<-as.matrix(dates)

dates<-gsub("JAN","01",dates)
dates<-gsub("FEB","02",dates)
dates<-gsub("MAR","03",dates)
dates<-gsub("APR","04",dates)
dates<-gsub("MAY","05",dates)
dates<-gsub("JUN","06",dates)
dates<-gsub("JUL","07",dates)
dates<-gsub("AUG","08",dates)
dates<-gsub("SEP","09",dates)
dates<-gsub("OCT","10",dates)
dates<-gsub("NOV","11",dates)
dates<-gsub("DEC","12",dates)

dates<-gsub("[A-z]","",dates)

length1<-sapply(gregexpr("\\W+",positioncap$Dates), length)
length1<-as.matrix(length-1)

actions<-word(string = positioncap$Dates, start = 3, end = (length1-1), sep=" ")
actions<-as.matrix(actions)


lengthchar<-nchar(actions, type = "chars", allowNA = FALSE)
lengthchar<-as.matrix(lengthchar)
actions<-word(string = actions, start=5, end=(lengthchar+1), sep = "")

actions<-as.matrix(actions)

positioncap<-cbind(positioncap,dates,actions)

write.csv(positioncap,"C:/Users/whjac/Downloads/Term Paper Data/Contract Information/CLEANEDPunterEarnings2.csv")