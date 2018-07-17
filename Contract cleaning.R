library(xlsx)
library(tidyverse)
library(plyr)

contracts<-read.csv("C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Contract Signings Combined.csv")

contractstest<-contracts[1:2,]

combined<-contractstest[1,]


i=1
agg=0
for (i in 1:nrow(contracts)){
  numrep<-contracts$Years.completed.under.initial.terms[i]
  #print(numrep)
  contractsout<-contracts[rep(i, each=numrep),]
  combined<-rbind(combined,contractsout)
}

combined$Yearofcontract<-1
combined$Freeagent<-0

for (i in 1:nrow(combined)){
  if (i != nrow(combined)){
    player<-combined$Player[i]
    playernext<-combined$Player[i+1]
    if (player==playernext){
      combined$Year[i+1]<-(combined$Year[i]+1)
      combined$Yearofcontract[i+1]<-(combined$Yearofcontract[i]+1)
    } else if (player!=playernext && combined$Reason.for.early.termination[i]=="Released"){
      combined$Freeagent[i]<-1
    } else if (player!=playernext && combined$Yearofcontract[i]==combined$Length.of.contract..years.[i]){
      combined$Freeagent[i]<-1
    }
  }
}

write.csv(combined, "C:/Users/whjac/Downloads/Term Paper Data/Contract Information/Yearly Contract Information.csv")