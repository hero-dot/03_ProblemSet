 
# TODO
# Dataframe mit Spieltag, Team, kummulierten Punkten
# Darstellung als Line Plot
# Anfänge und Ausgestaltung siehe remarks
 
library(reshape2)
library(magrittr)
library(dplyr)
library(lubridate)
install.packages("lubridate")

BundesligaDaten = read.csv('D1.csv')

BundesligaDaten%>%
  melt(measure.vars=c("HomeTeam", "AwayTeam"), value.name="Team", variable.name="Position")%>%
  select(Date,Team,Position,FTR)%>%
  arrange(-desc(Team))%>%
  mutate(DayOfPlay=rep(seq(1,34,1),18), Points = mapply(EvalPoints,.$FTR,.$Position))%>%
  group_by(Team)%>%
  mutate(CumPoints = cumsum(Points))-> TablePos

EvalPoints <- function(FTR,Position)
{ 
  if (Position=="AwayTeam") {if (FTR==strsplit(as.character(Position),"wayTeam")) { res = 3} else {res = 0}}
  else{if (FTR==strsplit(as.character(Position),"omeTeam")){res = 3} else {res = 0}}

  if (FTR== "D"){res=1}
  return(res)
}

TablePos%>%
  group_by(Team)%>%
  mutate(cumsum=cumsum(Points)) -> Test
