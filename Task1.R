 
# TODO
# 
 
library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)

BundesligaDaten = read.csv('D1.csv')

EvalPoints <- function(FTR,Position)
{ 
  if (Position=="AwayTeam") {if (FTR==strsplit(as.character(Position),"wayTeam")) { res = 3} else {res = 0}}
  else{if (FTR==strsplit(as.character(Position),"omeTeam")){res = 3} else {res = 0}}
  
  if (FTR== "D"){res=1}
  return(res)
}

BundesligaDaten%>%
  melt(measure.vars=c("HomeTeam", "AwayTeam"), value.name="Team", variable.name="Position")%>%
  select(Date,Team,Position,FTR)%>%
  arrange(-desc(Team))%>%
  mutate(DayOfPlay=rep(seq(1,34,1),18), Points = mapply(EvalPoints,.$FTR,.$Position))%>%
  group_by(Team)%>%
  mutate(CumPoints = cumsum(Points))%>%
  ggplot(.,aes(DayOfPlay,CumPoints,color=Team))+
  geom_line()+
  scale_color_manual(values=c(#BA3733,#ED1248,#014E9E,#FDE100,#CA0000,#ED1C23,#0A3F86,#179D33,#005CA9,#00559E,#D71920,#E32221,#2A6601,#FE0000,#2167AC,#D9504E,#137B38,#4C9C00))-> TablePos
TablePos
