 
# TODO
# Y Achsenbeschriftung auf die rechte Seite
# X Achsenbeschriftung oben einfügen
# Darstellung auf Wertebereich beschränken
# Punkte die nicht existieren auf der x Achse in hellerem Grau einfärben
# Zusatzleistung Legende mit dem Logo ersetzen
# x Achsenbeschriftung grau einfärben
# Dynamische Darstellung mit ggvis
 
library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)
install.packages("ggvis")
library(ggvis)

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
  guides(color="none")+
  scale_color_manual(values=c("#BA3733","#ED1248","#014E9E","#FDE100","#CA0000","#ED1C23","#0A3F86","#179D33","#005CA9","#00559E","#D71920","#E32221","#2A6601","#FE0000","#2167AC","#D9504E","#137B38","#4C9C00"))+
  scale_x_continuous(breaks=seq(1,34,1),limits=c(1,34),name="")+
  scale_y_continuous(breaks=seq(0,90,1),limits=c(0,90),name="")+
  theme(panel.grid.major.x= element_line(color = "grey"), axis.ticks.x = element_line(color = "grey"))-> TablePos

TablePos


BundesligaDaten%>%
  melt(measure.vars=c("HomeTeam", "AwayTeam"), value.name="Team", variable.name="Position")%>%
  select(Date,Team,Position,FTR)%>%
  arrange(-desc(Team))%>%
  mutate(DayOfPlay=rep(seq(1,34,1),18), Points = mapply(EvalPoints,.$FTR,.$Position))%>%
  group_by(Team)%>%
  mutate(CumPoints = cumsum(Points))%>%
  ggvis(~DayOfPlay,~CumPoints, stroke=~Team)%>%
  layer_lines()%>%
  scale_nominal("stroke", range = c("#BA3733","#ED1248","#014E9E","#FDE100","#CA0000","#ED1C23","#0A3F86","#179D33","#005CA9","#00559E","#D71920","#E32221","#2A6601","#FE0000","#2167AC","#D9504E","#137B38","#4C9C00"))%>%
  add_axis("y",properties=axis_props(axis=c(stroke ="grey")),values=seq(0,90,1),orient="right")%>%
  add_axis("x",values=seq(1,34,1))->TablePos
TablePos
add_axi