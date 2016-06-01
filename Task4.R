PopAnual = read.csv('WPP2015_DB02_Populations_Annual.csv')
library(dplyr,magrittr)
library(ggplot2)
# 1.Graph
# Regional % change, 2015-2050 forecast
# Regions Africa, Oceania, Latin America&Caribbean, North America, Asia, Europe
# lying Bar Chart 

PopAnual%>%
  select(Location,Time,PopTotal,VarID)%>%
  filter(Time==2015|Time==2050)%>%
  filter(Location=="Africa"|Location=="Oceania"|Location=="Latin America and the Caribbean"|Location=="Northern America"|Location=="Europe"|Location=="Asia")%>%
  filter(VarID==2)%>%
  group_by(Location)%>%
  mutate(growthRate=round((diff(PopTotal)/PopTotal)*100),3)%>%
  ungroup()%>%
  filter(Time==2015)%>%
  select(Location,growthRate)%>%
  arrange(desc(growthRate))%>%
  ggplot(.,aes(x=reorder(Location,growthRate),growthRate))+
  geom_bar(stat="identity")+
  geom_text(aes(x=Location,y=growthRate,label=growthRate,hjust = 0))+
  geom_hline(yintercept = 0,color="black")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio=.7)+
  #scale_y_discrete(labels=c("Africa","Oceania","Latin America & \n the Caribbean", "Northern America","Europe","Asia"))+
  labs(title="Regional % change, 2015-2050",x="",y="")-> graph1
graph1

#2.Graph

TotPop = function(groupTime)
{
  groupTime%>%
    summarise(TotPop=(sum(PopTotal))/10e5)%>%
    round(.,2)->totalPop
  return(totalPop[,"TotPop"])
}
PopAnual%>%
  select(Location,Time,PopTotal,VarID)%>%
  filter(Time==2015|Time==2050|Time==2099)%>%
  filter(Location=="Africa"|Location=="Oceania"|Location=="Latin America and the Caribbean"|Location=="Northern America"|Location=="Europe"|Location=="Asia")%>%
  filter(VarID==2)%>%
  group_by(Time)%>%
  ggplot(.,aes(reorder(Time,-PopTotal),PopTotal,fill=Location))+
  geom_bar(stat="identity",position = "stack",width = 0.5)+
  geom_text(aes(as.factor(Time),TotPop(.)*10e5,label=TotPop(.),hjust=-0))+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust=0.05),
        aspect.ratio=.7)+
  labs(title="Total Population, bn",x="",y="")-> graph2
graph2

#3.Graph

PopAnual%>%
  select(Time,PopTotal,VarID,Location,LocID)%>%
  filter(Time==1950|Time==2015|Time==2050)%>%
  filter(LocID<900)%>%
  filter(VarID==2)%>%
  group_by(Time)%>%
  arrange(desc(PopTotal))%>%
  top_n(12,PopTotal)%>%
  ggplot(.,aes(Location,PopTotal))+
  geom_bar(stat="identity")+
  facet_grid(.~Time)+
  coord_flip()->graph3
graph3
