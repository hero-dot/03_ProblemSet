PopAnual = read.csv('WPP2015_DB02_Populations_Annual.csv')
library(dplyr,magrittr,ggplot2)
library(reshape2)

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
  geom_bar(stat="identity",width = 0.5)+
  geom_text(aes(x=Location,y=growthRate,label=growthRate,hjust = -0.5))+
  coord_flip()-> finalData
finalData
