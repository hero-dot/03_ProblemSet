PopAnual = read.csv('WPP2015_DB02_Populations_Annual.csv')
library(dplyr,magrittr,ggplot2)

# 1.Graph
# Regional % change, 2015-2050 forecast
# Regions Africa, Oceania, Latin America&Caribbean, North America, Asia, Europe
# lying Bar Chart 

PopAnual%>%
  select(Location,Time,PopTotal,VarID)%>%
  filter(Time==2015|Time==2050)%>%
  filter(Location=="Africa"|Location=="Oceania"|Location=="Latin America and the Caribbean"|Location=="Northern America"|Location=="Europe"|Location=="Asia")%>%
  filter(VarID==2)%>%
  mutate(growthRate=())-> finalData
finalData


unique(PopAnual$Location)
