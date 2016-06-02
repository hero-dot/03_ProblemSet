install.packages("ggvis")
install.packages("gapminder")
library(gapminder)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggvis)

gapminderLocal <- gapminder

# TODO
# y-Achse lifeExp
# x-Achse gdpPercap
# country ist eine Blase
# Die Farbe der Blase ist der Kontinent
# Der Durchmesser der Blase ist pop
# year ist die 6.te Dimension


#a.
gapminderLocal%>%
  filter(year==1952)%>%
  group_by(continent)%>%
  ggplot(.,aes(x=gdpPercap,y=lifeExp))+
  geom_point(aes(color=continent,size=pop))+
  scale_color_manual(values = continent_colors)+
  scale_size_continuous(range = c(2,20),limits = c(0,10e+07),breaks=seq(0,10e+07,10e+06))+
  scale_x_log10(breaks=c(400,4000,40000))+
  scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
  xlab("income")+
  ylab("lifespan")-> graph
graph

#b.
gapminderLocal%>%
  group_by(continent)%>%
  ggplot(.,aes(x=gdpPercap,y=lifeExp))+
  geom_point(aes(color=continent,size=pop))+
  scale_color_manual(values = c("cornflowerblue","yellow2","tomato","tan3", "seagreen1"))+
  scale_size_continuous(range = c(2,6))+
  scale_x_log10(breaks=c(400,4000,40000))+
  scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
  xlab("income")+
  ylab("lifespan")+
  facet_wrap(~year)-> graph
graph

#c.

countries = unique(as.character(gapminderLocal$country))
gapminderLocal%>%
  ggvis(~gdpPercap,~lifeExp)%>%
  filter(country==eval(input_select(countries,map=as.character,selected="Afghanistan",label="Country")))%>%
  layer_paths()%>%
  layer_points()%>%
  layer_text(text:=~year, dx := 10, dy := -10)-> graph3c
graph3c

#d. 

#Create a graph of Kuwait, China, Singapore, India
gapminderLocal%>%
  filter(country=="China"|country=="India"|country=="Singapore"|country=="Kuwait")%>%
  ggplot(., aes(gdpPercap, lifeExp,color=country))+geom_point()+
  geom_line()+scale_y_continuous(limits = c(0,85))+scale_color_manual(values = c("black","red","green","blue"))->graph3d
graph3d

# Auffallend ist zunächst, das Indien und China von der Entwicklung der Lebenserwartung und dem pro-Kopf-Einkommen 
# fast identisch sind. Der einzige nennenswerte Unterschied liegt in der von Beginn an höheren Lebenserwartung der 
# Chinesen um ca. 7Jahre. Das Pro-Kopf-Einkommen steigt ebnfalls fast identisch an. Singapur passt von der Entwicklung 
# der durchschnittlichen Lebenserwartung zu den beiden, da sie in den 55 Jahren der Betrachtung ebenfalls um ca. 20 Jahre
# ansteigt(von 60 auf80 Jahre). Das jährliche Pro-Kopf-Einkommen steigt in Singapur im Verhältnis zu Indien und China
# drastisch an, obwohl es sich dort über die gesamte Beobachtungsdauer verzehnfacht hat(auf ~ 4000 bis 5000$.Singapur 
# verzehnfacht sein Einkommen in etwa auch, allerdings von ca. 4000$ auf ca. 47000$. 
# Komplett aus der Reihe fällt Kuwait, da es das einzige der vier Länder ist, bei welchem die Lebenserwartung von 75 Jahren
# auf knapp unter 60 sinkt. Das Pro-Kopf-Einkommen steigt zwar auch "nur" um das vierfache, aber Kuweit startete 1955 auch 
# schon bei knapp 30000$ und steht 2007 bei einem stolzen durchschnittlichen Einkommen von über 110000$. 
 
#e.

continents  = unique(as.character(gapminderLocal$continent))
selected = input_select(continents,label="Continent",selected="Asia",multiple = TRUE)

gapminderLocal%>%
  ggvis(~gdpPercap,~lifeExp, fill=~continent,size=~pop)%>%
  filter(year==eval(input_slider(1952,2007,step=5, label="Year")))%>%
  filter(continent == eval(selected)|continent == eval(selected))%>%
  layer_points()%>%
  add_legend(c("size","fill"))-> graph3e
graph3e

