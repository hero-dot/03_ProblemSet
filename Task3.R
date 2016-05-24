
install.packages("gapminder")
library(gapminder)
library(dplyr)
library(magrittr)
library(ggplot2)

gapminder <- gapminder

# TODO
# y-Achse lifeExp
# x-Achse gdpPercap
# country ist eine Blase
# Die Farbe der Blase ist der Kontinent
# Der Durchmesser der Blase ist pop
# year ist die 6.te Dimension


#a.
gapminder%>%
  filter(year==1952)%>%
  group_by(continent)%>%
  ggplot(.,aes(x=gdpPercap,y=lifeExp))+
  geom_point()+
  scale_x_log10(breaks=c(400,4000,40000))+
  scale_y_continuous(breaks=c(0,25,50,75,80),labels = c(0,"25 years", "50 years", "75 years",""))+
  xlab("income")+
  ylab("lifespan")-> graph
graph






gapminder%>%
  filter(year==1952)%>%
  filter(gdpPercap>30000) -> Data

Data
