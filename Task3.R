
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
  geom_point(aes(color=continent,size=pop))+
  scale_color_manual(values = c("blue","yellow","red","brown","green"))+
  scale_size_continuous(range = c(2,20),limits = c(0,10e+07),breaks=seq(0,10e+07,10e+06))+
  scale_x_log10(breaks=c(400,4000,40000))+
  scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
  xlab("income")+
  ylab("lifespan")-> graph
graph



gapminder%>%
  filter(year==1952)%>%
  filter(gdpPercap>30000) -> Data

Data
