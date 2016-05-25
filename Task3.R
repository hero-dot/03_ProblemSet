
install.packages("gapminder")
library(gapminder)
library(dplyr)
library(magrittr)
library(ggplot2)

gapminderLocal <- gapminder

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

gapminderLocal%>%
  group_by(continent)%>%
  ggplot(.,aes(x=gdpPercap,y=lifeExp))+
  geom_point(aes(color=continent,size=pop))+
  scale_color_manual(values = c("blue","yellow","red","brown","green"))+
  scale_size_continuous(range = c(2,10),limits = c(0,10e+07),breaks=seq(0,10e+07,10e+06))+
  scale_x_log10(breaks=c(400,4000,40000))+
  scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
  xlab("income")+
  ylab("lifespan")+
  facet_wrap(~year)-> graph
graph


#b.

graph3b <- function(x){
    gapminderLocal%>%
      filter(year==x)%>%
      group_by(continent)%>%
      ggplot(.,aes(x=gdpPercap,y=lifeExp))+
      geom_point(aes(color=continent,size=pop))+
      scale_color_manual(values = c("blue","yellow","red","brown","green"))+
      scale_size_continuous(range = c(2,20),limits = c(0,10e+07),breaks=seq(0,10e+07,10e+06))+
      scale_x_log10(breaks=c(400,4000,40000))+
      scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
      xlab("income")+
      ylab("lifespan")
}


for (year in gapminderLocal$year) {
  graph3b(year)
}




years = 0
for (years in gapminder$year) {
  gapminder%>%
    filter(year==years)%>%
    group_by(continent)%>%
    ggplot(.,aes(x=gdpPercap,y=lifeExp))+
    geom_point(aes(color=continent,size=pop))+
    scale_color_manual(values = c("blue","yellow","red","brown","green"))+
    scale_size_continuous(range = c(2,20),limits = c(0,10e+07),breaks=seq(0,10e+07,10e+06))+
    scale_x_log10(breaks=c(400,4000,40000))+
    scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
    xlab("income")+
    ylab("lifespan")

}
x11()
