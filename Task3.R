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


#b.

graph3b <- function(x){
    gapminderLocal%>%
      filter(year==x)%>%
      group_by(continent)%>%
      ggplot(.,aes(x=gdpPercap,y=lifeExp,frame=year))+
      geom_point(aes(color=continent,size=pop))+
      scale_color_manual(values = c("blue","yellow","red","brown","green"))+
      scale_size_continuous(range = c(2,20),limits = c(0,10e+07),breaks=seq(0,10e+07,10e+06))+
      scale_x_log10(breaks=c(400,4000,40000))+
      scale_y_continuous(limits = c(25,80), breaks = seq(25,80,25),labels = c("25   \nyears", "50   \nyears","75   \nyears"))+
      xlab("income")+
      ylab("lifespan")
}


for (year in unique(gapminderLocal$year)) {
  graph3b(year)
}

lapply(unique(gapminderLocal$year), graph3b)

years = 0
for (years in unique(gapminderLocal$year)) {
  gapminderLocal%>%
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

graph <- ggplot(gapminderLocal, aes(gdpPercap, lifeExp, size=pop, color=continent,frame=year))+
  geom_point()+scale_x_log10()
graph
install.packages("devtools")
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
gg_animate(graph)

#c.

graph <- ggplot(gapminderLocal, aes(gdpPercap, lifeExp, color=country))+
  geom_path()+scale_x_log10()+facet_wrap(~continent)+scale_color_manual(values = country_colors)+guides(color=F)
graph

#d. 

#Create a graph of Kuwait, China, Singapore, India

#e.
# Better Visualization with ggvis. Slider to switch the year. Display the past years with a different opacity
gapminderLocal%>%
  ggvis(~gdpPercap,~lifeExp, fill=~factor(continent))%>%
  filter(year==eval(input_slider(1952,2007,step=5)))%>%
  layer_points(size=~pop)%>%
  group_by(continent)%>%
<<<<<<< HEAD
  layer_model_predictions(stroke=~continent, model = "lm")-> graph3b
=======
  layer_model_predictions(stroke=~continent,model="lm")-> graph3b
>>>>>>> e099e138efe5b160d80052ec3cc9fea358ecc240
graph3b
