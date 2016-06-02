#2 Corruption and human development 
#a Basic plot structure (values colored by region)
EconimistData <- read.csv("EconomistData.csv")
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)
library(grid)


pointsToLabel <- c("Afghanistan", "Greece", "China", "India", "Rwanda", "Spain", "France", "United States", "Japan", "Norway", "Singapore")

ggplot(EconimistData, aes(x = CPI, y = HDI, color = Region))+
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), se = FALSE, color = "red") +
  geom_point(size = 4.5, shape = 21) +
  geom_point(size = 4, shape = 21) +
  geom_point(size = 3.5, shape = 21)+
  geom_text_repel(aes(label = Country),color = "gray20",data = subset(EconimistData, Country %in% pointsToLabel),force = 10)+
  
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     limits = c(.9, 10.5),
                     breaks = 1:10) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F")) +
  ggtitle("Corruption and Human development")-> graph2

graph2
