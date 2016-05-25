#2 Corruption and human development 
#a Basic plot structure (values colored by region)
EconimistData <- read.csv("EconomistData.csv")
a1 <- ggplot(EconimistData, aes(x = CPI, y = HDI, color = Region))
a1 + geom_point()

#e fit line (Trendlinie)
(a2 <- a1 +
  geom_smooth(aes(group = 1),
              method = "lm",
              formula = y ~ log(x),
              se = FALSE,
              color = "red")) +
  geom_point()
#geom_line is put first. In this way the trendline will be plottet underneath the points

#f
#f1: Offene Punkte
(a3 <- a2 +
  geom_point(size = 4.5, shape = 21) +
  geom_point(size = 4, shape = 21) +
  geom_point(size = 3.5, shape = 21))

#c Textual labeling of selected countries
pointsToLabel <- c("Afghanistan", "Greece", "China", "India", "Rwanda", "Spain", "France", "United States", "Japan", "Norway", "Singapore")
#label these points with geom_text
(a4 <- a3 +
  geom_text(aes(label = Country),
            color = "gray20",
            data = subset(EconimistData, Country %in% pointsToLabel)))
#Labels text overlapping the points. ggreppel connects labels with points
install.packages("ggrepel")
library("ggrepel")
a3 +
  geom_text_repel(aes(label = Country),
                  color = "gray20",
                  data = subset(EconimistData, Country %in% pointsToLabel),
                  force = 10)
#d Add title and edit axis labels
library(grid)
(a4 <- a3 +
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
  ggtitle("Corruption and Human development"))


