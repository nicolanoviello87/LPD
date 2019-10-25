library(ggplot2)
library(tidyverse)
library(ggmap)
library(readxl)

setwd("C:/PhD")
LPI <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")

world <- map_data("world")

ggplot(world, aes(x = long, y = lat, group = group, fill = Class)) +
  geom_polygon(fill="white", colour = "gray35") +
  geom_point(data = LPI, aes(x = LPI$Longitude, y = LPI$Latitude, color = Class), size = 1, shape = 16, stroke = 0, 
        inherit.aes = F)+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),panel.background = element_rect(fill = "lightcyan2"),
        rect = element_blank(),line = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank()) +
  theme(legend.position="top")+guides(fill=guide_legend(ncol=5))+ guides(colour = guide_legend(override.aes = list(size=3)))
?help(ggplot2)
