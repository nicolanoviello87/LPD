setwd("C:/PhD")

rm(list = ls())


install.packages("maps")
install.packages("ggmap")
install.packages("tidyverse")

#load packages
library(ggplot2)
library(tidyverse)
library(ggmap)
library(tidyr)
library(readr)
library(maps)
library(readxl)
library(maps)
library(purrr)
library(plyr)

#read in xlsx file
LPI <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
LPI <- LPI[LPI$Threat_status !="Unknown (no information)",]
LPI <- LPI[LPI$Threat_status !="Unknown (large data set)",]

LPI = as.matrix(LPI)

#replace NULL values with NA
LPI[LPI=="NULL"] <- NA

LPI = as.data.frame(LPI)

#create new 'Threats' column with count of threats for each population
LPI$Threats <-apply(LPI[,c(145, 146, 147)],1,function(x) length(which(!is.na(x))))

#load world basemap
world <- map_data("world")

LPI$Threats = as.numeric(LPI$Threats)
LPI$Longitude = as.numeric(LPI$Longitude)
LPI$Latitude = as.numeric(LPI$Latitude)

#create map using 'Threats' column as point size
ggplot(world, aes(x = long, y = lat, group = group, fill = Class)) +
  geom_polygon(fill="white", colour = "gray35") + #colours for land masses
  geom_point(data = LPI, aes(x = LPI$Longitude, y = LPI$Latitude, color = Class, size = Threats), shape = 1, 
             stroke = 0, inherit.aes = F)+ #specify category, point size and shape by number of threats
  theme(axis.text.x = element_blank(),#plain background colour with no gridlines
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),panel.background = element_rect(fill = "lightcyan2"), 
        rect = element_blank(),line = element_blank(),axis.title.y=element_blank(),axis.title.x=element_blank()) +
  theme(legend.position="top")+guides(fill=guide_legend(ncol=5))+ 
  guides(colour = guide_legend(override.aes = list(size=3)))
