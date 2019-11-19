rm(list = ls())

install.packages("Hmisc")
install.packages("plyr")

library(plyr)
library(wesanderson)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(writexl)
library(binr)
library(Hmisc)

#read in xlsx file
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")

lat <- lpi[c(31, 145:147)]

lat[lat=="NULL"] <- NA

#create new column with stressor count
lat$na_count <- apply(lat[2:4], 1, function(x) sum(!is.na(x)))

#convert to matrix
lat = as.matrix(lat)

#identify unique stressor combinations (ignoring order) and count frequency of each, returning dataframe
latf <-lat %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

#separate numbers within column in to new column
latf$threats <- substr(latf$combo, 16, 65)
latf$latitude <- substr(latf$combo, 1, 10)
latf$number <- substr(latf$combo, 13, 13)

#Sort in descending order of latitude
latf <- latf[order(latf$latitude, decreasing =TRUE),]

#insert id number for each combination
latf$id <- 1:nrow(latf)

lat <- latf[c(2:5)]

lat = as.data.frame(lat)
lat$latitude = as.numeric(lat$latitude)
lat$number = as.numeric(lat$number)

split(lat, cut2(lat$latitude,seq(-80,85,5)))
lat$group <- cut(lat$latitude, seq(-80, 85, 5))

mean <- tapply(lat$number, lat$group, mean)

mean = as.data.frame(mean)
mean$id <- 1:nrow(mean)
mean <- mean[order(mean$id, decreasing = TRUE),]

mean$names <- rownames(mean)

#
mean$names = as.factor(mean$names)
mean$id = as.factor(mean$id)

mean$names <- factor(mean$names, levels = mean$names[order(mean$id)])

#
ggplot(mean, aes(x = reorder(names, -id), y=mean)) + 
  geom_bar(stat = 'identity', fill = "brown2") +
  theme_light() +
  coord_flip() +                        
  xlab("Latitude") + ylab("Average Threat Number")+
  scale_fill_manual("#FF6666") +
  scale_x_discrete(labels=c("-80 - -75", "-75 - -70", "-70 - -65", "-65 - -60", "-60 - -55","-55 - -50","-50 - -45",
                            "-45 - -40","-40 - -35","-35 - -30","-30 - -25","-25 - -20","-20 - -15","-15 - -20",
                            "-15 - -10","-10 - -5","-5 - 0", "0 - 5", "5 - 10", "10 - 15", "15 - 20",
                            "20 - 25","25 - 30","30 - 35","35 - 40","40 - 45","45 - 50","50 - 55","55 - 60","60 - 65",
                            "65 - 70","70 - 75", "75 - 80", "80 - 85"))
