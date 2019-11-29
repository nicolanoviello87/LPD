#Merge PanTHERIA and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(Hmisc)
library(data.table)
library(scales)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI_Threats.xlsx")
amni <- read_csv("C:/PhD/Supplementary_Datasets/Amniote/Amniote_Database_Aug_2015.csv")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
amni = as.data.frame(amni)

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#merge datasets and lpi
amni<- merge(lpi, amni, by="Binomial")

#reduce data frame to only those traits deemed useful
amnibs <- amni[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 181, 186, 197)]

#homogenise body mass column names
colnames(amnibs)[12] <- "bs"

#remove rows where IDs are duplicated
amnibs <- amnibs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
amnibs$bs = as.numeric(amnibs$bs)
amnibs <- amnibs[amnibs$bs >= 0, ]
amnibs$bs <- log(amnibs$bs)

#remove rows with NAs
amnibs <- amnibs[complete.cases(amnibs), ]

#number of species represented (1434) and total time series (5058)
species <- length(unique(amnibs[,"Binomial"]))

ggplot(amnibs, aes(bs, no_stress, colour = System)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Amniote Body Size vs Threats by System") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(amnibs, aes(bs, no_stress, colour = Region)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Amniote Body Size vs Threats by Region") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(amnibs, aes(bs, no_stress, colour = Red_list_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Amniote Body Size vs Threats by Red List Status") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(amnibs, aes(bs, no_stress, colour = GROMS_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Amniote Body Size vs Threats by GROMS Category") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(amnibs, aes(bs, no_stress, colour = Trophic_level)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Amniote Body Size vs Threats by Trophic Level") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

