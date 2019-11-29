setwd("C:/PhD")

rm(list = ls())

library(Matrix)
library(MuMIn)
library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(binr)
library(Hmisc)
library(data.table)
library(scales)
library(lme4)


#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI_Threats.xlsx")
pan <- read_xlsx("C:/PhD/Supplementary_Datasets/PanTHERIA/PanTHERIA.xlsx")

#correct formatting and remove rows with no threat data in LPI
pan = as.matrix(pan)
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
pan[pan=="NULL"] <- NA
lpi = as.data.frame(lpi)
pan = as.data.frame(pan)

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#merge datasets and lpi
mamm <- merge(lpi, pan, by="Binomial")

#reduce data frame to only those traits deemed useful
mammbs <- mamm[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 181, 186, 192, 196, 203, 205, 206)]

#rename body mass column
colnames(mammbs)[12] <- "bs"
colnames(mammbs)[13] <- "afb"
colnames(mammbs)[14] <- "hr"
colnames(mammbs)[15] <- "ibi"
colnames(mammbs)[16] <- "ls"

#remove rows where IDs are duplicated
mammbs <- mammbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0 (-999 values)
mammbs$bs = as.numeric(mammbs$bs)
mammbs <- mammbs[mammbs$bs >= 0, ]
mammbs$bs <- log(mammbs$bs)
mammbs$tl[mammbs$tl=="-999"] <- NA

#number of species represented (491) and total time series (2062)
species <- length(unique(mammbs[,"Binomial"]))
totalTS <- sum(mammbs$Class == "Mammalia")

ggplot(mammbs, aes(bs, no_stress, colour = System)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Mammal Body Size vs Threats by System") +ylim(0,3)

ggplot(mammbs, aes(bs, no_stress, colour = Region)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Mammal Body Size vs Threats by Region") +ylim(0,3)

ggplot(mammbs, aes(bs, no_stress, colour = Red_list_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Mammal Body Size vs Threats by Red List Status") +ylim(0,3)

ggplot(mammbs, aes(bs, no_stress, colour = GROMS_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Mammal Body Size vs Threats by GROMS Category") +ylim(0,3)

ggplot(mammbs, aes(bs, no_stress, colour = Trophic_level)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Mammal Body Size vs Threats by Trophic Level") +ylim(0,3)+ xlim (5,8)+
  scale_colour_discrete(na.translate = F)
