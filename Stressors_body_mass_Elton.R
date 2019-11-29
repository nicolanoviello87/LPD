#Merge Eltontraits and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(Hmisc)
library(scales)
library(data.table)
library(readr)
library(writexl)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI_Threats.xlsx")
elton <- read_xlsx("C:/PhD/Supplementary_Datasets/Elton_Traits/Elton_Traits.xlsx")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

#rename column
colnames(elton)[8] <- "Binomial"

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#replace space with underscore in species column
elton$Binomial <- sub(" ", "_", elton$Binomial)

#merge datasets and lpi
elt <- merge(lpi, elton, by="Binomial")

#reduce data frame to only those traits deemed useful
eltbs <- elt[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 181, 186, 221)]

#homogenise body mass column names
colnames(eltbs)[12] <- "bs"

#remove rows where IDs are duplicated
eltbs <- eltbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
eltbs$bs = as.numeric(eltbs$bs)
eltbs$bs <- log(eltbs$bs)
eltbs <- eltbs[eltbs$bs >= 0, ]


#number of species represented (758) and total number of time series (2371)
species <- length(unique(eltbs[,"Binomial"]))
totalTS <- sum(eltbs$Class == "Aves")

#plot all mammals by system in facets
ggplot(eltbs, aes(bs, no_stress, colour = System)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Bird Body Size vs Threats by System") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(eltbs, aes(bs, no_stress, colour = Region)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Bird Body Size vs Threats by Region") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(eltbs, aes(bs, no_stress, colour = Red_list_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Bird Body Size vs Threats by Red List Status") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(eltbs, aes(bs, no_stress, colour = GROMS_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Bird Body Size vs Threats by GROMS Category") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)

ggplot(eltbs, aes(bs, no_stress, colour = Trophic_level)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Bird Body Size vs Threats by Trophic Level") +ylim(0,3)+
  scale_colour_discrete(na.translate = F)
