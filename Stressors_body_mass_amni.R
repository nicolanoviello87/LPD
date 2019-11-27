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
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
amni <- read_csv("C:/PhD/Supplementary_Datasets/Amniote/Amniote_Database_Aug_2015.csv")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

amni = as.data.frame(amni)

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#merge datasets and lpi
amni<- merge(lpi, amni, by="Binomial")

#reduce data frame to only those traits deemed useful
amnibs <- amni[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 186, 197)]

#homogenise body mass column names
colnames(amnibs)[11] <- "bs"

#remove rows where IDs are duplicated
amnibs <- amnibs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
amnibs$bs = as.numeric(amnibs$bs)
amnibs <- amnibs[amnibs$bs >= 0, ]

#remove rows with NAs
amnibs <- amnibs[complete.cases(amnibs), ]

#number of species represented (1434) and total time series (5058)
species <- length(unique(amnibs[,"Binomial"]))

ggplot(amnibs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Amniotes by System") +
  facet_wrap(~System, scales = "free")

ggplot(amnibs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Amniotes by GROMS Category") +
  facet_wrap(~GROMS_category, scales = "free")

ggplot(amnibs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Amniotes by Region") +
  facet_wrap(~Region, scales = "free")

ggplot(amnibs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Amniotes by Red List Category") +
  facet_wrap(~Red_list_category, scales = "free")
