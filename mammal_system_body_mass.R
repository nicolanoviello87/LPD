#Merge PanTHERIA and LPI by binomial
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
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
pan <- read_xlsx("C:/PhD/Supplementary_Datasets/PanTHERIA/PanTHERIA.xlsx")

#correct formatting and remove rows with no threat data in LPI
pan = as.matrix(pan)
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
pan[pan=="NULL"] <- NA
lpi = as.data.frame(lpi)
pan = as.data.frame(pan)
lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#merge datasets and lpi
mamm <- merge(lpi, pan, by="Binomial")

#reduce data frame to only those traits deemed useful
mammbs <- mamm[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 186, 192, 196, 203, 205, 206, 217)]

#rename body mass column
colnames(mammbs)[11] <- "bs"
colnames(mammbs)[12] <- "afb"
colnames(mammbs)[13] <- "hr"
colnames(mammbs)[14] <- "ibi"
colnames(mammbs)[15] <- "ls"
colnames(mammbs)[16] <- "tl"

#remove rows where IDs are duplicated
mammbs <- mammbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0 (-999 values)
mammbs$bs = as.numeric(mammbs$bs)
mammbs <- mammbs[mammbs$bs >= 0, ]
mammbs$bs <- log(mammbs$bs)

#remove rows with NAs
mammbs <- mammbs[complete.cases(mammbs), ]

#number of species represented (491) and total time series (2062)
species <- length(unique(mammbs[,"Binomial"]))
totalTS <- sum(mammbs$Class == "Mammalia")

#plot all mammals by system in facets
ggplot(mammbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Mammals by System") +
  facet_wrap(~System, scales = "free")

ggplot(mammbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Mammals by Region") +
  facet_wrap(~Region, scales = "free")

ggplot(mammbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Mammals by Red List Category") +
  facet_wrap(~Red_list_category, scales = "free")

