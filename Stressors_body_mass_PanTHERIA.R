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
library(binr)
library(Hmisc)
library(data.table)

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
mamm <- mamm[c(1, 2, 15, 23, 35, 47, 52, 144, 145, 146, 147, 186, 192, 196, 203, 205, 206, 217)]

#include only bonomial, ID, class, threat number and body mass
mammbs <- mamm[c(1, 2, 3, 13, 12)]

#rename body mass column
colnames(mammbs)[4] <- "bs"

mammbs <- mammbs[c(1:5)]

#remove rows where IDs are duplicated
mammbs <- mammbs %>% distinct(ID, .keep_all = TRUE)

mammbs$bs = as.numeric(mammbs$bs)

#remove rows where body mass is less than 0 (-999 values)
mammbs <- mammbs[mammbs$bs >= 0, ]

#remove rows with NAs
mammbs <- mammbs[complete.cases(mammbs), ]

#number of species represented (2151)
species <- length(unique(mammbs[,"Binomial"]))

mammbs = as.data.frame(mammbs)
mammbs$bs = as.numeric(mammbs$bs)
mammbs$no_stress = as.numeric(mammbs$no_stress)

#order data frame by body size
mammbs <- mammbs[order(mammbs$bs, decreasing = TRUE),]

#divide data in to 50 equally sized groups
mammbs$group <- as.numeric(cut2(mammbs$bs, g=50))

#find average of body weight and number of threats within those groups
mammbsgroups <- aggregate(bs ~ group, mammbs, mean)
mammbsmeans <- aggregate(no_stress ~ group, mammbs, mean)

mammbsgroups <- as.data.frame(mammbsgroups)
mammbsmeans <- as.data.frame(mammbsmeans)

#merge group means and body size
mammbsfinal <- merge(mammbsgroups, mammbsmeans, by="group")

ggplot(mammbsfinal, aes(bs, no_stress)) +
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")



