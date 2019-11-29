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
mammbs <- mamm[c(1, 2, 3, 5, 13, 12)]

#rename body mass column
colnames(mammbs)[5] <- "bs"

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

marine <- mammbs[mammbs$System == 'Marine',]
fw <- mammbs[mammbs$System == 'Freshwater',]
terr <- mammbs[mammbs$System == 'Terrestrial',]

#order data frame by body size
marine <- marine[order(marine$bs, decreasing = TRUE),]
fw <- fw[order(fw$bs, decreasing = TRUE),]
terr <- terr[order(terr$bs, decreasing = TRUE),]

#TERRESTRIAL
#divide data in to 50 equally sized groups
terr$group <- as.numeric(cut2(terr$bs, g=10))

#find average of body weight and number of threats within those groups
terrgroups <- aggregate(bs ~ group, terr, mean)
terrmeans <- aggregate(no_stress ~ group, terr, mean)

terrgroups <- as.data.frame(terrgroups)
terrmeans <- as.data.frame(terrmeans)

#merge group means and body size
terrfinal <- merge(terrgroups, terrmeans, by="group")

ggplot(terrfinal, aes(bs, no_stress)) +
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Avergae Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Terrestrial Mammals")

#MARINE
#divide data in to 50 equally sized groups
marine$group <- as.numeric(cut2(marine$bs, g=10))

#find average of body weight and number of threats within those groups
marinegroups <- aggregate(bs ~ group, marine, mean)
marinemeans <- aggregate(no_stress ~ group, marine, mean)

marinegroups <- as.data.frame(marinegroups)
marinemeans <- as.data.frame(marinemeans)

#merge group means and body size
marinefinal <- merge(marinegroups, marinemeans, by="group")

ggplot(marinefinal, aes(bs, no_stress)) +
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Avergae Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Marine Mammals")

#FRESHWATER
#divide data in to 50 equally sized groups
fw$group <- as.numeric(cut2(fw$bs, g=10))

#find average of body weight and number of threats within those groups
fwgroups <- aggregate(bs ~ group, fw, mean)
fwmeans <- aggregate(no_stress ~ group, fw, mean)

fwgroups <- as.data.frame(fwgroups)
fwmeans <- as.data.frame(fwmeans)

#merge group means and body size
fwfinal <- merge(fwgroups, fwmeans, by="group")

ggplot(fwfinal, aes(bs, no_stress)) +
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Avergae Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Freshwater Mammals")
