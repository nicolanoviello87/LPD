#Merge elton traits and LPI by binomial
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
library(scales)
library(data.table)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
elton <- read_xlsx("C:/PhD/Supplementary_Datasets/Elton_Traits/Elton_Traits.xlsx")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

#rename column
colnames(elton)[8] <- "Binomial"

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#replace space with underscore in species column
elton$Binomial <- sub(" ", "_", elton$Binomial)

#merge datasets and lpi
elt <- merge(lpi, elton, by="Binomial")

#reduce data frame to only those traits deemed useful
elt <- elt[c(1, 2, 15, 23, 35, 47, 52, 144, 145, 146, 147, 186, 221)]

#include only bonomial, ID, threat number and body mass
eltbs <- elt[c(1, 2, 3, 5, 7, 13, 12, 13)]

#homogenise body mass column names
colnames(eltbs)[6] <- "bs"

#remove rows where IDs are duplicated
eltbs <- eltbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
eltbs <- eltbs[eltbs$bs >= 0, ]

#remove rows with NAs
eltbs <- eltbs[complete.cases(eltbs), ]

#number of species represented (2151)
species <- length(unique(eltbs[,"Binomial"]))

eltbs = as.data.frame(eltbs)
eltbs$bs = as.numeric(eltbs$bs)
eltbs$no_stress = as.numeric(eltbs$no_stress)

marine <- eltbs[eltbs$System == 'Marine',]
fw <- eltbs[eltbs$System == 'Freshwater',]
terr <- eltbs[eltbs$System == 'Terrestrial',]

#order data frame by body size
marine <- marine[order(marine$bs, decreasing = TRUE),]
fw <- fw[order(fw$bs, decreasing = TRUE),]
terr <- terr[order(terr$bs, decreasing = TRUE),]

#TERRESTRIAL
#divide data in to 50 equally sized groups
terr$group <- as.numeric(cut2(terr$bs, g=50))

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
  ggtitle("Body Size vs Threats for Terrestrial Birds")

#MARINE
#divide data in to 50 equally sized groups
marine$group <- as.numeric(cut2(marine$bs, g=55))

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
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Marine Birds")

#FRESHWATER
#divide data in to 50 equally sized groups
fw$group <- as.numeric(cut2(fw$bs, g=50))

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
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Freshwater Birds")

fwfinal$system <- 'fw'
marinefinal$system <- 'marine'
terrfinal$system <- 'terr'

elt2 <- rbind(fwfinal, marinefinal, terrfinal)

#All in facet wrap - not averaged
ggplot(elt2, aes(bs, no_stress))+
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Birds by System")  +
  facet_wrap(~system, scales = "free")

#for all Birds by Migratory Behaviour
ggplot(eltbs, aes(bs, no_stress))+
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Birds by Migratory Behaviour")  +
  facet_wrap(~GROMS_category, scales = "free")

#for Marine Birds by Migratory Behaviour
ggplot(marine, aes(bs, no_stress))+
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Marine Birds by Migratory Behaviour")  +
  facet_wrap(~GROMS_category, scales = "free")

#for Terrestrial Birds by Migratory Behaviour
ggplot(terr, aes(bs, no_stress))+
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Terrestrial Birds by Migratory Behaviour")  +
  facet_wrap(~GROMS_category, scales = "free")

#for Freshwater Birds by Migratory Behaviour
ggplot(fw, aes(bs, no_stress))+
  geom_point(stat = "identity")+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20)+
  ggtitle("Body Size vs Threats for Freshwater Birds by Migratory Behaviour")  +
  facet_wrap(~GROMS_category, scales = "free")

