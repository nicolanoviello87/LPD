setwd("/Users/le19811/Documents/PhD")

rm(list = ls())

library(ggcorrplot)
library(lme4)
library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggeffects)
library(prediction)
library(tidyr)
library(mgcv)
library(scales)
library(tidymv)
library(Hmisc)
library(GGally)
library(readxl)
library(scales)
library(readr)
library(ggfortify)
library(ggplot2)
library(stringr)
library(car)
library(data.table)
library(purrr)
library(iterators)
library(ape)
require(ggiraph)
library(sjPlot)
library(effects)
library(ggplot2)
library(usdm)

#read in xlsx file
lpd <- read_xlsx("/Users/le19811/Documents/PhD/LPI_Data/LPI.xlsx")
bs <- read_xlsx("/Users/le19811/Documents/PhD/Supplementary_Datasets/Body_Mass_means.xlsx")

#keep only those lpi entries with threat data
lpi <- lpd[ which(lpd$Threat_status=="Threatened"),]
lpi2 <- lpd[ which(lpd$Threat_status== "No threats"),]
lpi <- rbind(lpi, lpi2)

#replace NULL values with NA
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

#change names of Classes to be more friendly
lpi$Class <- as.character(lpi$Class)
lpi$Class[lpi$Class == 'Actinopterygii'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Sarcopterygii'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Osteichthyes'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Myxini'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Cephalaspidomorphi'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Holocephali'] <- "Cartilaginous Fish"
lpi$Class[lpi$Class == 'Elasmobranchii'] <- "Cartilaginous Fish"
lpi$Class[lpi$Class == 'Aves'] <- "Birds"
lpi$Class[lpi$Class == 'Reptilia'] <- "Reptiles"
lpi$Class[lpi$Class == 'Amphibia'] <- "Amphibians"
lpi$Class[lpi$Class == 'Mammalia'] <- "Mammals"

#isolate latitude and ID number (ID needed for merging later)
lat <- lpi[c(1, 31)]

#reformat and scale latitude variable
lat = as.data.frame(lat)
lat$Latitude = as.character(lat$Latitude)
lat$Latitude = as.numeric(lat$Latitude)
lat$Lat.scaled <- rescale(lat$Latitude, to=c(0,90))

#split latitudes into bins of 5 degrees each
split(lat, cut2(lat$Lat.scaled,seq(0, 90, 5)))
lat$group <- cut(lat$Lat.scaled, seq(0, 90, 5))

#remove redundant symbols from new 'group' column
lat$group = as.character(lat$group)
lat$group = substr(lat$group,1,nchar(lat$group)-1)
lat$group <- substring(lat$group, 2)
lat$group <- sub(",", " - ", lat$group)

#count stressors
lpi$threat_number <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#merge LPD and body mass data
all <- merge(bs, lpi, by="Binomial")
all <- merge(all, lat, by="ID")

#ln body size data and roudn to two decimal places
all <- all[all$lognat_bs >= 0.00, ]
all <- all %>% distinct(ID, .keep_all = TRUE)
all$lognat_bs = as.numeric(all$lognat_bs)

#count species represented
species <- length(unique(all[,"Binomial"]))

#check collinarity of continuous variables
vif.test <- all[c(5, 191)]
vif(vif.test)

#create system subsets
marine <- all[ which(all$System=="Marine"),]
marine <- marine[c(1, 5, 18, 189,191)]
fresh <- all[ which(all$System=="Freshwater"),]
fresh <- fresh[c(1, 5, 18, 189,191)]
terr <- all[ which(all$System=="Terrestrial"),]
terr <- terr[c(1, 5, 18, 189,191)]

#Models by system
#Marine
marine.mod = glmer(threat_number ~ lognat_bs * Class + Class * Lat.scaled +
                     (1|ID), data = marine, family = poisson(link = "log"))

plot_model(marine.mod, type = "pred", terms = c("lognat_bs", "Class"), title = "Marine",
           grid = TRUE) + coord_cartesian(expand = TRUE,ylim=c(0,3)) + 
  scale_x_continuous(labels = comma)

marine$fit <- predict(marine.mod, type = "response")

ggplot(marine, aes(lognat_bs, threat_number, group=Class, 
               col=Class)) + 
  facet_grid(~Class) +
  geom_point(aes(y=fit), alpha = 0.5) + 
  ggtitle("Marine Vertebrates") +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()

#Freshwater
fresh.mod = glmer(threat_number ~ lognat_bs * Class + Class * Lat.scaled +
                     (1|ID), data = fresh, family = poisson(link = "log"))

plot_model(fresh.mod, type = "pred", terms = c("lognat_bs", "Class"), title = "Freshwater",
           grid = TRUE) + coord_cartesian(expand = TRUE,ylim=c(0,3)) + 
  scale_x_continuous(labels = comma)

summary(fresh.mod)
fresh$fit <- predict(fresh.mod, type = "response")

ggplot(fresh, aes(lognat_bs, threat_number, group=Class, 
                   col=Class)) + 
  facet_grid(~Class) +
  geom_point(aes(y=fit), alpha = 0.5) + 
  ggtitle("Freshwater Vertebrates") +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()

#Terrestrial
terr.mod = glmer(threat_number ~ lognat_bs * Class + Class * Lat.scaled +
                     (1|ID), data = terr, family = poisson(link = "log"))

plot_model(terr.mod, type = "pred", terms = c("lognat_bs", "Class"), title = "Terrestrial",
           grid = TRUE) + coord_cartesian(expand = TRUE, ylim=c(0,3)) + 
  scale_x_continuous(labels = comma)

terr$fit <- predict(terr.mod, type = "response")

ggplot(terr, aes(lognat_bs, threat_number, group=Class, 
                  col=Class)) + 
  facet_grid(~Class) +
  geom_point(aes(y=fit), alpha = 0.5) + 
  ggtitle("Terrestrial Vertebrates") +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()

