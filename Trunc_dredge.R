setwd("/Users/le19811/Documents/PhD")

rm(list = ls())

library(ggcorrplot)
library(lme4)
library(plyr)
library(blme)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(merTools)
library(ggeffects)
library(prediction)
library(tidyr)
library(mgcv)
library(tidymv)
library(Hmisc)
library(GGally)
library(readxl)
library(readr)
library(ggfortify)
library(stringr)
library(purrr)
library(parallel)
library(ape)
library(effects)
library(glmmTMB)
library(DHARMa)
library(blmeco)
library(scales)
library(usdm)
library(MuMIn)
library(coda)
library(car)

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

#count stressors
lpi$threat_number <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#merge LPD and body mass data
all <- merge(bs, lpi, by="Binomial")

all$Latitude = as.character(all$Latitude)
all$Latitude = as.numeric(all$Latitude)

all$Lat.scaled <- rescale(all$Latitude, to=c(0, 90))

#ln body size data
all <- all[all$lognat_bs >= 0.00, ]
all <- all %>% distinct(ID, .keep_all = TRUE)
all$lognat_bs = as.numeric(all$lognat_bs)

#count species represented
species <- length(unique(all[,"Binomial"]))

#check collinarity of continuous variables
vif.test <- all[c(4, 190)]
vif(vif.test)

#create system subsets
marine <- all[ which(all$System=="Marine"),]
fresh <- all[ which(all$System=="Freshwater"),]
terr <- all[ which(all$System=="Terrestrial"),]

gen.bin.trunc.compois.marine.inter = glmmTMB(threat_number+1 ~ (lognat_bs + Class + Lat.scaled)^3 + 
                                               (1|Genus/Binomial), data = marine, na.action = "na.fail", 
                                             family = "truncated_compois")

gen.bin.trunc.compois.terr.inter = glmmTMB(threat_number+1 ~ (lognat_bs + Class + Lat.scaled)^3 + 
                                             (1|Genus/Binomial), data = terr, na.action = "na.fail", 
                                           family = "truncated_compois")

gen.bin.trunc.compois.fresh.inter = glmmTMB(threat_number+1 ~ (lognat_bs + Class + Lat.scaled)^3 + 
                                              (1|Genus/Binomial), data = fresh, na.action = "na.fail", 
                                            family = "truncated_compois")

#start working on parallising dredge function
numCores <- detectCores()
registerDoParallel(numCores)
getDoParWorkers()

list <- list(gen.bin.trunc.compois.marine.inter, gen.bin.trunc.compois.terr.inter,
             gen.bin.trunc.compois.marine.inter)

#parallel processing of dredge function
trunc.compois.dredge <- foreach(i = list) %dopar% dredge(i)

#split dredge list in to systems
marine.dredge <- trunc.compois.dredge[[1]]
terr.dredge <- trunc.compois.dredge[[2]]
fresh.dredge <- trunc.compois.dredge[[3]]
