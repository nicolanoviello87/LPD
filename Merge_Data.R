#Merge PanTHERIA and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

install_github("ropensci/rfishbase")

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(binr)
library(Hmisc)
library(readxl)
library(tidyr)
library(dplyr)
library(naniar)
library(ggplot2)
library(scales)
library(data.table)
library(rfishbase)
library(readr)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
elton <- read_xlsx("C:/PhD/Supplementary_Datasets/Elton_Traits/Elton_Traits.xlsx")
pan <- read_xlsx("C:/PhD/Supplementary_Datasets/PanTHERIA/PanTHERIA.xlsx")
amp <- read_csv("C:/PhD/Supplementary_Datasets/Amphibio/AmphiBIO_v1/AmphiBIO_v1.csv")
fb <- brains(species_list = NULL, fields = NULL, server = NULL)
amni <- read_csv("C:/PhD/Supplementary_Datasets/Amniote/Amniote_Database_Aug_2015.csv")

#correct formatting
pan = as.matrix(pan)
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
pan[pan=="NULL"] <- NA
lpi = as.data.frame(lpi)
pan = as.data.frame(pan)
fb = as.data.frame(fb)
amp = as.data.frame(amp)
amni = as.data.frame(amni)

#rename fishbase column
colnames(fb)[2] <- "Binomial"
colnames(elton)[8] <- "Binomial"
colnames(amp)[5] <- "Binomial"

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#replace space with underscore in species column
fb$Binomial <- sub(" ", "_", fb$Binomial)
elton$Binomial <- sub(" ", "_", elton$Binomial)
amp$Binomial <- sub(" ", "_", amp$Binomial)

#merge datasets and lpi
mamm <- merge(lpi, pan, by="Binomial")
elt <- merge(lpi, elton, by="Binomial")
fish <- merge(lpi, fb, by="Binomial")
amph <- merge(lpi, amp, by="Binomial")
amni <- merge(lpi, amni, by="Binomial")

#reduce data frame to only those traits deemed useful
mamm <- mamm[c(1, 2, 15, 23, 35, 47, 52, 145, 146, 147, 186, 192, 196, 203, 205, 206, 217)]
fish <- fish[c(1, 2, 15, 23, 35, 47, 52, 145, 146, 147, 186, 198)]
elt <- elt[c(1, 2, 15, 23, 35, 47, 52, 145, 146, 147, 186, 221)]
amph <- amph[c(1, 2, 15, 23, 35, 47, 52, 145, 146, 147, 186, 208, 209, 215, 216)]
amni <- amni[c(1, 2, 15, 23, 35, 47, 52, 145, 146, 147, 186, 187, 194, 195, 197, 199, 208)]

#include only bonomial, ID, threat number and body mass
mammbs <- mamm[c(1, 2, 11, 12)]
fishbs <- fish[c(1, 2, 11, 12)]
eltbs <- elt[c(1, 2, 11, 12)]
amphbs <- amph[c(1, 2, 11, 12)]
amnibs <- amni[c(1, 2, 11, 15)]

#homogenise body mass column names
colnames(mammbs)[4] <- "bs"; colnames(fishbs)[4] <- "bs"; colnames(eltbs)[4] <- "bs"; colnames(amphbs)[4] <- "bs"
colnames(amnibs)[4] <- "bs"

#combine reduced dataframes
all <- rbind.fill(mammbs, fishbs, eltbs, amphbs, amnibs)

#remove rows where IDs are duplicated
all <- all %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
all <- all[all$bs >= 0, ]

#remove rows with NAs
all <- all[complete.cases(all), ]

#number of species represented (2151)
species <- length(unique(all[,"Binomial"]))

all = as.data.frame(all)
all$bs = as.numeric(all$bs)
all$no_stress = as.numeric(all$no_stress)

#order data frame by body size
all <- all[order(all$bs, decreasing = TRUE),]

#divide data in to 50 equally sized groups
all$group <- as.numeric(cut2(all$bs, g=50))

#find average of body weight and number of threats within those groups
groups <- aggregate(bs ~ group, all, mean)
means <- aggregate(no_stress ~ group, all, mean)

groups <- as.data.frame(groups)
means <- as.data.frame(means)

final <- merge(groups, means, by="group")

plot(final$bs, final$no_stress)
