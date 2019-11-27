#Merge PanTHERIA and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

install_github("ropensci/rfishbase")

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(binr)
library(Hmisc)
library(readxl)
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
lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

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
mammbs <- mamm[c(1, 2, 15, 23, 30, 35, 47, 52, 186, 192)]
fishbs <- fish[c(1, 2, 15, 23, 30, 35, 47, 52, 186, 198)]
eltbs <- elt[c(1, 2, 15, 23, 30, 35, 47, 52, 186, 221)]
amphbs <- amph[c(1, 2, 15, 23, 30, 35, 47, 52, 186, 208)]
amnibs <- amni[c(1, 2, 15, 23, 30, 35, 47, 52, 186, 197)]

#homogenise body mass column names
colnames(mammbs)[10] <- "bs"; colnames(fishbs)[10] <- "bs"; colnames(eltbs)[10] <- "bs";
colnames(amphbs)[10] <- "bs"; colnames(amnibs)[10] <- "bs"

mammbs$data <- "pan"
fishbs$data <- "fish"
eltbs$data <- "elt"
amphbs$data <- "amph"
amnibs$data <- "amni"

#combine reduced dataframes
all <- rbind.fill(mammbs, fishbs, eltbs, amphbs, amnibs)

#remove rows where IDs are duplicated
all <- all %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
all$bs = as.numeric(all$bs)
all <- all[all$bs >= 0, ]

#remove rows with NAs
all <- all[complete.cases(all), ]

#number of species represented (1759) and total time series (2023)
species <- length(unique(all[,"Binomial"]))

ggplot(all, aes(bs, no_stress)) +
  geom_point(stat = "identity") + 
  geom_smooth(method = "glm") + 
  labs(x = "Body Mass (g)", y = "Avergae Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Vertebrates by System") +
  facet_wrap(~System, scales = "free")

ggplot(all, aes(bs, no_stress)) +
  geom_point(stat = "identity") + 
  geom_smooth(method = "glm") + 
  labs(x = "Body Mass (g)", y = "Avergae Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Vertebrates by Class") +
  facet_wrap(~Class, scales = "free")

ggplot(all, aes(bs, no_stress)) +
  geom_point(stat = "identity") + 
  geom_smooth(method = "glm") + 
  labs(x = "Body Mass (g)", y = "Avergae Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Vertebrates by Class") +
  facet_wrap(~Region, scales = "free")
