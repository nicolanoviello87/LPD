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
library(writexl)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
fb <- brains(species_list = NULL, fields = NULL, server = NULL)

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

fb = as.data.frame(fb)

#rename fishbase column
colnames(fb)[2] <- "Binomial"

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#replace space with underscore in species column
fb$Binomial <- sub(" ", "_", fb$Binomial)

#merge datasets and lpi
fish <- merge(lpi, fb, by="Binomial")

#reduce data frame to only those traits deemed useful
fishbs <- fish[c(1, 2, 15, 23, 30, 35, 40, 41, 42, 47, 52, 144, 186, 198)]

#homogenise body mass column names
colnames(fishbs)[14] <- "bs"

#remove rows where IDs are duplicated
fishbs <- fishbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
fishbs <- fishbs[fishbs$bs >= 0, ]

#remove rows with NAs
fishbs <- fishbs[complete.cases(fishbs), ]

#number of species represented (253)
species <- length(unique(fishbs[,"Binomial"]))

fishbs = as.data.frame(fishbs)
fishbs$bs = as.numeric(fishbs$bs)
fishbs$no_stress = as.numeric(fishbs$no_stress)

ggplot(fishbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Fish by System") +
  facet_wrap(~System, scales = "free")

ggplot(fishbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Fish by GROMS Category") +
  facet_wrap(~GROMS_category, scales = "free")

ggplot(fishbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Fish by Red List Category") +
  facet_wrap(~Red_list_category, scales = "free")

ggplot(fishbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Fish by Realm") +
  facet_wrap(~M_realm, scales = "free")

