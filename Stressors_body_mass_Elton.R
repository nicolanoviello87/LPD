#Merge Eltontraits and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(Hmisc)
library(scales)
library(data.table)
library(readr)
library(writexl)

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
eltbs <- elt[c(1, 2, 15, 23, 30, 35, 37,  39, 42, 47, 52, 144, 186, 221)]

#homogenise body mass column names
colnames(eltbs)[12] <- "bs"

#remove rows where IDs are duplicated
eltbs <- eltbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
eltbs$bs = as.numeric(eltbs$bs)
eltbs <- eltbs[eltbs$bs >= 0, ]

#remove rows with NAs
eltbs <- eltbs[complete.cases(eltbs), ]

#number of species represented (758) and total number of time series (2371)
species <- length(unique(eltbs[,"Binomial"]))
totalTS <- sum(eltbs$Class == "Aves")

#plot all mammals by system in facets
ggplot(eltbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Birds by System") +
  facet_wrap(~System, scales = "free")

ggplot(eltbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Birds by GROMS Category") +
  facet_wrap(~GROMS_category, scales = "free")

ggplot(eltbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Birds by Red List Category") +
  facet_wrap(~Red_list_category, scales = "free")

ggplot(eltbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Birds by Terrestrial Biome") +
  facet_wrap(~T_biome, scales = "free")

ggplot(eltbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Birds by Marine Biome") +
  facet_wrap(~M_biome, scales = "free")

ggplot(eltbs, aes(bs, no_stress)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm") +
  labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
  ggtitle("Body Size vs Threats for Birds by Freshwater Biome") +
  facet_wrap(~FW_biome, scales = "free")
