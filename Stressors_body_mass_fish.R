#Merge PanTHERIA and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

install_github("ropensci/rfishbase")

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(Hmisc)
library(scales)
library(data.table)
library(rfishbase)
library(readr)
library(writexl)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI_threats.xlsx")
fb <- brains(species_list = NULL, fields = NULL, server = NULL)

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
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
fishbs <- fish[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 186, 198)]

#homogenise body mass column names
colnames(fishbs)[11] <- "bs"

#remove rows where IDs are duplicated
fishbs <- fishbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
fishbs$bs = as.numeric(fishbs$bs)
fishbs <- fishbs[fishbs$bs >= 0, ]
fishbs$bs <- log(fishbs$bs)
fishbs <- fishbs[fishbs$bs >= 0, ]

#remove rows with NAs
fishbs <- fishbs[complete.cases(fishbs), ]

#number of species represented (253)
species <- length(unique(fishbs[,"Binomial"]))

ggplot(fishbs, aes(bs, no_stress, colour = System)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Fish Body Size vs Threats by System") + ylim(0,3)+
  scale_colour_discrete(na.translate = F) + scale_colour_manual(values=cbbPalette)

ggplot(fishbs, aes(bs, no_stress, colour = Region)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Fish Body Size vs Threats by Region") + ylim(0,3)+
  scale_colour_discrete(na.translate = F) + scale_colour_manual(values=cbbPalette)

ggplot(fishbs, aes(bs, no_stress, colour = Red_list_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Average Number of Threats", size = 20) +
  ggtitle("Fish Body Size vs Threats by Red List Status") + ylim(0,3)+
  scale_colour_discrete(na.translate = F) + scale_colour_manual(values=cbbPalette)

ggplot(fishbs, aes(bs, no_stress, colour = GROMS_category)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Fish Body Size vs Threats by GROMS Category") + ylim(0,3)+
  scale_colour_discrete(na.translate = F) + scale_colour_manual(values=cbbPalette)

ggplot(fishbs, aes(bs, no_stress, colour = Class)) +
  geom_point(stat = "identity") +
  stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
  labs(x = "Log Body Mass", y = "Number of Threats", size = 20) +
  ggtitle("Fish Body Size vs Threats by Class") + ylim(0,3) +
  scale_colour_discrete(na.translate = F) +scale_colour_manual(values=cbbPalette)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
