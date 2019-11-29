#Merge PanTHERIA and LPI by binomial
setwd("C:/PhD")

rm(list = ls())

library(plyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(binr)
library(Hmisc)
library(data.table)
library(readr)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI_Threats.xlsx")
amph <- read_csv("C:/PhD/Supplementary_Datasets/Amphibio/AmphiBIO_v1/AmphiBIO_v1.csv")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
amph = as.data.frame(amph)

#rename fishbase column
colnames(amph)[5] <- "Binomial"

#homogenise Log Body Mass (g) column names
colnames(amph)[23] <- "bs"

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#replace space with underscore in species column
amph$Binomial <- sub(" ", "_", amph$Binomial)

#merge datasets and lpi
amph1 <- merge(lpi, amph, by="Binomial")

#reduce data frame to only those traits deemed useful
amphbs <- amph1[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 181, 186, 208)]

#remove rows where IDs are duplicated
amphbs <-  amphbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where Log Body Mass (g) is less than 0
amphbs$bs = as.numeric(amphbs$bs)
amphbs <- amphbs[amphbs$bs >= 0, ]
amphbs$bs <- log(amphbs$bs)

#number of species represented (74)
species <- length(unique( amphbs[,"Binomial"]))

ggplot(amphbs, aes(bs, no_stress, colour = System)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
   labs(x = "Log Body Mass (g) (g)", y = "Number of Threats", size = 20) +
   ggtitle("Amphibian Body Size vs Threats by System") +ylim(0,3)+
   scale_colour_discrete(na.translate = F)

ggplot(amphbs, aes(bs, no_stress, colour = Region)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
   labs(x = "Log Body Mass (g) (g)", y = "Number of Threats", size = 20) +
   ggtitle("Amphibian Body Size vs Threats by Region") +ylim(0,3)+
   scale_colour_discrete(na.translate = F)

ggplot(amphbs, aes(bs, no_stress, colour = Red_list_category)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
   labs(x = "Log Body Mass (g) (g)", y = "Number of Threats", size = 20) +
   ggtitle("Amphibian Body Size vs Threats by IUCN Status") +ylim(0,3)+
   scale_colour_discrete(na.translate = F)

ggplot(amphbs, aes(bs, no_stress, colour = Red_list_category)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm", method.args = list(family = "poisson"), se=FALSE) +
   labs(x = "Log Body Mass (g) (g)", y = "Number of Threats", size = 20) +
   ggtitle("Amphibian Body Size vs Threats by IUCN Status") +ylim(0,3)+
   scale_colour_discrete(na.translate = F)