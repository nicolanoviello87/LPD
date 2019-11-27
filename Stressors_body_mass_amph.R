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
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
amph <- read_csv("C:/PhD/Supplementary_Datasets/Amphibio/AmphiBIO_v1/AmphiBIO_v1.csv")

#correct formatting

lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

amph = as.data.frame(amph)

#rename fishbase column
colnames(amph)[5] <- "Binomial"

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#replace space with underscore in species column
amph$Binomial <- sub(" ", "_", amph$Binomial)

#merge datasets and lpi
amph <- merge(lpi, amph, by="Binomial")

#reduce data frame to only those traits deemed useful
amphbs <- amph[c(1, 2, 15, 23, 30, 35, 47, 52, 144, 186, 208)]

#homogenise body mass column names
colnames(amphbs)[11] <- "bs"

#remove rows where IDs are duplicated
amphbs <-  amphbs %>% distinct(ID, .keep_all = TRUE)

#remove rows where body mass is less than 0
amphbs <-  amphbs[ amphbs$bs >= 0, ]

#remove rows with NAs
amphbs <-  amphbs[complete.cases( amphbs), ]

#number of species represented (74)
species <- length(unique( amphbs[,"Binomial"]))

amphbs = as.data.frame( amphbs)
amphbs$bs = as.numeric( amphbs$bs)
amphbs$no_stress = as.numeric( amphbs$no_stress)

ggplot(amphbs, aes(bs, no_stress)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm") +
   labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
   ggtitle("Body Size vs Threats for Amphibians by System") +
   facet_wrap(~System, scales = "free")

ggplot(amphbs, aes(bs, no_stress)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm") +
   labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
   ggtitle("Body Size vs Threats for Amphibians by Region") +
   facet_wrap(~Region, scales = "free")

ggplot(amphbs, aes(bs, no_stress)) +
   geom_point(stat = "identity") +
   stat_smooth(method = "glm") +
   labs(x = "Body Mass (g)", y = "Average Number of Threats", size = 20) +
   ggtitle("Body Size vs Threats for Amphibians by Red List Category") +
   facet_wrap(~Red_list_category, scales = "free")
