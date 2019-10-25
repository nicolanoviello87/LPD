library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

setwd("C:/PhD")

LPI <- read_excel("C:/PhD/LPI_Data/LPI.xlsx")

#replace NULL values with NA
LPI[LPI=="NULL"] <- NA

#identify number of unique stressor combinations (135)
Threat_Combos <- unique(Viable$Combination)

#find no. of unique species in LPI
UniqueSpecies <- unique(LPI$Binomial)

#find no. unique classes in LPi
UniqueClasses  <- unique(LPI$Class)

#find % of each class in main LPI
proportions <- table(LPI$Class)/length(LPI$Class)
percentages <- proportions*100

#plot % class in LPI
barplot(percentages, col = rainbow(9))
