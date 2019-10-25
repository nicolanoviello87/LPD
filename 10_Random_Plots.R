lm(list=ls())

install.packages("scales")
install.packages("readxl")
install.packages("tidyquant")
install.packages("timetk")
install.packages("digest")

library(digest)
library(tidyquant)
library(timetk)
library(mgcv)
library(stats)
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(scales)

setwd("C:/PhD")

LPI <- read_excel("C:/PhD/LPI_Data/LPI.xlsx")

#replace NULL values with NA
LPI[LPI=="NULL"] <- NA

#count years with abundance data present
LPI$Length_Time_Series <-apply(LPI[,c(65:133)],1,function(x) length(which(!is.na(x))))

#create new 'Threats' column with count of threats for each population
LPI$Threats <-apply(LPI[,c(145:147)],1,function(x) length(which(!is.na(x))))

#create subset of LPI with >2 threats and >4 years
Viable_Wide <- subset(LPI, LPI$Length_Time_Series > 4 & LPI$Threats > 1)

#wide -> long format
Viable <- gather(Viable_Wide, Year, Pop, 65:133)

#get R to select 10 random ID numbers from 'Viable' dataframe
sample (Viable$ID, c(1:77901), size=10, replace =F)

#subset those random ID number in to data frame
ten <- Viable[Viable$ID %in% c(8276, 6409, 11942, 6398, 11192, 11691, 17710, 10556, 10633, 11798),]

#convert character data to numeric
ten$Pop <- as.numeric(ten$Pop)
ten$Year <- as.numeric(ten$Year)

#rename IDs to species name for labels
spp_names <- as_labeller(c("8276" = "Haliaeetus albicilla", "6409" = "Oxyura leucocephala 2", "11942" = "Boa constrictor",
               "6398" = "Oxyura leucocephala 1", "11192" = "Pelecanus crispus", "11691" = "Pygoscelis antarcticus",
               "17710" = "Bothrops insularis", "10556" = "Anguilla dieffenbachii", "10633" = "Hydrophasianus chirurgus",
               "11798" = "Galeocerdo cuvier"))

#plot with linear regression, standard error and GAM
#legend symbols must be within aes to show on plot
ggplot(data=subset(ten, !is.na(ten$Pop)), aes(x = Year, y = Pop)) +
  geom_line(aes(colour="black"), size = 1)+
  geom_point(aes(colour='black'), size = 3) +
  geom_smooth(method = "lm", se = TRUE, size = 1, aes(colour="mediumseagreen")) +
  stat_smooth(method = "gam", se = FALSE, aes(colour="firebrick3"), formula = y ~ s(x)) +
  scale_color_identity(labels = c("Population Time Series", "GAM Smoothing", "Linear Regression"),
                       guide = "legend")+
  theme_classic() +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"),
        legend.title = element_blank(), legend.position = c(0.8, 0.1)) +
  ggtitle("Simple Population Time Series Analysis of Three Randomly Generated Populations") +
  ylab("Abundance") +
  guides(color = guide_legend(override.aes = list(shape = NA, size = 2))) +
  facet_wrap(. ~ ID, scales = "free", labeller = spp_names)

#Computation failed in `stat_smooth()`:
#A term has fewer unique covariate combinations than specified maximum degrees of freedom
