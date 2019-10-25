library(mgcv)
library(stats)
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)

setwd("C:/PhD")

LPI <- read_excel("C:/PhD/LPI_Data/LPI.xlsx")

#replace NULL values with NA
LPI[LPI=="NULL"] <- NA

#count years with abundance data present
LPI$Length_Time_Series <-apply(LPI[,c(65:133)],1,function(x) length(which(!is.na(x))))

#wide -> long format
LPI_Long <- gather(LPI, Year, Pop, 65:133)

#isolate data for Limosa haemastica from LPI dataset (ID: 10392)
Muskox <- LPI_Long[LPI_Long$ID %in% c(10392),]

#convert character data to integer
Muskox$Pop <- as.numeric(Muskox$Pop)
Muskox$Year <- as.numeric(Muskox$Year)

#plot with linear regression, standard error and GAM
#legend symbols must be within aes to show on plot
ggplot(data=subset(Muskox, !is.na(Muskox$Pop)), aes(x = Year, y = Pop)) + 
  geom_line(aes(colour="black"), size = 1)+
  geom_point(aes(colour='black'), size = 3) +
  geom_smooth(method = "lm", se = TRUE, size = 1, aes(colour="mediumseagreen")) +
  stat_smooth(method = "gam", se = FALSE, aes(colour="firebrick3"), formula = y ~ s(x)) +
  scale_color_identity(labels = c("Population Time Series", "GAM Smoothing", "Linear Regression"),
                       guide = "legend")+
  theme_classic() +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.2, 0.8)) +
  ggtitle("Simple Population Time Series Analysis of the Muskox (Ovibos moschatus)") +
  ylab("Abundance (No. Individuals)") +
  guides(color = guide_legend(override.aes = list(shape = NA, size = 2))) 
