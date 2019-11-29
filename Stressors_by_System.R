rm(list = ls())
  
install.packages("ggpubr")   

library(ggpubr)
library(wesanderson)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

#create new dataframe with three threat columns; replace NULL values with NA
system <- lpi[c(35, 145:147)]
system[system=="NULL"] <- NA

system = as.data.frame(system)

#create new column with stressor count
system$na_count <- apply(system[2:4], 1, function(x) sum(!is.na(x)))

#subset by system
marine <- system[system[ ,1] == "Marine", ]
terr <- system[system[ ,1] == "Terrestrial", ]
fw <- system[system[ ,1] == "Freshwater", ]
marine <- marine[c(2:5)]
terr <- terr[c(2:5)]
fw <- fw[c(2:5)]

#convert to matrix
fw = as.matrix(fw); marine = as.matrix(marine); terr = as.matrix(terr)

#identify unique stressor combinations (ignoring order) and count frequency of each, returning dataframe
fw_freq <-fw %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

terr_freq <-terr %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

marine_freq <-marine %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

#separate numbers within column in to new column
fw_freq$number <- substr(fw_freq$combo, 1, 2); fw_freq$threats <- substr(fw_freq$combo, 3, 70)
marine_freq$number <- substr(marine_freq$combo, 1, 2); marine_freq$threats <- substr(marine_freq$combo, 3, 70)
terr_freq$number <- substr(terr_freq$combo, 1, 2); terr_freq$threats <- substr(terr_freq$combo, 3, 70)

#remove superfluous stressor column
fw_freq <- fw_freq[c(2:4)]
marine_freq <- marine_freq[c(2:4)]
terr_freq <- terr_freq[c(2:4)]

#Sort in descending order of frequency
fw_freq <- fw_freq[order(fw_freq$Freq, decreasing =TRUE),];
marine_freq <- marine_freq[order(marine_freq$Freq, decreasing =TRUE),];
terr_freq <- terr_freq[order(terr_freq$Freq, decreasing =TRUE),]

#Remove top row with no stressor data
fw_freq = fw_freq[-1,]; marine_freq = marine_freq[-1,]; terr_freq = terr_freq[-1,]

#insert id number for each combination
fw_freq$id <- 1:nrow(fw_freq); marine_freq$id <- 1:nrow(marine_freq);terr_freq$id <- 1:nrow(terr_freq)

#load colour pallette for plots
darj <- wes_palette(3, name = "Darjeeling1", type = "discrete")

#plot
fwp <- ggplot(data=fw_freq, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Freshwater Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

marinep <- ggplot(data=marine_freq, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Marine Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

terrp <- ggplot(data=terr_freq, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Terrestrial Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggarrange(terrp, marinep, fwp, 
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)

