rm(list = ls()
   
library(wesanderson)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

#read in xlsx file
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")

#create new dataframe with three threat columns; replace NULL values with NA
combo <- lpi[c(145:147)]
combo[combo=="NULL"] <- NA
combo$non_na <- apply(combo[1:3], 1, function(x) sum(!is.na(x)))

#convert to matrix
combo = as.matrix(combo)

#convert to matrix
d = as.matrix(combo)

#concatenate stressors and calculate frequencies of unique combinations
combos <- d %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = " ")
) %>% table(combo = .) %>% data.frame

#separate columns into stressor number and stressor combination
combos$number <- substr(combos$combo, 1, 2); combos$threats <- substr(combos$combo, 3, 60)
combos <- combos[c(2:4)]

#Sort in descending order of frequency
combos <- combos[order(combos$Freq, decreasing =TRUE),]

darj <- wes_palette(4, name = "Darjeeling1", type = "discrete")

#Remove top row with no stressor data
combos = combos[-1,]

#insert id number for each combination
combos$id <- 1:nrow(combos)

darj <- wes_palette(3, name = "Darjeeling1", type = "discrete")

ggplot(data=combos, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)
