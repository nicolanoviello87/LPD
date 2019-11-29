rm(list = ls())
   

install.packages("writexl")

library(wesanderson)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(writexl)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

#create new dataframe with three threat columns; replace NULL values with NA
class <- lpi[c(15, 145:147)]
class[class=="NULL"] <- NA

class = as.data.frame(class)

#create new column with stressor count
class$na_count <- apply(class[2:4], 1, function(x) sum(!is.na(x)))

#subset by class
aves <- class[class[ ,1] == "Aves", ]; rept <- class[class[ ,1] == "Reptilia", ]; 
amph <- class[class[ ,1] == "Amphibia", ]; acti <- class[class[ ,1] == "Actinopterygii", ];
elas <- class[class[ ,1] == "Elasmobranchii", ]; mamm <- class[class[ ,1] == "Mammalia", ];
sarc <- class[class[ ,1] == "Sarcopterygii", ]; ceph <- class[class[ ,1] == "Cephalaspidomorphi", ];
holo <- class[class[ ,1] == "Holocephali", ]; myxi <- class[class[ ,1] == "Myxini", ]

aves <- aves[c(2:5)]; amph <- amph[c(2:5)]; elas <- elas[c(2:5)]; sarc <- sarc[c(2:5)];
holo <- holo[c(2:5)]; rept <- rept[c(2:5)]; acti <- acti[c(2:5)]; mamm <- mamm[c(2:5)];
ceph <- ceph[c(2:5)]; myxi <- myxi[c(2:5)]

#convert to matrices
aves = as.matrix(aves); amph = as.matrix(amph); elas = as.matrix(elas); sarc = as.matrix(sarc); 
holo = as.matrix(holo); rept = as.matrix(rept); acti = as.matrix(acti); mamm = as.matrix(mamm); 
ceph = as.matrix(ceph); myxi = as.matrix(myxi)

#create list of new data frames
x <- list(aves, amph, elas, sarc, holo, rept, acti, mamm, myxi, ceph)

#concatenate stressors and calculate frequencies of unique combinations
aves <- aves %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

amph <- amph %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

elas <- elas %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

sarc <- sarc %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

holo <- holo %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

rept <- rept %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

acti <- acti %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

mamm <- mamm %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

myxi <- myxi %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

ceph <- ceph %>% split(., row(.)) %>% sapply(
  . %>% sort %>% paste(collapse = ", ")
) %>% table(combo = .) %>% data.frame

#separate columns into stressor number and stressor combination
aves$number <- substr(aves$combo, 1, 2); aves$threats <- substr(aves$combo, 3, 70)
aves <- aves[c(2:4)]

amph$number <- substr(amph$combo, 1, 2); amph$threats <- substr(amph$combo, 3, 70)
amph <- amph[c(2:4)]

elas$number <- substr(elas$combo, 1, 2); elas$threats <- substr(elas$combo, 3, 70)
elas <- elas[c(2:4)]

ceph$number <- substr(ceph$combo, 1, 2); ceph$threats <- substr(ceph$combo, 3, 70)
ceph <- ceph[c(2:4)]

acti$number <- substr(acti$combo, 1, 2); acti$threats <- substr(acti$combo, 3, 70)
acti <- acti[c(2:4)]

myxi$number <- substr(myxi$combo, 1, 2); myxi$threats <- substr(myxi$combo, 3, 70)
myxi <- myxi[c(2:4)]

holo$number <- substr(holo$combo, 1, 2); holo$threats <- substr(holo$combo, 3, 70)
holo <- holo[c(2:4)]

mamm$number <- substr(mamm$combo, 1, 2); mamm$threats <- substr(mamm$combo, 3, 70)
mamm <- mamm[c(2:4)]

sarc$number <- substr(sarc$combo, 1, 2); sarc$threats <- substr(sarc$combo, 3, 70)
sarc <- sarc[c(2:4)]

rept$number <- substr(rept$combo, 1, 2); rept$threats <- substr(rept$combo, 3, 70)
rept <- rept[c(2:4)]


#Sort in descending order of frequency
aves <- aves[order(aves$Freq, decreasing =TRUE),]; rept <- rept[order(rept$Freq, decreasing =TRUE),]
sarc <- sarc[order(sarc$Freq, decreasing =TRUE),]; mamm <- mamm[order(mamm$Freq, decreasing =TRUE),]
holo <- holo[order(holo$Freq, decreasing =TRUE),]; myxi <- myxi[order(myxi$Freq, decreasing =TRUE),]
acti <- acti[order(acti$Freq, decreasing =TRUE),]; ceph <- ceph[order(ceph$Freq, decreasing =TRUE),]
elas <- elas[order(elas$Freq, decreasing =TRUE),]; amph <- amph[order(amph$Freq, decreasing =TRUE),]

#Remove top row with no stressor data
aves = aves[-1,]; sarc = sarc[-1,]; amph = amph[-1,]; rept = rept[-1,]; elas = elas[-1,];
acti = acti[-1,]; holo = holo[-1,]; myxi = myxi[-1,]; mamm = mamm[-1,]; ceph = ceph[-1,]

#insert id number for each combination
aves$id <- 1:nrow(aves); sarc$id <- 1:nrow(sarc); holo$id <- 1:nrow(holo); acti$id <- 1:nrow(acti); 
elas$id <- 1:nrow(elas); rept$id <- 1:nrow(rept); mamm$id <- 1:nrow(mamm); myxi$id <- 1:nrow(myxi); 
ceph$id <- 1:nrow(ceph); amph$id <- 1:nrow(amph)

#load colour pallette for plots
darj <- wes_palette(3, name = "Darjeeling1", type = "discrete")

ggplot(data=aves, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Aves Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=acti, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Acti Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=amph, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Amph Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=ceph, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Ceph Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=elas, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Elas Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=holo, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Holo Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=mamm, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Mamm Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=rept, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Rept Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

ggplot(data=sarc, aes(x = reorder(id, -Freq), y = Freq, fill=number, ylab="Stressor Frequency"))+
  geom_bar(stat="identity", width = 0.8) + theme_light(base_size = 18) + 
  labs(x = "Stressor ID", y = "Number of Sarc Time Series", size = 20) +
  geom_text(aes(label=id), position=position_dodge(width=1), vjust=-0.25) +
  theme(axis.text.x = element_text(size = 12), axis.text.y=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.text.x = element_blank(), axis.text.y = element_text(angle = 90)) +
  theme(legend.text = element_text(size=12),legend.key.size = unit(1, "cm"), 
        legend.title = element_blank(), legend.position = c(0.85, 0.7)) +
  scale_fill_manual(legend, values=darj)

write_xlsx(aves, "C:/PhD/Aves_Stressors.xlsx")
write_xlsx(acti, "C:/PhD/Acti_Stressors.xlsx")
write_xlsx(amph, "C:/PhD/Amph_Stressors.xlsx")
write_xlsx(ceph, "C:/PhD/Ceph_Stressors.xlsx")
write_xlsx(elas, "C:/PhD/Elas_Stressors.xlsx")
write_xlsx(holo, "C:/PhD/Holo_Stressors.xlsx")
write_xlsx(mamm, "C:/PhD/mamm_Stressors.xlsx")
write_xlsx(rept, "C:/PhD/Rept_Stressors.xlsx")
write_xlsx(sarc, "C:/PhD/Sarc_Stressors.xlsx")
