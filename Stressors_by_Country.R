setwd("C:/PhD")


library(rworldmap)
library(RColorBrewer)
library(rworldxtra)                  
library(maps)
library(maptools)
library(raster)
library(ISOcodes)
library(ggmap)
library(dplyr)
library(readxl)
library(Rcolombos)

#read in datasets
lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")

#correct formatting
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)
lpi <- lpi[lpi$Threat_status !="Unknown (no information)",]
lpi <- lpi[lpi$Threat_status !="Unknown (large data set)",]

#count stressors
lpi$no_stress <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

country <- lpi[c(28, 186)]

country = as.data.frame(country)

mean <- tapply(country$no_stress, country$Country, mean)

mean = as.data.frame(mean)
mean$country <- rownames(mean)

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

map <- merge(WorldData, mean, joinCode="NAME", nameJoinColumn="country")

spdf <- joinCountryData2Map(mean, worldmap, joinCode="NAME", nameJoinColumn="country")

numcats <- 11

palette = colorRampPalette(brewer.pal(n=9, name='Reds'))(numcats)

mapParams <- mapCountryData(spdf, nameColumnToPlot="mean", numCats = numcats, colourPalette="heat", 
               mapTitle = "Average Number of Threats by Country", oceanCol='white', missingCountryCol='dark grey', 
               addLegend = FALSE)

do.call(addMapLegend, c(mapParams, legendWidth=1, legendMar = 2))
