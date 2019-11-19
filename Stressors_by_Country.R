install.packages("raster")
install.packages("ISOcodes")
install.packages("ggmap")
install.packages("maps")
install.packages("maptools")
install.packages("rworldxtra")
install.packages("Rcolombos")
install.packages("rworldmap")
library(rworldmap)
library(RColorBrewer)
library(rworldxtra)                  
library(maps)
library(maptools)
library(raster)
library(ISOcodes)
library(ggmap)

lpi <- read_xlsx("C:/PhD/LPI_Data/LPI.xlsx")
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

lpi$non_na <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

country <- lpi[c(28, 186)]

country = as.data.frame(country)

mean <- tapply(country$non_na, country$Country, mean)

mean = as.data.frame(mean)
mean$country <- rownames(mean)

map <- merge(worldmap, mean, joinCode="NAME", nameJoinColumn="country")

spdf <- joinCountryData2Map(mean, worldmap, joinCode="NAME", nameJoinColumn="country")

numcats <- 11

palette = colorRampPalette(brewer.pal(n=9, name='Reds'))(numcats)

mapParams <- mapCountryData(spdf, nameColumnToPlot="mean", numCats = numcats, colourPalette="heat", 
               mapTitle = "Average Number of Threats by Country", oceanCol='white', missingCountryCol='dark grey', 
               addLegend = FALSE)

do.call(addMapLegend, c(mapParams, legendWidth=1, legendMar = 2))
