#read in original csv file
LPI <- read_csv("LPI/lpi_original.csv")

#simple wide to long data format conversion; new dataframe renamed
LPI_Long <- gather(LPI, Year, Pop, 65:133)

#export new csv file at long format
write.csv(LPI2, file = "LPI_Long.csv") #data nissing with this function
