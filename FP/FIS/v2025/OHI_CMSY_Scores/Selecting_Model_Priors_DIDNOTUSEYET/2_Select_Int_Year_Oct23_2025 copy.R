# SELECTING YEARS OF INTERMEDIATE CATCH LEVELS, aka "INTERMEDIATE YEAR"

# Script to plot raw time series per stock and select intermediate year
# Ferrer et al. (2022)
# Giron-Nava et al. (2019)

# Clear environment & set working directory:
rm(list = ls())
setwd('/Users/EricaFerrer/Desktop/NCEAS Postdoc Work (2025-2027)/OHI - Test CMSY')

# Load libraries:
library(dplyr)

# Load data:
data = read.csv("Data_New_Stocks/File1_UNDER_CONSTRUCTION_Oct 22 2025.csv")   # File 1 - timeseries file
# Exclude all stock years from 2025 as this data is currently incomplete
data <- data %>% filter(Year < 2025)

cinfo = read.csv("Data_New_Stocks/File2_UNDER_CONSTRUCTION_Oct 22 2025.csv")   # File 2 - metadata file

colnames(data)[2]<- "year"  # Year
colnames(data)[4]<- "catch"  # Total landed biomass

# Select different stocks:
stocks = cinfo$Stock
int_yr = numeric()
StartYear = numeric()
EndYear = numeric()
Sufficient_No_Years = numeric()

#pdf('output/intermediate_years.pdf', height = 5,width = 7)
# pdf("IntermediateYears.pdf", height = 5, width = 7)

for (i in 1:length(stocks)){
  
  Stock = as.character(stocks[i])
  Stock_ID =  which(data$Stock == Stock)
  Stock_data = data[Stock_ID, ]
  
  # Export min/max year:
  n.data <- NROW(Stock_data)
  
  # StartYear[i] = min(Stock_data$year)
  # EndYear[i] = max(Stock_data$year)
  # duration = EndYear[i] - StartYear[i]
  
  
  if (n.data > 10){
    Sufficient_No_Years[i] = 1
    
    # Plot
    plot(catch ~ year, Stock_data, type = 'l', lwd = 2,
         xlab = "Year", ylab = "Total catch (kg)",
         main = Stock[1])
    points(catch ~ year, Stock_data)
    abline(v = max(Stock_data$year) - 1, lty = 2)
    #abline(v = 2015, lty = 2)
    abline(v = min(Stock_data$year) + 1, lty = 2)
    
    # Plot maximum
    m = max(Stock_data$catch[2:16])
    id.max = which(Stock_data$catch == m)
    points(catch[id.max] ~ year[id.max], Stock_data, col = 'red', pch = 19)
    
    # Interactive selection of the intermediate year
    int_yr[i] = round(as.numeric(locator(1))[1])
    int_yr.ID = which(Stock_data$year == int_yr[i])
    points(catch[int_yr.ID] ~ year[int_yr.ID], Stock_data, col = 'blue', pch = 19)
    
    StartYear[i] = min(Stock_data$year)
    EndYear[i] = max(Stock_data$year)
    
  } else {
    Sufficient_No_Years[i] = 0
    int_yr[i] = NA
    StartYear[i] = NA
    EndYear[i] = NA
  }
  
}

Selection_Output <- data.frame(Stock = cinfo$Stock,
                     int_yr = int_yr,
                     StartYear = StartYear,
                     EndYear = EndYear,
                     Sufficient_No_Years = Sufficient_No_Years)

write.csv(Selection_Output, "Data_New_Stocks/Priors_Intermediate Years_Oct 23 2025.csv", row.names = F)
dev.off()
