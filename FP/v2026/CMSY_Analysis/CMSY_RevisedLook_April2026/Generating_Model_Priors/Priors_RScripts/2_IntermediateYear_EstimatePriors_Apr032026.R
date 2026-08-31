# SELECTING YEARS OF INTERMEDIATE CATCH LEVELS, aka "INTERMEDIATE YEAR"

# Script to plot raw time series per stock and select intermediate year
# Ferrer et al. (2022)
# Giron-Nava et al. (2019)

# ----------- Getting Started -----------

# Clear environment & set working directory:
rm(list = ls())
setwd('/home/ferrer/ohi_goc_prep/FP/')

# Load libraries:
library(dplyr)

# Load in data and rename columns as needed:

# File 1 - timeseries file
Landings_Data = read.csv("/home/ferrer/ohi_goc_prep/FP/CMSY_Analysis/CMSY_Revised_April2026/Model_Input_Files/File1_CMSY_LandingsPerStock_Apr042026.csv")
# 2252 obs., 7 vars.
# Exclude all stock years from 2025 as this Landings_Data is currently incomplete
# Landings_Data <- Landings_Data %>% filter(year < 2025)

colnames(Landings_Data)[3]<- "catch_kg"  # Total landed biomass, in kilograms

# File 2 - metadata file
Stock_Metadata = read.csv("/home/ferrer/ohi_goc_prep/FP/CMSY_Analysis/CMSY_Revised_April2026/Model_Input_Files/File2_CMSY_StockMetadata_UNDERCONSTRUCTION_Apr032026.csv")
# 95 obs., 39 vars.


# Select different stocks:
stocks = Stock_Metadata$Stock
int_yr = numeric()
StartYear = numeric()
EndYear = numeric()
Sufficient_No_Years <- logical()  # TRUE/FALSE
length_timeseries<- length(unique(Landings_Data$year))-1


# ----------- Estimate! -----------

for (i in 1:length(stocks)){
  
  Stock = as.character(stocks[i])
  Stock_ID =  which(Landings_Data$Stock == Stock)
  Stock_data = Landings_Data[Stock_ID, ]
  
  # Export min/max year:
  n.data <- NROW(Stock_data)
  
  # StartYear[i] = min(Stock_data$year)
  # EndYear[i] = max(Stock_data$year)
  # duration = EndYear[i] - StartYear[i]
  
  
  if (n.data > 10){
    Sufficient_No_Years[i] = TRUE
    
    # Plot
    plot(catch_kg ~ year, Stock_data, type = 'l', lwd = 2,
         xlab = "Year", ylab = "Total catch (kg)",
         main = Stock[1])
    points(catch_kg ~ year, Stock_data)
    abline(v = max(Stock_data$year) - 1, lty = 2)
    #abline(v = 2015, lty = 2)
    abline(v = min(Stock_data$year) + 1, lty = 2)
    
    # Plot maximum
    m = max(Stock_data$catch_kg[2:length_timeseries])
    id.max = which(Stock_data$catch_kg == m)
    points(catch_kg[id.max] ~ year[id.max], Stock_data, col = 'red', pch = 19)
    
    # Interactive selection of the intermediate year
    int_yr[i] = round(as.numeric(locator(1))[1])
    int_yr.ID = which(Stock_data$year == int_yr[i])
    points(catch_kg[int_yr.ID] ~ year[int_yr.ID], Stock_data, col = 'blue', pch = 19)
    
    StartYear[i] = min(Stock_data$year)
    EndYear[i] = max(Stock_data$year)
    
  } else {
    Sufficient_No_Years[i] = FALSE
    int_yr[i] = NA
    StartYear[i] = NA
    EndYear[i] = NA
  }
  
}

Selection_Results<- data.frame(Stock = Stock_Metadata$Stock,
                     int_yr = int_yr,
                     StartYear = StartYear,
                     EndYear = EndYear,
                     Sufficient_No_Years = Sufficient_No_Years)

write.csv(Selection_Results, "CMSY_Analysis/CMSY_Revised_April2026/Generating_Model_Priors/Generated_Priors/2_IntermediateYearsPriors_fromLandings_Apr042026.csv", row.names = F)
dev.off()
