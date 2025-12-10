

# DEFINING **INTERMEDIATE** BIOMASS PRIORS 

# Define priors by following fixed rules
# Ferrer et al. (2022)
# Giron-Nava et al. (2019)

# ----------- Getting Started -----------

# Set working directory:
setwd('/Users/EricaFerrer/Desktop/NCEAS Postdoc Work (2025-2027)/OHI - Test CMSY')

# Clear environment:
rm(list = ls())

# ----------- Define Functions -----------

# Rolling average:
ma <- function(x){
  x.1    <-   stats::filter(x, rep(1/3,3), sides=1)
  x.1[1] <- x[1]
  x.1[2] <- (x[1]+x[2])/2
  return(x.1)
}

# Turns all entries lower-case
lowercase_function<- function(x){
  if(is.character(x)){
    tolower(x)
  } else if (is.factor(x)){
    tolower(as.character(x))
  } else {
    x
  }
}

# ----------- Read Data -----------

metadata = read.csv("Data_New_Stocks/File2_Abbreviated_UNDER_CONSTRUCTION_Oct 23 2025.csv")   # Load File 2: metadata
metadata<- as.data.frame(lapply(metadata, lowercase_function))
# metadata = read.csv("Data_New_Stocks/File2_UNDER_CONSTRUCTION_Oct 22 2025.csv")   # Load File 2: metadata

timeseries_data = read.csv("Data_New_Stocks/File1_UNDER_CONSTRUCTION_Oct 22 2025.csv")  # Load File 1: fishery timeseries
timeseries_data<- as.data.frame(lapply(timeseries_data, lowercase_function))
# data.ts = read.csv("Data/stocks_landings_File1.csv")  # Load File 1: fishery timeseries

# ----------- Prep Data Frames -----------

colnames(timeseries_data)[2]<- "year"   #ERICA PAY ATTENTION TO THESE COLUMNS AS THEY MAY CHANGE! - Oct 23
colnames(timeseries_data)[4]<- "catch"  #ERICA PAY ATTENTION TO THESE COLUMNS AS THEY MAY CHANGE! - Oct 23

# Go stock by stock, estimate ranges for intermediate and final biomass priors:
n <- NROW(metadata)

int_b.low = rep(NA,n)
int_b.hi = rep(NA,n)
end_b.low = rep(NA,n)
end_b.hi = rep(NA,n)

threshold = 0.3 # Threshold of increment of a 30%

#i=1

# ----------- Run the Loop - Generate Priors -----------

for (i in 1:length(metadata$Stock)){
  
  if(metadata$Sufficient_No_Years[i] == 1){
    
    stock = as.character(metadata$Stock[i])
    stock_ID = which(timeseries_data$Stock == stock)
    stock_timeseries = timeseries_data[stock_ID,]
    
    # Calculate 3 years running average:
    stock_timeseries$catch = ma(stock_timeseries$catch)
    #stock_timeseries$catch[i] = forecast::ma(stock_timeseries$catch[i], order=3)
    
    # Calculate the catch contrast coefficient (C) between the intermediate and starting years
    int_yr = metadata$int_yr[i]
    int_yr.ID = which(stock_timeseries$year == int_yr)
    
    starting_catch = stock_timeseries$catch[1]
    intermediate_catch = stock_timeseries$catch[int_yr.ID]
    
    C = (intermediate_catch - starting_catch) / starting_catch
   
    # Example 1: C = (10000 - 9000) / 9000 = 0.11
    # Example 2: C = (9000 - 10000) / 9000 = -0.11
    # Example 3: C = (20000 - 9000) / 9000 = 1.22
    # Example 4: C = (1000 - 9000) / 9000 = -0.889
    
    # Ex 1: If intermediate_catch is SLIGHTLY greater than starting_catch then C will be small and positive
    # Ex 2: If intermediate_catch is SLIGHTLY less than starting_catch then C will be small and negative
    # Ex 3: If intermediate_catch is MUCH greater than starting_catch, then C will be large and positive
    # Ex 4: If intermediate_catch is MUCH less than starting_catch, then C will be small and negative (increasingly close to -1)
    # If intermediate_catch is exactly equal to starting_catch then C will be 0
    
    # Intermediate Biomass Priors:
    if(abs(C) <= threshold){
      int_b.low[i] = metadata$st_b.low[i]
      int_b.hi[i] = metadata$st_b.hi[i]
      # Stock is doing OKAY well by comparison, similar to starting biomass

      } else if(C < threshold){
      int_b.low[i] = metadata$st_b.low[i] - 0.1
      int_b.hi[i] = metadata$st_b.hi[i] - 0.1
      # Stock isn't doing so well by comparison, less than starting biomass
    
      } else if(C > threshold){
      int_b.low[i] = metadata$st_b.low[i] + 0.1
      int_b.hi[i] = metadata$st_b.hi[i] + 0.1
      # Stock IS doing so well by comparison, more than starting biomass
    
      }
    
    # Get coefficient and ranges for final biomass
    max.yr.i <- which.max(stock_timeseries$catch[4:(length(stock_timeseries$catch)-3)])+3
    
    # Calculate the catch contrast coefficient between the final and max_catch years (C2): Contrast final year against maximum year
    max_catch = stock_timeseries$catch[max.yr.i]
    end_catch = stock_timeseries$catch[nrow(stock_timeseries)]
    
    C2 = end_catch / max_catch
    # C2 is bounded 0 to 1 because:
    # If end_catch is greater than max_catch then it is max_catch and will be bounded at 1
    # If end_catch is less than max_catch then C2 will be less than 1
    # If end_catch is MUCH less than max_catch then C2 will be MUCH less than 1, closer to 0
    
    # If Contrast is high C2 > 0.8 (end_catch similar to max_catch), increase the upper limit of the original biomass
    if(C2 > 0.8){
      
      end_b.low[i] = metadata$st_b.low[i]
      end_b.hi[i] = min(c(metadata$st_b.hi[i] + 0.1, 1)) # Upper boundary of 1
      
    # If Contrast is very low, decrease it by 1
    
      } else if(C2 < 0.5){
      end_b.low[i] = 0.01
      end_b.hi[i] = 0.4
      
      if(end_b.hi[i] == 0.4){
        if(C2 < 0.05) {end_b.hi[i] <- 0.1
        
        } else if(C2 < 0.15) {
          end_b.hi[i] <- 0.2
        
          } else if(C2 < 0.35) {
          end_b.hi[i] <- 0.3
        
          } else {end_b.hi[i] <- 0.4}
      
        }
      
    } else {
      end_b.low[i] = metadata$st_b.low[i]
      end_b.hi[i] = metadata$st_b.hi[i]
    }
    
    # if default end biomass is low (0.01 - 0.4), check whether the upper bound should be lower than 0.4 for depleted stocks
  }
}

# ----------- Compile and Save -----------

Intermediate_Biomass_Priors_df = data.frame(Stock= metadata$Stock, int_b.low, int_b.hi, end_b.low, end_b.hi)

# I actually think this should not be called "Initial Conditions", it should be called "Intermediate Conditions" but I'll leave it for now.
write.csv(Intermediate_Biomass_Priors_df, file = 'Data_New_Stocks/Priors_Initial Conditions_Oct 23 2025.csv', row.names= FALSE)
