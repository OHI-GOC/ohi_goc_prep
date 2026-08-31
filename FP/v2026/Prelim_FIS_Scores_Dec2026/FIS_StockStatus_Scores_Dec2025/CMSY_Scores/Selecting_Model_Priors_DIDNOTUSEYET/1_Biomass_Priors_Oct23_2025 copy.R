
# DEFINING **INITIAL** BIOMASS PRIORS 

# Define priors of species standing stock biomass by following fixed rules
# Ferrer et al. (2022)
# Giron-Nava et al. (2019)

# Clear environment & set working directory:
rm(list = ls())
setwd('/Users/EricaFerrer/Desktop/NCEAS Postdoc Work (2025-2027)/OHI - Test CMSY')

# Load data
FAO_data = read.csv('Data_New_Stocks/FAO_Historic_Timeseries_copy.csv')
# FAO timeseries data organized with Stock (taxa) as # columns and years as # rows.
  
# 1) Calculate st_b.low and st_b.hi
# All differences are calculated with a margin of 20 %
#   i.   New fishery (0.6 - 0.9): No records before 1995
#   ii.  Healthy (0.5 - 0.8): Historical max_biomass < mean_biomass from 2000-2002
#   iii. Sustainable (0.4 - 0.6): Historical max_biomass = mean_biomass from 2000-2002
#   iv.  Depleted (0.2 - 0.5): Historical max_biomass > mean_biomass of 2000-2002

year = FAO_data[, 1] # Year 1999

hist_years = which(year < 2000)  # Historical years
present_years = which(year >= 2000 & year <= 2002) # "Present" years, 2000-2002

st_b.low = numeric()
st_b.hi = numeric()
state = character()

Starting_Biomass_Priors_df = data.frame()

for (i in 2:ncol(FAO_data)){
  
  c = i-1
  
  timeseries = FAO_data[,i]
  timeseries_historic = sort(timeseries[hist_years],decreasing = T)
  
  hist_max = mean(timeseries_historic[1:3], na.rm = T) # Historical maximum (3 max years)
  present_mean = mean(timeseries[present_years], na.rm = T) # Present mean
  
  Coefficient = (present_mean - hist_max) / present_mean
  # If present_mean is SLIGHTLY GREATER than hist_max then Coeff will be positive and closer to 0 than 1
  # If present_mean is MUCH GREATER than hist_max, then Coeff will be positive and close to 1
  # If present_mean is SLIGHTLY LESS than hist_max then Coeff will be negative and closer to 0 than 1
  # If present_mean is MUCH LESS than hist_max, then C will be negative and greater than 1
  
  th = 0.3 # Coefficient threshold
  
  # New fishery
  if(is.na(Coefficient)){
    st_b.low[c] = 0.6
    st_b.hi[c] = 0.9
    state[c] = "new"
  
    } else{
    # Healthy
    if(Coefficient > th){
      st_b.low[c] = 0.5
      st_b.hi[c] = 0.8
      state[c] = "healthy"
    }
    # Sustainable
    if(abs(Coefficient) <= th){
      st_b.low[c] = 0.3
      st_b.hi[c] = 0.5
      state[c] = "sustainable"
    }
    # Depleted
    if(Coefficient < -th){
      st_b.low[c] = 0.1
      st_b.hi[c] = 0.3
      state[c] = "depleted"
    }
  }
}

Starting_Biomass_Priors_df = data.frame(Stock = names(FAO_data)[2:ncol(FAO_data)], st_b.low, st_b.hi, Status = state)

# Export dataframe
write.csv(Starting_Biomass_Priors_df, file = 'Data_New_Stocks/Priors_Starting Biomass_from FAO data_Oct 23 2025.csv', row.names= FALSE)
 