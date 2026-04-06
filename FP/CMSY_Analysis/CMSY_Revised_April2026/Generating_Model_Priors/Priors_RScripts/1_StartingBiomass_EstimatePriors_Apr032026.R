
# DEFINING **INITIAL** BIOMASS PRIORS 
# I would classify this as under construction (as of April 3 2026). See if I can get more FAO data to back this up.

# Define priors of species standing stock biomass by following fixed rules
# Ferrer et al. (2022)
# Giron-Nava et al. (2019)

# ----------- Getting Started -----------

# Clear environment & set working directory:
rm(list = ls())
setwd('/home/ferrer/ohi_goc_prep/FP/')

# Load libraries:
library(stringr)

# Load data
FAO_data = read.csv('/home/ferrer/ohi_goc_prep/FP/CMSY_Analysis/CMSY_Revised_April2026/Generating_Model_Priors/Priors_Data/FAOMexicoData_19762023_downloadedApr042026.csv')
# FAO timeseries data organized with Stock (taxa) as # columns and years as # rows.
  
# We've got to do a little work on these data before they're ready to use.
colnames(FAO_data)

# Clean column names
clean_names <- colnames(FAO_data) %>%
  # Remove trailing periods
  str_replace_all("\\.$", "") %>%
  # Replace multiple periods with a single underscore
  str_replace_all("\\.+", "_") %>%
  # Remove leading X_ for years
  str_replace("^X_(\\d+)$", "\\1") %>%
  # Convert to lowercase
  tolower()

# Apply cleaned names to the dataframe
colnames(FAO_data) <- clean_names

# Delete the columns that start with s or s_, we don't need these.
FAO_data <- FAO_data %>%
  select(-starts_with("s"))

# Filter out aquaculture production:
table(FAO_data$detailed_production_source_name)
FAO_data<- FAO_data %>%
  filter(detailed_production_source_name=="Capture production")

# Delete other columns we don't need:
colnames(FAO_data)
FAO_data<- FAO_data %>%
  select(-c(country_name, fao_major_fishing_area_name, detailed_production_source_name,
            unit_name, unit))

# Transpose these data such that years are rows per taxa (and rename species column):
FAO_data_wide <- FAO_data %>%
  # Rename species column
  rename(estimated_family_name = asfis_species_scientific_name) %>%
  
  # Step 1: go long
  pivot_longer(
    cols = -estimated_family_name,
    names_to = "year",
    values_to = "value"
  ) %>%
  
  # Step 2: clean year
  mutate(year = as.integer(year)) %>%
  
  # Step 3: go wide again (species become columns)
  pivot_wider(
    names_from = estimated_family_name,
    values_from = value
  )

FAO_data_wide$year <- as.numeric(FAO_data_wide$year)

# Make sure zeros are actually NAs:
FAO_data_wide <- FAO_data_wide %>%
  mutate(across(-year, ~ na_if(., 0)))


# ----------- Estimate !-----------

# 1) Calculate st_b.low and st_b.hi
# All differences are calculated with a margin of 20 %
#   i.   New fishery (0.6 - 0.9): No records before 1995
#   ii.  Healthy (0.5 - 0.8): Historical max_biomass < mean_biomass from 2000-2002
#   iii. Sustainable (0.4 - 0.6): Historical max_biomass = mean_biomass from 2000-2002
#   iv.  Depleted (0.2 - 0.5): Historical max_biomass > mean_biomass of 2000-2002

year = FAO_data_wide[, 1] # Years 1950-present
year <- as.numeric(FAO_data_wide[, 1])

hist_years = which(year < 2000)  # Historical years
present_years = which(year >= 2000 & year <= 2002) # "Starting" years, 2000-2002

st_b.low = numeric()
st_b.hi = numeric()
state = character()

Starting_Biomass_Priors_df = data.frame()

for (i in 2:ncol(FAO_data_wide)){
  
  c = i-1
  
  timeseries = FAO_data_wide[[i]]
  timeseries_historic = sort(timeseries[hist_years], decreasing = T)
  
  hist_max = mean(timeseries_historic[1:3], na.rm = T) # Historical maximum (3 max years)
  starting_mean = mean(timeseries[present_years], na.rm = T) # Present mean
  
  Coefficient = (starting_mean - hist_max) / starting_mean
  # If starting_mean is SLIGHTLY GREATER than hist_max then Coefficient will be positive and closer to 0 than 1
  # If starting_mean is MUCH GREATER than hist_max, then Coefficient will be positive and close to 1
  # If starting_mean is SLIGHTLY LESS than hist_max then Coefficient will be negative and closer to 0 than 1
  # If starting_mean is MUCH LESS than hist_max, then Coefficient will be negative and greater than 1
  
  th = 0.3 # Coefficient threshold
  
  # New fishery (no evidence of fishery circa 2000)
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

Starting_Biomass_Priors_df = data.frame(Stock = names(FAO_data_wide)[2:ncol(FAO_data_wide)], st_b.low, st_b.hi, Status = state)

# Export dataframe
write.csv(Starting_Biomass_Priors_df, row.names= FALSE,
          file = "/home/ferrer/ohi_goc_prep/FP/CMSY_Analysis/CMSY_Revised_April2026/Generating_Model_Priors/Generated_Priors/1_StartingBiomassPriors_fromFAO_Apr042026.csv")
 