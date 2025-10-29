################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# ERICA's NOTES / EDITS I MADE (Oct 29 2025):
# I changed the working directory on files as necessary to load in the right functions, scripts, and data.
# The raw data files come directly from Juan Carlos and are saved to the /home/shares/ohi/OHI_GOC/_raw_data directory, 
# The edited script files, including this one, are save through the home/ferrer/ohi_goc_prep directory
# I also edited the last couple of lines to include select("species_name") and saved this new dataframe to the home/ferrer/ohi_goc_prep directory
#
################################################################################

## SET UP ######################################################################

# Clear environment - EMF, Oct 29 2025
rm(list=ls(all=T))

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

# Load and define functions ----------------------------------------------------  
source(here("/home/ferrer/ohi_goc_prep/FP/EMF_CONAPESCA_Exploration/JC_Scripts_EMF_Edited/00_setup.R"))

# Load data --------------------------------------------------------------------
old <- readRDS(here("/home/shares/ohi/OHI_GOC/_raw_data/CONAPESCA/d2025/SharedByJuanCarlos/mex_landings/data/clean/", "mex_conapesca_avisos_2000_2019.rds")) |> 
  filter(year_cut <= 2017)

apertura <- readRDS(here("/home/shares/ohi/OHI_GOC/_raw_data/CONAPESCA/d2025/SharedByJuanCarlos/mex_landings/data/clean/", "mex_conapesca_apertura_2018_present.rds"))

months <- tibble(month_cut = c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"),
                 month = 1:12) 

## PROCESSING ##################################################################

# Combine and select columns ---------------------------------------------------
landings <- bind_rows(old,
                      apertura) |> 
  left_join(months, by = "month_cut")

# Fix dates --------------------------------------------------------------------
landings_fixed_dates <- landings %>%
  mutate(period_start_fixed = fix_dates(data = .,
                                        date_to_fix = period_start),
         period_end_fixed = fix_dates(data = .,
                                      date_to_fix = period_end),
         receipt_date_fixed = fix_dates(data = .,
                                        date_to_fix = receipt_date))


final_landings_edited <- landings_fixed_dates |> 
  select(state,
         office_name,
         landing_site,
         landing_site_key,
         year = year_cut,
         month = month,
         receipt_date_fixed,
         period_end_fixed,
         period_start_fixed,
         eu_rnpa,
         eu_name = economic_unit,
         fleet,
         acuaculture_production,
         vessel_rnpa,
         vessel_name,
         main_species_group,
         species_name,
         landed_weight,
         live_weight,
         value)

## EXPORT ######################################################################

# Export file ------------------------------------------------------------------
saveRDS(object = final_landings_edited,
        file = here("/home/ferrer/ohi_goc_prep/FP/EMF_CONAPESCA_Exploration/JC_Scripts_EMF_Edited/", "mex_landings_2000_present_with_Species.rds"))
