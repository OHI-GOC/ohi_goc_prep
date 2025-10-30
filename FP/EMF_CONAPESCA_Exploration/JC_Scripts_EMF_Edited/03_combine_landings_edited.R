################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# ERICA's EDITS - Oct 29 2025
# The raw data and output data for this file is included in the /home/shares/ohi/OHI_GOC/_raw_data/CONAPESCA/d2025/SharedByJuanCarlos/ directory
# Small files (including this script) is included in the /home/ferrer/ohi_goc_prep directory
# I made a few edits here and there with the objective of extracting the species names from the landings data.

################################################################################

## SET UP ######################################################################

# Clear environment
rm(list=ls(all=T))

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  tidyverse
)

library(dplyr)

# Load and define functions ----------------------------------------------------
source(here("/home/ferrer/ohi_goc_prep/FP/EMF_CONAPESCA_Exploration/JC_Scripts_EMF_Edited", "00_setup_edited.R"))

# Load data --------------------------------------------------------------------
avisos_old_landings <- readRDS(here("/home/shares/ohi/OHI_GOC/_raw_data/CONAPESCA/d2025/SharedByJuanCarlos/mex_landings/data/clean/", "mex_conapesca_avisos_2000_2019.rds")) |> 
  filter(year_cut <= 2017)
# 8270314 obs.

apertura_newer_landings <- readRDS(here("/home/shares/ohi/OHI_GOC/_raw_data/CONAPESCA/d2025/SharedByJuanCarlos/mex_landings/data/clean/", "mex_conapesca_apertura_2018_present.rds"))
# 3883565 obs.

months <- tibble(month_cut = c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"),
                 month = 1:12) 

## PROCESSING ##################################################################

# Combine and select columns ---------------------------------------------------
landings <- bind_rows(avisos_old_landings,
                      apertura_newer_landings) |> 
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
         species_name, # I added this column, Erica - Oct 29 2025
         landed_weight,
         live_weight,
         value)

# 12,153,879 obs. 

## EXPORT ######################################################################

# Export file ------------------------------------------------------------------
# This creates all sorts of issues because it's so big.
saveRDS(final_landings_edited, 
        file = "/home/shares/ohi/OHI_GOC/goal_prep/fis/v2025/int/juan_carlos/mex_landings_2000_present_EMF_edited.rds")
