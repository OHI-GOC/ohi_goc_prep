# Functions
source(here::here("/home/ferrer/ohi_goc_prep/FP/EMF_CONAPESCA_Exploration/JC_Scripts_EMF_Edited", "helpers_clean_dates.R"))
source(here::here("/home/ferrer/ohi_goc_prep/FP/EMF_CONAPESCA_Exploration/JC_Scripts_EMF_Edited", "helpers_clean_eu_names.R"))
source(here::here("/home/ferrer/ohi_goc_prep/FP/EMF_CONAPESCA_Exploration/JC_Scripts_EMF_Edited", "helpers_fix_rnpa.R"))
# String-fixing function
str_fix <- function(x) {
  x <- str_to_upper(x)        # String to upper
  x <-
    str_trim(x)            # Trim leading and trailing whites paces
  x <- str_squish(x)          # Squish repeated white spaces
  return(x)                   # Return clean string
}

# Design speed function
design_speed <- function(engine_power_hp) {
  engine_power_kwh <- (engine_power_hp / 1.34102)                           # Convert to kwh
  10.4818 + (1.2e-3 * engine_power_kwh) - (3.84e-8 * engine_power_kwh ^ 2)  # Calculate design speed
}
