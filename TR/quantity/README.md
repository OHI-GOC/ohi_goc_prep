# TR Quantity Updated Scripts

This folder is a cleaned, outlined version of the original TR quantity workflow for clarity.  We now have an ordering of scripts and each one has a table of contents that is also numbered.  This was updated on July 30th, 2026.

## Order to run scripts

1. `script1_denue_prep.qmd`  
   Sequential DENUE prep for 2019-2026 (Nov releases when available; May for 2025-2026).  
   Writes accommodation / recreation intermediates used later.

2. `script2_arrivals_prep.qmd`  
   SECTUR Datatur hotel arrivals cleaning and prep.  
   Writes yearly CSVs and domestic/international arrivals shapefiles.  
   Also explores observed SECTUR scores before gapfilling.

3. `script3_gapfilling_arrivals_by_employees.qmd`  
   Uses DENUE accommodation employees (Script 1) + SECTUR arrivals (Script 2) to gapfill missing municipio-year arrivals.  
   Writes `arrivals_filled_by_DENUE_accommodation_employees_2019_2026.csv` (and the updated/censored version).

4. `script4_tr_quantity.qmd`  
   Combines SECTUR + gapfilled arrivals, aggregates to OHI regions, computes current-status scores (2024 baselines), and writes final quantity outputs.

## Important data caveats

1. **DENUE 2025-2026:** I use May releases (`0525`, `0526`) because November 2025 was not usable and November 2026 is after our submission window. Prefer November for earlier years.
2. **SECTUR 2026:** the current Datatur download only goes through half of 2026, so annual 2026 totals are incomplete as of now. Scripts use complete SECTUR annual years **2013-2025** (and **2019-2025** in the gapfill join), while DENUE can still go through **2019-2026**.
3. **Score baselines stay 2024, per the Plán México 2030 document targets:** domestic target = 2024 arrivals x 1.098; international target = 2024 arrivals x 1.30.

## What each script needs

| Script | Main inputs | Main outputs |
|---|---|---|
| 1 | Raw DENUE CSVs under `_raw_data/inegi_denue/d2025/` | `update_alojamientos_2019_2026_goc_50km.shp` (+ culture/recreation intermediates) |
| 2 | `SECTUR_Datatur_Hoteles/SECTUR_resaved_032426/*.csv` | `hotel_data_cleaned_yearly_2013_2025.csv`, `domestic_arrivals_sectur_goc.shp`, `international_arrivals_sectur_goc.shp` |
| 3 | Script 1 alojamientos + Script 2 arrivals shapefiles | `arrivals_filled_by_DENUE_accommodation_employees_2019_2026.csv` |
| 4 | Script 2 yearly SECTUR + Script 3 gapfilled arrivals | regional quantity scores / plotly outputs under `goal_prep/tr/v2025/` |

## Notes 

- The original exploratory scripts still live in `~/OHI_Intro/ohi_goc_prep/TR/quantity/old_2025_scripts`. These updated copies are meant to be the clearer for reproducibility.
