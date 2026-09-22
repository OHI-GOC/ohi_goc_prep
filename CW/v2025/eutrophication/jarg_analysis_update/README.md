# Eutrophication component: using total nitrogen

This is a re-implementation of Romero-Gil et al. (2026) nitrogen-discharge methods for OHI Gulf of California Clean Waters **Eutrophication**, using **watershed → pourpoint → OHI region** assignment instead of municipal coastline standardization.

**Paper:** Romero-Gil, J. A., Halpern, B. S., Tuholske, C., Arriaga, J. A., & Brault, J.-M. (2026). Nitrogen discharge in the Gulf of California from wastewater, agriculture, livestock, and aquaculture. *Water Practice & Technology*, 21(3), 1205. https://doi.org/10.2166/wpt.2026.239

**Romero-Gil et al. 2026 repository:** [AntonioRog/OceanHealth-Sanitation](https://github.com/AntonioRog/OceanHealth-Sanitation) (local copy: `nitrogen_discharge_goc_from_wastewater_agriculture_livestock_aquaculture/OceanHealth-Sanitation-main/`)

## Why redo this

1. JARG reports TN at the **municipality** level; we need loads routed by **watershed pourpoints** so they match how water actually reaches the Gulf (same spatial logic as Chemicals pesticides / pharmaceuticals).
2. JARG’s study area omitted **Nayarit** and **Jalisco** municipalities that fall in GoC OHI regions (specifically Region 9).
3. Dividing by **coastline length** inflated Mexicali (Region 1) relative to Sinaloa; when we spoke with him, JARG expected Sinaloa-area loads to dominate when hydrology is considered.
4. Region 9 had no TN in the paper; the prior CW script gapfilled with Region 8, which was relatively inaccurate.
5. We already hold most of the spatial layers (CONABIO USV, SIAP, CONAGUA SINA, FAO GLW, watersheds/pourpoints) used elsewhere in Clean Waters, so a reproducible rerun (and future Index updates) is feasible.

## Preferred coefficients (sensitivity columns used for CW)

From JARG Supplementary Table 6:

| Source | Coefficient used for scoring | Notes |
|--------|------------------------------|-------|
| Wastewater | **Mean** protein-consumption scenario | Mean − σ / Mean / Mean + σ in Table 6 |
| Agriculture | **10%** leaching (`Lc`) | Table 6 also has 5% and 20% (paper main text uses 20%) |
| Livestock | **40%** export | Table 6 also has 70% and 100% (paper main text uses 100%) |
| Shrimp aquaculture | **100%** reaches sea | Table 6 also has 50% and 85% |

## Scripts

| Order | File | Role |
|-------|------|------|
| 1 | `script1_wastewater.qmd` | Human sanitation N (urban + rural) |
| 2 | `script2_agriculture.qmd` | Fertilizer N × area × leaching |
| 3 | `script3_livestock.qmd` | Heads × emission factors × export |
| 4 | `script4_shrimp_aquaculture.qmd` | Production × 47 kg N t⁻¹ × export |
| 5 | `script5_total_nitrogen.qmd` | Sum sources → watershed → pourpoint → region |
| 6 | `script6_eut_current_status.qmd` | Pressure / current status scores |

## Date we use within Chemicals as well:

- GoC OHI regions: `spatial/ohi_regions/goc_ohi_rgns.shp`
- Watersheds: `/home/shares/ohi/stressors_2021/_dataprep/nutrients/watersheds_pourpoints/watersheds_all_4326.shp`
- Pourpoints: `.../pourpoints_mol.shp`
- Inland buffers (50 km mainland / 10 km Pacific-side peninsula)
