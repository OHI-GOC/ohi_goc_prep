# CNGMD Untreated Wastewater: Year-by-Year Data Structure

This document describes how each CNGMD census year stores untreated municipal wastewater discharge data, and how `clean_waters_untreated_ww.Rmd` harmonizes them for the OHI Gulf of California (GoC) clean waters indicator.

**Goal:** identify discharge points where untreated wastewater flows directly into the ocean (Mar, Playa, or Estero/marisma).

**GoC states:** Baja California, Baja California Sur, Jalisco, Nayarit, Sinaloa, Sonora.

---

## Quick reference

| Census year | Reference year in data | Main data file | Municipality ID column | Receptor code column | Flow variable |
|-------------|------------------------|----------------|------------------------|----------------------|---------------|
| 2013 | 2013 | `tr_cengob13_5_vii_ar.csv` | `FOLIO` | `P11_CPO_RP` | none |
| 2015 | 2015 | `secc_vii_tr_cngmd15_m5_p9_1.csv` | `FOLIO` | `P9_1_2` | none |
| 2017 | 2016 | `secc_vii_td_m5_2016_agua_resd.csv` | `agua_resd_folio` | `agua_resd_tip_crp_recp` | none |
| 2019 | 2018 | `secc_vii_td_m5_2018_agua_resd.csv` | `folio` | `res_trec` | `res_v_m3`, `res_v_es` |
| 2021 | 2021 | `aguaresd_cngmd2021.csv` | `folio` | `cuerprec` | `res_caud` |
| 2023 | 2023 | `aguaresd_cngmd2023.csv` | `mnpio` | `res_trec` | `res_caud` |

All files live under:

```
CNGMD_censacional_gobiernos_municipales_demarcaciones_territoriales/agua_sin_tratamiento/
```

---

## Harmonized output schema

After cleaning, every year is returned with these columns (where available):

| Column | Description |
|--------|-------------|
| `census_year` | Census round (2013, 2015, 2017, 2019, 2021, 2023) |
| `id_resd` | Discharge point ID (missing for 2013) |
| `mnpio` | 5-digit municipality folio (entity + municipality code) |
| `cve_geo` | Geographic key (2021/2023 only) |
| `res_nom` | Discharge point name |
| `res_caud` | Flow rate in L/s (2021/2023 only) |
| `res_v_m3` | Measured volume in m³ (2019 only) |
| `res_v_es` | Estimated volume in m³ (2019 only) |
| `res_trec_raw` | Receptor code as reported in that year's raw data |
| `res_trec_std` | Receptor code mapped to the 2023 standard |
| `descrip_std` | Label for the harmonized receptor code |
| `ocean_discharge` | `TRUE` if `res_trec_std` is 6, 7, or 8 |
| `descrip_ent`, `descrip_mun` | State and municipality names |
| `rgn_id` | OHI Gulf of California region ID |

---

## Receptor (discharge location) codes

The biggest cross-year difference is how receptor types are coded.

### 2023 and 2021 standard (target schema)

Used in `res_trec` (2023) and `cuerprec` (2021):

| Code | Description |
|------|-------------|
| 0 | No aplica / Sin respuesta |
| 1 | Río o arroyo |
| 2 | Lago o laguna |
| 3 | Presa |
| 4 | Suelo o barranca |
| 5 | Canal o dren |
| **6** | **Mar** |
| **7** | **Playa** |
| **8** | **Estero, marisma** |
| 9 | Gran colector |
| 10 | Otro |

**Ocean codes:** 6, 7, 8

### 2019, 2017, 2015 standard

Used in `res_trec` (2019), `agua_resd_tip_crp_recp` (2017), and `P9_1_2` (2015):

| Code | Description |
|------|-------------|
| 0 | No aplica / Sin respuesta (2019, 2017 only) |
| 1 | Río o arroyo |
| 2 | Lago o laguna |
| 3 | Presa |
| 4 | Suelo o barranca |
| **5** | **Mar** |
| 6 | Canal o dren |
| 7 | Gran colector |
| 8 | Otro |

**Ocean code:** 5 only (no separate Playa or Estero categories)

**Mapping to 2023 standard:** `5 ? 6` (Mar)

### 2013 standard

Used in `P11_CPO_RP`:

| Code | Description |
|------|-------------|
| 1 | Río o arroyo |
| 2 | Lago o laguna |
| 3 | Presa |
| 4 | Suelo o barranca |
| **5** | **Mar** |
| 6 | Canal o dren |
| 7 | Gran colector |
| 8 | Otro |
| 9 | No especificado |

**Ocean code:** 5 only

**Mapping to 2023 standard:** `5 ? 6` (Mar)

Note: 2013 uses the same numeric values as 2015–2019 for Mar, but the catalog file (`TD_CUERPO_RECEPTOR.csv`) lists categories in a different order.

---

## Year-by-year details

### 2023

- **Folder:** `aguas_residuales_sin_tratamiento_cngmd2023_csv/`
- **Columns:** `id_resd`, `mnpio`, `cve_geo`, `res_nom`, `res_caud`, `res_trec`
- **Municipality catalog:** `catalogos/mnpio_cngmd2023.csv` (`mnpio`, `descrip`, `entidad`)
- **Notes:** Current reference schema. Flow (`res_caud`) is in L/s and may contain special codes (NSS, NA, ND, NP).

### 2021

- **Folder:** `aguas_residuales_sin_tratamiento_cngmd2021_csv/`
- **Columns:** `id_resd`, `folio`, `cve_geo`, `res_nom`, `res_caud`, `cuerprec`
- **Municipality catalog:** `catalogos/mnpio_cngmd2021.csv` (`folio`, `descrip`, `entidad`)
- **Receptor catalog:** `catalogos/cuerprec_cngmd2021.csv`
- **Notes:** Nearly identical to 2023 except `folio` replaces `mnpio` and `cuerprec` replaces `res_trec`. First year with separate Playa (7) and Estero (8) codes.

### 2019

- **Folder:** `aguas_sintrat_cngmd2019_csv/`
- **Columns:** `id_resd`, `folio`, `res_nom`, `res_v_m3`, `res_v_es`, `res_trec`, `res_espe`
- **Municipality catalog:** `catalogos/tc_municipio_2018.csv` (`folio`, `des_municp`, `id_entidad`)
- **Receptor catalog:** `catalogos/ttc_tipo_cuerpo_recp_2018.csv`
- **Notes:**
  - Data reflects 2018 reporting year inside the 2019 census release.
  - Flow is reported as volume (m³), not L/s.
  - `folio` is stored as a zero-padded 5-character string (e.g. `"01001"`).
  - `res_espe` holds free text when receptor code is 8 (Otro).
  - No `cve_geo` column.

### 2017

- **Folder:** `aguas_sintrat_cngmd2017_csv/`
- **Columns:** `agua_resd_id_serial`, `agua_resd_folio`, `agua_resd_tip_crp_recp`, `agua_resd_tip_crp_recp_esp`
- **Municipality catalog:** `catalogos/ttc_municipios_2016.csv` (`mun_folio`, `mun_des_municipio`, `mun_ce`)
- **Receptor catalog:** `catalogos/ttc_tipo_crp_recp_2016.csv`
- **Notes:**
  - Data reflects 2016 reporting year.
  - No discharge point name or flow columns.
  - Column names use the `agua_resd_` prefix throughout.

### 2015

- **Folder:** `Aguas_sintrat_cngmd2015_csv/`
- **Columns:** `ID_GR111224807`, `FOLIO`, `P9_1_1`, `P9_1_2`, `P9_1_2_1`
- **Municipality catalog:** `catalogos/secc_vii_ttc_municipio.csv` (`folio`, `des_municipio`, `id_entidad`)
- **Receptor catalog:** `catalogos/secc_vii_ttc_tipo_cuerpo_receptor.csv`
- **Notes:**
  - `P9_1_1` = discharge point name
  - `P9_1_2` = receptor type code (use this for filtering, not `P9_1_2_1`)
  - `P9_1_2_1` = free-text specification when "other" is selected; often contains descriptive strings like "FOSA" or "LAGUNA DE OXIDACION"
  - `FOLIO` stored as zero-padded character string.

### 2013

- **Folder:** `m5_vii_Aguas_residuales_cngmd2013_csv/`
- **Columns:** `FOLIO`, `P11_CPO_RP`
- **Municipality catalog:** `catalogos/ttc_municipio.csv` (`FOLIO`, `DES_MUNICIPIO`, `ID_ENTIDAD`)
- **Receptor catalog:** `catalogos/TD_CUERPO_RECEPTOR.csv`
- **Notes:**
  - Sparsest dataset: only municipality and receptor type per row.
  - No discharge point ID, name, geographic key, or flow.
  - `FOLIO` stored as integer without leading zeros (e.g. `1001`).

---

## Municipality ID normalization

Municipality identifiers are all mapped to a common 5-digit integer `mnpio`:

| Year | Raw column | Raw format | Example raw ? normalized |
|------|------------|------------|--------------------------|
| 2023 | `mnpio` | integer | `26072` ? `26072` |
| 2021 | `folio` | integer | `26018` ? `26018` |
| 2019 | `folio` | character, zero-padded | `"03002"` ? `3002` |
| 2017 | `agua_resd_folio` | integer | `3002` ? `3002` |
| 2015 | `FOLIO` | character, zero-padded | `"26029"` ? `26029` |
| 2013 | `FOLIO` | integer | `3001` ? `3001` |

The first two digits encode the state (entidad) and the remaining digits encode the municipality within that state.

---

## Special missing-value codes

Across years, numeric and character fields may contain:

| Code | Meaning |
|------|---------|
| NSS | No sabe / no cuenta con elementos para responder |
| NA | No aplica |
| ND | Información no disponible |
| NP | No publicable (confidentiality) |
| NULL | Missing |

The cleaning script converts these to proper `NA` values before analysis.

---

## Files in this folder

| File | Purpose |
|------|---------|
| `clean_waters_untreated_ww.Rmd` | R Markdown script to load, harmonize, and filter all census years |
| `README.md` | This documentation |

---

## Running the script

1. Ensure raw CNGMD data is in the project root under `CNGMD_censacional_gobiernos_municipales_demarcaciones_territoriales/agua_sin_tratamiento/`.
2. Ensure `spatial/municipality_to_rgnid/municipality_to_rgnid.csv` exists (available on the OHI server).
3. Open and knit `clean_waters_untreated_ww.Rmd`, or run chunks interactively in RStudio.

**Key outputs:**

- `goc_aguaresd_all_years` — all GoC discharge points, all years, harmonized schema
- `goc_aguaresd_ocean` — subset discharging to Mar, Playa, or Estero/marisma
