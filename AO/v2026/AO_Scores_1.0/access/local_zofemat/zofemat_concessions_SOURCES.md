# ZOFEMAT concession resolutivos — sources and counts

## Download

Published resolutivos lists were downloaded from:

- http://dsiappsdev.semarnat.gob.mx/datos/zonafederal/

linked from SEMARNAT transparencia materials for zona federal (ZOFEMAT).

**Download date:** 2026-07-15

## Local file paths

Under `local_zofemat/sources/concessions/`:

| File | Role |
|------|------|
| `Resolutivos_zona_federal.csv` | **Primary analysis file** used for municipio and year counts |
| `Resolutivos_ZF_2015.xlsx` … `Resolutivos_ZF_2019.xlsx` | Annual workbooks (schemas vary by year; 2018 has Estado/Municipio) |
| `mensuales/enero_2020.xlsx`, `mensuales/agosto_2020.xlsx` | Monthly resolutivo tables with Estado/Municipio |
| ArcGIS JSON extracts (`arcgis_layer_*`) | SITGIGS FeatureServer snapshot (see caveat below) |

## What the count means

Counts are **published resolutivos** (administrative acts such as grants and related DGZF resolutions appearing in the SEMARNAT zona federal resolutivos extract).

They are **not** a stock of currently active concessions. A single expediente may appear once or more across years; `n_unique_expedientes` uses the `EXPEDIENTE` field when present.

**Primary CSV coverage:** calendar years **2013–2016** (6102 data rows after header). Secondary study counts for **2018** (`Resolutivos_ZF_2018.xlsx`) and **partial 2020** (`mensuales/`: enero_2020, agosto_2020) are in separate CSVs and merged as extra columns on the wide municipio file (see section below).

## ArcGIS caveat (active stock)

ArcGIS SITGIGS FeatureServer **layer 2 (Trámites Vigentes)** would be preferable for an active-concession stock, but in the downloaded extract `ENTIDAD` is effectively limited to **Guerrero** and **Quintana Roo** — **unusable for Gulf of California / study Pacific states**.

## Matching method and caveats

- Strings normalized: uppercase, strip accents/diacritics, collapse whitespace (Ñ→N).
- Matches require normalized ESTADO in the focus state and MUNICIPIO equal to a known alias list for each study `municipality_coded` (no edit-distance fuzzy matching).
- **San Felipe vs Mexicali:** INEGI coding in the study table still uses Mexicali (`CVE_MUN=2`); San Felipe locality is under that row. Alias `SAN FELIPE` is applied when matching Mexicali. In `2026-07-15` CSV, Baja California municipios are mainly Ensenada / Mexicali / Playas de Rosarito / Tijuana — **San Felipe as MUNICIPIO is typically absent**.
- Spelling variants in source (e.g. `Mazatlan`/`Mazatlán`, `Escuinapa`/`Escuinapa de hidalgo`, `Puerto peÑasco`/`Puerto peñasco`, `La paz`/`Los cabos`) are captured via normalization and aliases; originals listed in `matched_municipio_names_in_source`.
- **Pitiquito** (Sonora) had no rows in the primary CSV period.

## State rollup (focus states, primary CSV)

| State | n_resolutivos (all munis) | n_resolutivos (study munis) | n_unique_expedientes (all) | years |
|-------|---------------------------|-----------------------------|------------------------------|-------|
| Baja California | 274 | 25 | 243 | 2013–2016 |
| Baja California Sur | 422 | 394 | 379 | 2013–2016 |
| Sonora | 497 | 458 | 474 | 2013–2016 |
| Sinaloa | 506 | 429 | 478 | 2013–2016 |
| Nayarit | 611 | 579 | 518 | 2013–2016 |
| Jalisco | 371 | 187 | 321 | 2013–2016 |

## Output products

- `local_zofemat/zofemat_concession_counts_by_municipio.csv`
- `local_zofemat/zofemat_concession_counts_by_municipio_year.csv`
- this file: `local_zofemat/zofemat_concessions_SOURCES.md`

## Secondary sources: 2018 annual and 2020 mensuales

Additional municipio counts were produced from annual/monthly workbooks that include **Estado** and **Municipio** columns (usable for the same study-municipio matching as the primary CSV).

### Files and year coverage

| Product | Source files | Coverage |
|---------|--------------|----------|
| `zofemat_concession_counts_by_municipio_2018.csv` | `Resolutivos_ZF_2018.xlsx` (sheet(s) with Estado/Municipio header) | Calendar year **2018** resolutivos listed in that workbook |
| `zofemat_concession_counts_by_municipio_2020_mensuales.csv` | `mensuales/*.xlsx` — **enero_2020, agosto_2020** | **Partial 2020 only** (months present under `mensuales/`; not a full-year total). Counts are the **sum** of matching rows across those monthly files. |

Other annual workbooks (`Resolutivos_ZF_2015.xlsx`, `2016`, `2017`, `2019`) were not used for these secondary CSVs; schemas vary by year and were not required for this update.

### Matching

Same normalization and alias rules as the primary 2013–2016 counts (uppercase, strip accents/Ñ→N, collapse whitespace; Escuinapa de Hidalgo → Escuinapa; **San Felipe** under Baja California counted with study **Mexicali**, with Mexicali labels counted separately within that row via `matched_municipio_names_in_source`).

### Wide merge

`zofemat_concession_counts_by_municipio.csv` now carries:

- `n_resolutivos_2013_2016` (renamed from `n_resolutivos`; from `Resolutivos_zona_federal.csv`)
- `n_resolutivos_2018`
- `n_resolutivos_2020_mensuales_partial`

plus prior columns (`n_unique_expedientes`, year_min/max, matched names, notes, etc.). The year-long file `zofemat_concession_counts_by_municipio_year.csv` is unchanged.

