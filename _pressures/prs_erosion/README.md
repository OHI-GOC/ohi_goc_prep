# Shoreline class change (GCL_FCS30)

Compares the length of sandy shoreline (the beach class) with the other shoreline classes in each OHI Gulf of California region for 2010, 2015, and 2020.

The question this can answer is whether sandy shore was replaced by built shore. A region shows that pattern when sandy length goes down and artificial length goes up by a similar amount. A swap between sandy and rocky, or a change of only a few percent, is within the classification error.

## Data

**Name:** GCL_FCS30, a global 30 m coastline with a fine classification.

**Download:** https://zenodo.org/records/13943679 (DOI 10.5281/zenodo.13943679)

**Paper:** Zuo, J., Zhang, L., Xiao, J., et al. (2025). GCL_FCS30: a global coastline dataset with 30-m resolution and a fine classification system from 2010 to 2020. *Scientific Data*, 12, 129. https://doi.org/10.1038/s41597-025-04430-0

**Local copy:** `/home/shares/ohi/OHI_GOC/_raw_data/GCL_FCS30/d2025`

| File | Year | When it was saved here |
|---|---|---|
| `GCL2020.*` | 2020 | 3 April 2025 |
| `GCL2010.*` | 2010 | 23 September 2026 |
| `GCL2015.*` | 2015 | 23 September 2026 |

Each year is a shapefile. The `.prj` is Web Mercator (EPSG:3857). The Zenodo description says WGS84; the projection file is the one used here. Lengths in `Shape_Leng` and `Shape_Le_1` are leftover attributes and are not used.

**Classes**

| Code | Class | What it is |
|---|---|---|
| 0 | artificial | Seawalls, ports, piers, reclaimed land |
| 1 | biogenic | Mangroves, marshes, reefs |
| 2 | sandy | Beaches and other sand-dominated shore |
| 3 | muddy | Tidal flats and mudflats |
| 4 | rocky | Cliffs and bedrock |
| 5 | estuary | River–sea transition |

Sandy (class 2) is the beach class. These are shoreline lines, so the measured quantity is length, not beach area or width.

## Script

`beach_conversion_by_region.Rmd` clips each year to the Gulf ecoregion, measures length in a Gulf Albers projection, assigns each segment to the nearest marine OHI region, and writes tables and plots to `int/`.
