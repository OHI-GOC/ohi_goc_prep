# ZOFEMAT budget & enforcement — reference index

Collected **13–14 July 2026** for research on budgets and personnel managing/enforcing Mexico's Zona Federal Marítimo Terrestre (ZOFEMAT).

## Main analysis

**Notebook / report:** `pef_semarnat_zofemat_trends.Rmd` → [`pef_semarnat_zofemat_trends.html`](pef_semarnat_zofemat_trends.html)

**Framing (updated 15 Jul 2026):** beach-access **maintenance capacity** indicator —

1. **PROFEPA plazas** (national; raw + per capita)
2. **PROFEPA inspection PEF** (raw, INPC-adjusted, and real per capita)
3. **Share of study municipalities with an active ZOFEMAT office**, by OHI marine region
4. Narrative of agencies, complaint pathway, and data weaknesses

Supporting CSVs in `output/indicator_*.csv`. Prior PEF national/state extracts remain in `output/pef_*.csv`.

Clean CSVs in `output/`:

- `pef_national_series_2018_2026.csv`
- `pef_national_shares_2018_2026.csv`
- `pef_profepa_focus_states_2018_2026.csv`
- `pef_semarnat_offices_focus_states_2018_2026.csv`
- `pef_plazas_dgz_profepa_2018_2026.csv`

Narrative summary: [`zofemat_budget_enforcement_summary.html`](zofemat_budget_enforcement_summary.html)

## Downloaded documents (`sources/`)

| File | Source | Notes |
|------|--------|-------|
| `profepa_informe_anual_2024.pdf` | [gob.mx PROFEPA](https://www.gob.mx/cms/uploads/attachment/file/1038916/Informe_Anual_Profepa_2025_VF_.pdf) | 2024 enforcement stats, CVAP, ZOFEMAT actions |
| `nossa_pef_2025_ambiente.pdf` | [NOSSA México](https://nossamexico.com/wp-content/uploads/2024/12/NOSSA_CLQI2025_101224_FIN_lt.pdf) | PPEF 2025 analysis |
| `oceana_presupuesto_ambiental_2018_2024.pdf` | [Oceana México](https://mx.oceana.org/wp-content/uploads/sites/17/2023/11/Analisis-del-presupuesto-para-el-sector-ambiental-2024.pdf) | PROFEPA budget trend (real pesos) |
| `semarnat_reglamento_zofemat.pdf` | SEMARNAT biblioteca | Reglamento ZOFEMAT |

## PEF analiticos (raw)

```
references/sources/pef_analiticos/pp/pef{YYYY}_ac01_ra_pp_ur_og.xlsx   # 2018–2026 (used)
references/sources/pef_analiticos/plazas/analitico_plazas_apf_{YYYY}.xlsx
```

URL pattern:  
`https://www.pef.hacienda.gob.mx/work/models/PEF/Analiticos_Historico/{YEAR}/Autorizado/`

## Institutional split

| Institution | ZOFEMAT role |
|-------------|--------------|
| **SEMARNAT — DGZFMTAC** | Delimitation; grants concessions/permits |
| **PROFEPA** | Inspection, surveillance, sanctions; free-access enforcement |
| **Municipal ZOFEMAT** (e.g. La Paz) | Local fees, cleaning, notifications — not in PEF |

## Data gaps

- No **ZOFEMAT-dedicated** slice inside PROFEPA’s inspection program.
- Plazas = authorized posts, not filled headcount.
- State EF tags are geographic budget tags, not beach-only budgets.
