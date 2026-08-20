# Beach / beach-access inventories — study localities

**File:** `beach_access_inventory_by_locality.csv`  
**Date checked:** 2026-07-28  
**Universe:** 41 analyst localities in `GofC_access_localities.csv` / `local_zofemat_by_locality.csv`

## Scoring (`inventory_score`)

Aligned with `scores_access.Rmd` variable 2:

| Score | Meaning (`inventory_status`) |
|------:|------------------------------|
| 0 | No inventory found (`none_found`) |
| 0.25 | Plans announced only (`planned`) |
| 0.5 | Inventory/plan in review or awaiting approval (`in_review`) |
| 0.75 | Inventory/diagnostic completed but **not** easily public (`completed_not_public`) |
| 1 | Completed and easily available as open data/GIS (`easily_available`) |

Inventories are **municipal** products. Localities inherit their municipio’s status (e.g. Sayulita ← Bahía de Banderas; Los Barriles ← La Paz).

## What counts

Counts as an inventory: municipal/IMPLAN/ZOFEMAT products that map **playas and/or public beach accesses** (catálogo, atlas, matriz de accesos, informe de playas y accesos).

Does **not** count: tourism websites, SEMARNAT ZOFEMAT **delimitation** planes alone (federal strip geometry for concessions), or beach **capacity/aforo** dashboards without an access-rights inventory.

## Summary by score (2026-07-28)

| Score | Localities (examples) |
|------:|------------------------|
| 0.75 | La Paz, Los Barriles; Los Cabos, La Ribera; Mazatlán; all Bahía de Banderas study localities (10) |
| 0.5 | Puerto Peñasco |
| 0 | All other study localities (no municipal access inventory found) |
| 1 | **None** — no study locality had a public downloadable GIS/catalog |

## Municipios with documented inventory work

| Municipio | Product | Cited count | Links |
|-----------|---------|-------------|-------|
| La Paz | Reglamento + inventario de playas/accesos; prior Informe still reserved | ~127 playas | [HOY BCS](https://hoybcs.com/aprueba-la-paz-reglamento-para-blindar-acceso-libre-a-playas-podran-reportar-bloqueos/); [Sudcaliforniano](https://oem.com.mx/elsudcaliforniano/local/buscan-blindar-accesos-a-127-playas-de-la-paz-ante-cierres-y-disputas-por-caminos-tradicionales-30521634); [Heraldo](https://heraldodemexico.com.mx/nacional/2026/6/4/gobierno-de-milena-quiroga-consolida-el-acceso-libre-playas-para-las-familias-825702.html) |
| Los Cabos | IMPLAN diagnóstico → catálogo oficial; ZOFEMAT censo | 132 accesos | [CaboVisión](https://cabovision.tv/articulo/81762-avanza-implan-en-catalogo-oficial-de-accesos-a-playas-en-los-cabos-identifican-132-puntos-a-lo-largo-del-litoral); [HOY BCS](https://hoybcs.com/identifican-132-accesos-a-playa-en-los-cabos-trabajan-en-creacion-de-catalogo/); [Independiente](https://www.diarioelindependiente.mx/2026/04/impulsan-en-los-cabos-programa-para-ordenar-y-garantizar-el-acceso-libre-a-playas) |
| Puerto Peñasco | Plan Maestro de accesos | 147 accesos | [El Imparcial](https://www.elimparcial.com/son/sonora/2024/08/16/trabajan-en-plan-para-garantizar-libre-acceso-a-playas-de-penasco/) |
| Mazatlán | Matriz de accesos to SEMARNAT | 65 accesos | [PMX](https://pmxnoticias.com/local/acceso-a-playas-en-mazatlan-envian-a-semarnat-el-registro-de-65-accesos/); [Noroeste](https://www.noroeste.com.mx/mazatlan/buscara-gobierno-de-mazatlan-concesion-de-10-accesos-a-playas-para-garantizar-el-paso-libre-cada-500-metros-IK10386943) |
| Bahía de Banderas | Atlas Municipal de Playas (stage 1) | 100+ playas / ~135 accesos (press varies) | [IMPLAN](http://implan.bahiadebanderas.gob.mx/noticias/se-presenta-la-primera-etapa-del-atlas-de-playas-de-bahia-de-banderas); [On Bahía](https://onbahiamagazine.com/avanza-bahia-de-banderas-en-el-mapeo-y-proteccion-del-acceso-libre-a-sus-playas/) |

## Caveats

- Based on **open web / press / municipal pages** as of 2026-07-28 — not a full PNT sweep of every municipio.
- **None** of the inventories above were found as open GIS downloads; all scored ≤ 0.75.
- Puerto Vallarta’s 2026 ZOFEMAT delimitation update is **not** scored as an access inventory.
- National **Registro Nacional de Accesos a Playas** is legislative / in process and not a usable local dataset yet.
