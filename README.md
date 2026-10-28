# Beaver ponds and wildfire in Interior Alaska (pilot study)

Code and data for:

> Samuel, W. T., K. D. Tape, E. R. Schoen, and A. V. Coulson. *Do beaver ponds reduce wildfire effects on boreal riparian areas?* Preprint (not peer reviewed). [link to be added]

We compared MTBS burn severity, Sentinel-2 NDVI, and Sentinel-2 NDWI between beaver ponds and paired nearby riparian reference sites in 10 wildfires that burned in Interior Alaska from 2016 to 2021. We also compared riparian sites with the burn severity of each fire as a whole.

## Reproducing the results

1. Open `Beaver_Riparian_Resilience_Wildfire_Boreal.Rproj` in RStudio.
2. Run **`REVISED Beaver pond wildfire time series analysis.R`** from top to bottom.
   - It uses these inputs:
     - `Data/Burn Severity Summaries/`
     - `Data/beavers*.csv` and `Data/controls*.csv` (only for pairing sites and measuring distances)
     - `Newdata/*_long.csv`
   - It writes every manuscript figure to `Figures/REVISED_*.jpeg`.
   - It writes Table S1 (image dates by fire) to `Figures/REVISED_Table_image_dates_by_fire.csv`.
   - Its last section ("Summary of numbers for the manuscript") prints every statistic reported in the paper.
3. The `Newdata/*_long.csv` files were made by the "Data prep" section of the original script, `Beaver pond wildfire time series analysis.R`. You only need to rerun that section to rebuild them from the Google Earth Engine exports.

Packages: tidyverse, lubridate, patchwork, cowplot, viridis, lme4, lmerTest, ordinal. Tested with R 4.3.3.

Changes are marked in the revised script with `#CHANGED:` and `#NEW:` comments.

## Folder contents

| Path | Contents |
|---|---|
| `REVISED Beaver pond wildfire time series analysis.R` | **Analysis used in the preprint.** |
| `Data/beavers0.csv`–`beavers10.csv`, `Data/controls0.csv`–`controls10.csv` | Google Earth Engine exports, one file per fire. See the file key below. |
| `Data/Burn Severity Summaries/` | ArcGIS Pro exports of MTBS burn severity (percent of area in each class) for each beaver pond buffer, reference site buffer, and fire perimeter. |
| `Data/beaver_fire_annual_data.csv` (+ `_metadata.csv`) | Annual NDVI/NDWI composites from 5 years before to 6 years after each fire. **Not used in the preprint.** |
| `Newdata/` | Long-format NDVI/NDWI (one row per site and image date), made by the original script. |
| `Spatial/beaver_and_reference_sites.gpkg` | GeoPackage (WGS84, EPSG:4326) with two layers: `site_buffers` (the polygons used for all analyses) and `site_centroids`. |
| `Spatial/beaver_and_reference_site_centroids.csv` | The same sites as a table with `longitude` and `latitude` columns. |
| `Figures/` | `REVISED_*` files are the preprint figures. The other files are earlier versions. |

### Legacy files (not used in the preprint)

- `Beaver pond wildfire time series analysis.R`: the original analysis, submitted to *Environmental Research Letters* in 2025. Its statistics were revised (see below). Only its "Data prep" section is still needed.
- `OLDER Beaver Pond Fire Analysis.R` and `OLD Data from Munson fire/`: an early 2022 pilot on the Munson Creek Fire only.
- `mkaing_graphs.R`, `one-off_lm_testing.R`, `_.csv`: scratch files.

## Data notes

- **File key for `beavers#.csv` / `controls#.csv`:**

  | # | Fire |
  |---|---|
  | 0 | Iniakuk Lake |
  | 1 | Hogatza River |
  | 2 | Hog |
  | 3 | Chalkyitsik Complex (excluded; only one pond) |
  | 4 | Sunshine Mountain |
  | 5 | Munson Creek |
  | 6 | Page (Paige) Mountain |
  | 7 | Hurst Creek |
  | 8 | Old Grouch Top |
  | 9 | Victoria Mountain |
  | 10 | Little Mud River |

- **Inside each GEE file:**
  - Each row is one site buffer, with its polygon stored in the `.geo` column as GeoJSON.
  - Columns ending in `_ndvi` or `_ndwi` hold the mean index for one Sentinel-2 date (`YYYY_MM_DD`).
  - The pond number is in `Beaver_P_5` in the beaver files and `Control_Po` in the control files.
  - The columns in the control files are shifted: `Beaver_P_2` holds the fire year, not the fire name.
- **Reference sites** are called "Control" in the raw files. Each beaver pond and its reference site share a pond number within a fire.
  - In `Spatial/`, the shared ID is `pair_id`.
  - Paired sites are a median of 160 m apart.
- **Burn severity classes:** the ArcGIS exports use "Unburned", "Mild", "Moderate", and "Severe". These are the MTBS thematic classes "Unburned to Low", "Low", "Moderate", and "High".
- **Sites with no MTBS data** (masked for poor imagery) were excluded: 10 beaver ponds and 16 reference sites.
- **Pond confidence:** `pond_confidence` in `Spatial/` comes from the mapping notes. Y = confident, M = possible pond, blank = not recorded. All ponds were included in the analyses. Excluding the 26 "M" ponds does not change the conclusions.
- **Site counts:** the GEE files contain 231 beaver and 223 reference buffers, and the burn severity tables contain 235 of each. Analyses match sites by fire and pond number.

## Changes from the 2025 analysis

- **Burn severity model (CLMM):** each site now has weights that sum to 1. Before, weights equaled the percent of area, so each site counted about 100 times. The model also now includes a random effect for each beaver–reference pair.
- **Riparian vs. entire fire:** new paired comparison across the 10 fires.
- **Pre-fire window:** now runs from June 1 to each fire's MTBS ignition date, instead of a fixed day-of-year 175.
- **NDVI/NDWI changes:** now analyzed with paired mixed models, with and without the pre-fire value as a covariate.
- **Figure S1:** fixed. Each panel previously showed all fires except the one named.
- **Terminology:** "control" sites are called "reference" sites in the preprint and figures.

## Contact

William T. Samuel, wtsamuel@alaska.edu
