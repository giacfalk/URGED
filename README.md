# URGED: URban mitigation and adaptation strategies Gauging through Empirical functions and Data products
Falchetta, G., Lohrey, S., Souverijns, N., Lauwaet, D., Schleussner, C.-F., & Niamir, L. (2026). Street green space is relevant but not sufficient for adapting to growing urban heat in world cities. Environ. Res. Lett. doi: 10.1088/1748-9326/ae5c20. [Access the paper](https://iopscience.iop.org/article/10.1088/1748-9326/ae5c20)

## Overview

URGED is an analytical framework for assessing the impact of urban green spaces (UGS) on heat mitigation and adaptation strategies in cities worldwide. The project combines empirical data on urban vegetation, local climate zones, and heat metrics to quantify the cooling effects of green infrastructure and project future climate scenarios.

### Key Features

- **Multi-city analysis**: Coverage of 357 cities globally with UGS data
- **Climate integration**: Analysis across Köppen-Geiger climate zones and Local Climate Zones (LCZ)
- **Heat metrics**: Multiple thermal indicators including temperature, cooling degree hours, and WBGT
- **Future projections**: Climate change impact assessments and policy simulations
- **Population exposure**: Heat exposure analysis by city and urban form

## Repository Structure
### Entry point
- **`00_sourcer.R`**  
  Main orchestrator: sets working directories, loads packages, sources helper functions, then runs the pipeline (database build → regressions → projections → outputs → policy simulation → paper figures).
  
### Helper functions
Located in `support/` and sourced explicitly:
- `support/fcts_labelers_colors.R` (labels, LCZ/Köppen color schemes, dictionaries, sample cities) 
- `support/fcts_helpers_debug.R` (debug helpers / quick counts & plots) 
- `support/fct_scenarios.R` (scenario helper functions)

### Pipeline scripts
**0) Build / harmonize core databases**
- `0_build_cities_database.R`
- `0_build_ugspoints_citynames.R`
- `0_build_ugspoints_database.R`
- `0_citynames_harmonization.R`
- (optional / currently commented in `00_sourcer.R`) `0_output_template.R`
- `0_show_ugs_policy_meaning.R` (visual interpretation of GVI values)

**1) City-level regressions for heat metrics (WBGT and T2M)**
These “run_*” scripts are sourced directly by `00_sourcer.R` and typically call the underlying estimation scripts:
- `run_WBGT_max_all_cities.R`
- `run_WBGT_mean_all_cities.R`
- `run_WBGT_min_all_cities.R`
- `run_T2M_max_all_cities.R`
- `run_T2M_mean_all_cities.R`
- `run_T2M_min_all_cities.R`

Underlying estimation scripts present in the repo (useful if you want to run/inspect one model family directly):
- `estimate_gvi_coefs_WBGT_{max,mean,min}_panel_multicities.R`
- `estimate_gvi_coefs_T2M_{max,mean,min}_panel_multicities.R`

**2) Robustness + cross-metric summary**
- `1_daily_ugs_elasticities_r1_robustness.R` (robustness: pooled specs, clustering/SE variants, etc.)
- `1_summary_table_compare_across_metrics.R`

**3) Climate change deltas and processing of supporting climate inputs**
- `0_calculate_climate_change_markups.R` (future deltas from CMIP6-style inputs)
- `4_process_humidity_data.R` 
- `4_process_lst_data.R`
- `0_wbt_wbgt.R` (WBGT derivation / future WBGT estimation utilities)

**4) Projections of future GVI**
- `2_project_future_ugs_pointwise.R`

**5) Write outputs for downstream policy simulations and analysis**
- `3_write_output_chilled.R`

**6) Policy simulation**
- `4_policy_simulation_climate_change_ugs_heat_metrics.r`

**7) Paper figure scripts**
These are sourced at the bottom of `00_sourcer.R` (edit/comment depending on what you want to reproduce):
- `figures_scripts/fig_3.R`
- `figures_scripts/table_scenarios.R`
- `figures_scripts/21_project_future_ugs_pointwise_plotting.R`
- `figures_scripts/plot_scenarios.R`
- `figures_scripts/fig_4_new.r`
- `figures_scripts/map_counterbalancing_wbgt_mean.R`
- `figures_scripts/map_counterbalancing_tas_min.R`
- `figures_scripts/map_counterbalancing_wbgt_max.R`

---

## Repository tree (high level)
```text
URGED/
├─ 00_sourcer.R
├─ support/                  # helper functions sourced by 00_sourcer.R
├─ figures_scripts/          # paper figure reproduction scripts
├─ old/                      # legacy code
├─ other/                    # misc / auxiliary content
├─ 0_*.R                     # database builds & harmonization
├─ run_*.R                   # city-by-city runs for WBGT and T2M families
├─ estimate_gvi_coefs_*.R    # estimation backends for WBGT/T2M
├─ 1_*.R                     # robustness & summary tables (and additional analyses)
├─ 2_*.R                     # projections
├─ 3_*.R                     # output writers
├─ 4_*.R                     # policy sim + climate input processing
└─ 5_*.R                     # population/exposure analysis (not sourced by default)                          # Legacy code (archived)
```

## Data Requirements

### Input Data

All required input data files are available from the [Zenodo repository](https://doi.org/10.5281/zenodo.18848130).

**Key datasets:**
- Urban Green Space (UGS) point data with Green View Index (GVI)
- Local Climate Zone (LCZ) classifications
- Global Human Settlement (GHS) data
- Köppen-Geiger climate classifications
- Urban climate data (URBCLIM)
- Land surface temperature (LST)
- Humidity data

### Output Structure

Results are written to folders outside this repository:
- `results/` - Main analysis outputs
- `results/regtab/` - Regression tables
- Intermediate files saved as `.rds` or `.RData`

## Key Data Files

### UGS Database

**Legacy dataset (180 cities):**
```
ugs/after_points_030624_complete.rds
```

**Extended dataset (357 cities):**
```
ugs/after_points_100425_completedatabase.rds
```

**Key variables:**
- `city`, `country`, `year` - Geographic and temporal identifiers
- `out_b`, `out_b_mean`, `out_b_min`, `out_b_max` - Green View Index metrics
- `x`, `y` - Spatial coordinates
- `lcz_filter_v3` - Local Climate Zone classification
- `ID_HDC_G0` - GHS identifier
- `Cls`, `Cls_short` - Köppen-Geiger climate zones
- `UC_NM_LST` - Urban center name

### Climate Variables

**Dependent Variables:**
- `my_cooling_degree_hours_curpol` - Cooling degree hours (current policy)
- `my_urbclim_T2M_daily_mean_max_curpol` - Daily maximum temperature
- `my_urbclim_T2M_daily_mean_min_curpol` - Daily minimum temperature

**Independent Variables:**
- `Cls` / `Cls_short` - Köppen-Geiger climate classification
- `lcz_filter_v3` - Local climate zone (urban form)

## Technical Notes
### Land Cover Classes
We omit the land cover class "lightweight low-rise" class (LCZ == 7) from some analyses, as it contains only a few informal settlement data points in Lagos.

The same applies to the Heavy Industry (LCZ == 10) class.

## Köppen-Geiger Climate Zones
We have cities in the Tropical, Dry, Temperate, Continental climate zones. None in the Polar KGC.
## Folder structure
### Climate Classifications

**Köppen-Geiger Zones:**
The analysis covers cities in Tropical (A), Dry (B), Temperate (C), and Continental (D) climate zones. No cities in Polar (E) zones are included.

**Local Climate Zones:**
Analysis omits LCZ 7 ("lightweight low-rise") which contains limited data from informal settlements in Lagos.

### Climate Zones Abbreviations

- **Cls**: Full Köppen-Geiger classification
- **Cls_short** / **Cls_main**: First letter of Köppen-Geiger zone

## Getting Started

### Prerequisites

- R (version 4.0 or higher recommended)
- Required R packages (loaded via `00_sourcer.R`)
- Input data from Zenodo repository

### Installation

1. Clone this repository:
```bash
git clone https://github.com/giacfalk/URGED.git
cd URGED
```

2. Download input data from [Zenodo](https://doi.org/10.5281/zenodo.18848130)

3. Set up folder structure (create `results/` directory outside repository)

4. Run the initialization script:
```r
source("00_sourcer.R")
```

## Citation

If you use this code or data in your research, please cite:

```bibtex
@article{Falchetta2026_SGS_heat,
  title   = {Street green space is relevant but not sufficient for adapting to growing urban heat in world cities},
  author  = {Falchetta, Giacomo and Lohrey, Steffen and Souverijns, Niels and Lauwaet, Dirk and Schleussner, Carl-Friedrich and Niamir, Leila},
  journal = {Environmental Research Letters},
  year    = {2026},
  note    = {Under review}
}
```

## Contact

Giacomo Falcetta: falchetta@iiasa.ac.at
Steffen Lohrey: lohrey@iiasa.ac.at
