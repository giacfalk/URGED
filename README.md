# URGED: URban mitigation and adaptation strategies Gauging through Empirical functions and Data products

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17803495.svg)](https://doi.org/10.5281/zenodo.17803495)

## Overview

URGED is an analytical framework for assessing the impact of urban green spaces (UGS) on heat mitigation and adaptation strategies in cities worldwide. The project combines empirical data on urban vegetation, local climate zones, and heat metrics to quantify the cooling effects of green infrastructure and project future climate scenarios.

### Key Features

- **Multi-city analysis**: Coverage of 357 cities globally with UGS data
- **Climate integration**: Analysis across Köppen-Geiger climate zones and Local Climate Zones (LCZ)
- **Heat metrics**: Multiple thermal indicators including temperature, cooling degree hours, and WBGT
- **Future projections**: Climate change impact assessments and policy simulations
- **Population exposure**: Heat exposure analysis by city and urban form

## Repository Structure

```
URGED/
├── 00_sourcer.R                    # Main initialization and helper functions loader
├── 0_*.R                           # Data preparation and database building scripts
├── 1_*.R                           # Analysis scripts (elasticities, summaries)
├── 2_*.R                           # Future projections and visualizations
├── 3_*.R                           # Output generation scripts
├── 4_*.R                           # Policy simulations and data processing
├── 5_*.R                           # Population and heat exposure analysis
├── support/                        # Helper functions and utilities
├── figures_scripts/                # Scripts for generating publication figures
└── old/                            # Legacy code (archived)
```

## Data Requirements

### Input Data

All required input data files are available from the [Zenodo repository](https://doi.org/10.5281/zenodo.17803495).

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

## Workflow

### 1. Data Preparation (Scripts 0_*)

**Database Construction:**
- `0_build_cities_database.R` - Constructs city-level database
- `0_build_ugspoints_database.R` - Creates UGS points database with all attributes
- `0_build_ugspoints_citynames.R` - Harmonizes city names across datasets
- `0_citynames_harmonization.R` - Additional city name standardization

**Supporting Data Processing:**
- `0_calculate_climate_change_markups.R` - Computes future climate adjustments
- `0_wbt_wbgt.R` - Calculates Wet Bulb Globe Temperature metrics
- `0_tstats_city_lcz_urbclim.R` - Temperature statistics by city and LCZ

**Outputs and Templates:**
- `0_output_template.R` - Defines output structure
- `0_show_ugs_policy_meaning.R` - UGS policy interpretation
- `0_table_sgslevel_bylcz.R` - Summary tables by LCZ

### 2. Statistical Analysis (Scripts 1_*)

**Elasticity Estimation:**
- `1_monthly_ugs_elasticities.R` - Monthly UGS-temperature relationships
- `1_monthly_ugs_elasticities_WBGT.R` - WBGT-based elasticities
- `1_monthly_ugs_elasticities_italy.R` - Italy-specific analysis
- `1_summary_table_compare_across_metrics.R` - Cross-metric comparisons

### 3. Future Projections (Scripts 2_*)

- `2_project_future_ugs_pointwise.R` - Point-level future UGS projections
- `2ALPS_project_future_ugs_pointwise.R` - ALPS region projections
- `2_plot_ugs_frontrunner_cities.R` - Visualization of leading cities

### 4. Policy Analysis & Data Processing (Scripts 4_*)

- `4_policy_simulation_climate_change_ugs_heat_metrics.r` - Climate policy scenarios
- `4_process_humidity_data.R` - Humidity data processing
- `4_process_lst_data.R` - Land surface temperature processing

### 5. Population & Exposure Analysis (Scripts 5_*)

- `5_pop_bycity_bylcz.R` - Population distribution by city and LCZ
- `5_pop_heat_exposure.R` - Heat exposure population assessments

### 6. Output Generation (Scripts 3_*)

- `3_write_output_chilled.R` - Standard output generation
- `3_write_output_chilled_ALPS.R` - ALPS-specific outputs

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

2. Download input data from [Zenodo](https://doi.org/10.5281/zenodo.17803495)

3. Set up folder structure (create `results/` directory outside repository)

4. Run the initialization script:
```r
source("00_sourcer.R")
```

### Running the Analysis

Execute scripts in numerical order:

```r
# 1. Build databases
source("0_build_cities_database.R")
source("0_build_ugspoints_database.R")

# 2. Calculate metrics
source("0_calculate_climate_change_markups.R")
source("0_wbt_wbgt.R")

# 3. Run analyses
source("1_monthly_ugs_elasticities.R")
source("2_project_future_ugs_pointwise.R")

# 4. Generate outputs
source("3_write_output_chilled.R")
```

## Documentation

Additional documentation files:
- `Code_Descriptions.md` - Detailed script descriptions
- `ColorCoding.md` - Color schemes for visualizations
- `files_to_be_updated_new_gvi_data.txt` - Data update tracking

## Citation

If you use this code or data in your research, please cite:

```bibtex
@dataset{urged_2025,
  author       = {[Falchetta, Giacomo and Lohrey, Steffen]},
  title        = {Street green space is relevant but not sufficient for adapting to growing urban heat in world cities},
  year         = {2025},
  publisher    = {Preprint}
}
```

## Contact

Giacomo Falcetta: falchetta@iiasa.ac.at
Steffen Lohrey: lohrey@iiasa.ac.at


**Note:** This is an active research project. Code and documentation are continuously updated.
