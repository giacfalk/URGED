# URGED Data Repository

This repository contains the code relevant for the URGED project. Data and outputs are written to folders that are not part of this repository.

## Technical Notes
### Land Cover Classes
We omit the land cover class "lightweight low-rise" class (LCZ == 7) from some analyses, as it contains only a few informal settlement data points in Lagos.
## Köppen-Geiger Climate Zones
We have cities in the Tropical, Dry, Temperate, Continental climate zones. None in the Polar KGC.
## Folder structure

The main code is contained in the `URGED/` folder.

Results in the `results/` folder

Regression tables in `results/regtab/`

Helper functions in `URGED/support`

# Data sets
The following files are saved as intermediate or output files. Usually either in .rds or .RData.

## UGS data
This dataset contains all points for all 180 cities with ugs AND also KGC, LCZ, GHS-associated variables (subregion etc.). For various process reasons, this is produced in `0_build_ugspoints_database.R`:

The old file around which much code has been designed (180 cities): `ugs/after_points_030624_complete.rds`

Colnames:

	"city" "country" "year" "out_b" "x" "y" "ID" "lcz_filter_v3" "ID_HDC_G0"
	"CTR_MN_ISO" "GRGN_L1" "GRGN_L2" "UC_NM_LST" "Cls" "Cls_short"  


The new file with 357 cities
- `ugs/after_points_100425_completedatabase.rds`

Colnames:

	"city" "country" "id" "year" "x" "y" "out_b" "ID" "lcz_filter_v3" "ID_HDC_G0"
	"CTR_MN_ISO" "GRGN_L1" "GRGN_L2" "UC_NM_LST" "EL_AV_ALS" "Cls" "Cls_short" 
Attn! Here, we have variables id and ID which may break some grouping routines! 

## Cities in the analysis
### SGS side
`ugs/after_points_100425_completedatabase.rds` contains 357 entries
### PROVIDE side

### Intersecting the data sets



## List of dependent Variables for 
`t`: `my_cooling_degree_hours_curpol` (filtered by `is.na(t)`)

`t_max`: `my_urbclim_T2M_daily_mean_max_curpol`

`t_min`: `my_urbclim_T2M_daily_mean_min_curpol`

`out_b_mean`: Mean of GVI (mean of 2011 - 2016 of observations)

`out_b_min`: Min of GVI (mean of 2011 - 2016 of observations)

`out_b_max`: Max of GVI (mean of 2011 - 2016 of observations)

## List of independent Variables
`Cls`: Köppen Geiger zone acquired in `1_b_ugs_heat_metrics.R` using package kgc.R

`Cls_short`: Main (first letter) of Köppen Geiger climate zone. In some parts also calles `Cls_main`

`lcz_filter_v3`: Local climate zone (urban form)
 
