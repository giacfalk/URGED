# URGED  Code Descriptions
 
### **`0_build_cities_database.R`**
- Obtain the city list for the cities contained in the PROVIDE data
- Get Köppen-geiger classes using kgc package
- Get local climate zone using a raster file from https://journals.ametsoc.org/view/journals/bams/93/12/bams-d-11-00019.1.xml 
- Merge provide city names and GHS data using fuzzyjoin and stringdist_join()
- The result is visualized using mapview for an interactive map of cities colored by climate zone (kg_cl).
- The final dataset is saved as a GeoPackage file (`results/cities_database_climatezones.gpkg`), which can be used for further spatial analysis.

###**`1_b_ugs_heat_metrics.R`**
This code will produce the output file used for the statistical analysis.

- First load climate data for different time periods (2020, 2030, 2050, 2070, 2100): PROVIDE cooling degree hours, and urbclim max and min of daily mean T2
- We use GVI as urban green indicator (dubbed as variable out_ndvi_m)
- Analysis performed on 10 largest cities per region
- Summarize and transform the data into spatial objects (sf), preparing it for extraction of climate and other spatial information.
- Extract climate variables (e.g., cooling degree hours, temperature extremes) for the selected cities using exact_extract.
- Incorporate other spatial covariates such as building density/height, population density, poverty index, and water bodies.
- Output and write to `data_provide_cdh_gvi_143cities_withcovariates.rds`
- **Output variables in output file:**
	- `t`: `my_cooling_degree_hours_curpol` (filtered by `is.na(t)`)
	- `t_max`: `my_urbclim_T2M_daily_mean_max_curpol`
	- `t_min`: `my_urbclim_T2M_daily_mean_min_curpol`


### **`1bb_ugs_statsmodels`**
Fixed effects models to explore the relationship between GVI and climate vars (CDH) across cities, controlling for covariates (building height, population density, water, elevation, etc.).
Data from 1b code is loaded (`data_provide_cdh_gvi_143cities_withcovariates.rds`) as `df`.

- City-Level Analysis
- map plots

### **`1bb_ugs_statsvis`**
Visualize some of the statistical relatinships

### **`2_project_future_ugs_pointwise`**
[Nov 28, 2025]

Create projections for future Urban Green Space and calculate related future cooling capability based on determined elasticities.

Goal: make scenarios for each cluster of Cls_short and lcz in each city. Project into future taking into account the individual "adaptation limit".
Finally, summarize to city level again. Both data.frames can be writting to output.

- Load the complete UGS data from `ugs/after_points_xxxx_completedatabase.rds`
- Filter city names with NA, and LCZ > 10 (non-urban classes)
- Filter city names that are also in adjacent countries:
- ugs <- ugs %>%
  filter(
    !(city == "Singapore" & country == "MY"),
    !(city == "Asuncion" & country == "AR"),
    !(city == "Basel" & country %in% c("DE", "FR")),
    !(city == "Buffalo" & country == "CA"),
    !(city == "Ciudad Juárez" & country == "US"),
    !(city == "Comilla" & country == "IN"),
    !(city == "Detroit" & country == "CA"),
    !(city == "Geneva" & country == "FR"),
    !(city == "Ghent" & country == "NL"),
    !(city == "Guangzhou" & country == "HK"),
    !(city == "Kinshasa" & country == "CG"),
    !(city == "Lille" & country == "BE"),
    !(city == "Lomé" & country == "GH"),
    !(city == "N'Djamena" & country == "CM"),
    !(city == "Strasbourg" & country == "DE"),
    !(city == "Tijuana" & country == "US")
  )
- Make data.frame `dfspat`

- First, construct a data.frame which contains the GVI values on spatial average, as well as spatio-temporal average. Classified by LCZ and Cls_short.

The data.frames are called `dfspat` and `dfspattemp`


Column names:

- `out_b_mean_st`: The spatial (city, lcz, Clsmain) and temporal (2016-2023) mean 
- `out_b_mean_s`: The spatial (city, lcz, Clsmain) mean, but years are retained.
- `npid_xx`: The number of sampling points for each of the averaging

Produces various plots, including future scenarios of urban green, and some descriptive statistics of the data points in the ugs data frame `ugs/after_points_030624_completedatabase.rds`


There is a further data.frame `dfhighestobs`. This data.frame contains the 10th and 90th percentile values for GVI for each LCZ and KGC.

Key data.frames `dfspat` (spatially averaged GVI values [over all points], seperated by city and LCZ) and `dfspattemp` (spatially and temporally [2016-2023] averaged GVI values, seperated by city and LCZ).

We create an ID-Variable called `pid` which is made using paste0(x, y, sep = "-"), where x and y are longitude and latitude, respectively.
Note that the number of pids remains constant from 2016-2023 in most cities, but not in all cities. One example is Singapore, where pid changes year-to-year. Cities in which the number of pid are not constant over the analysis period (2016-2023):

* Accra
* Bogota
* Dubai
* Jakarta
* Lima
* Melbourne
* Singapore
* Stockholm
* Sydney
* Tokyo

# City names and harmonization

Most city names are the same across data sets, but not all. There are 142 cities in the PROVIDE data set.

There are 180 cities in the SGS data set used for URGED and developed in Falchetta and Hammad (2025).

**Spelling differences** could be found for:
Salvador, Addis Abeba, Teheran, Rotterdam, Belgrado, Ho Chi Minh

Project city names are stored in the following locations:

- `results/ghs_subregion_Cls.rds` 
- `DUMMY`

## Elevation
Elevation for the computation of WBT/WBGT is obtained reading the elevation from the existing data set, and then the *bigleaf* package to compute standard pressure for that elevation:

	mutate(patm = bigleaf::pressure.from.elevation(EL_AV_ALS.x, 273.15) * 10)
	