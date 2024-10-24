# URGED
 The main code is contained in the `URGED` folder.
 
## Code descriptions
 
**`0_build_cities_database.R`**

- Obtain the city list for the cities contained in the PROVIDE data
- Get Köppen-geiger classes using kgc package
- Get local climate zone using a raster file from https://journals.ametsoc.org/view/journals/bams/93/12/bams-d-11-00019.1.xml 
- Merge provide city names and GHS data using fuzzyjoin and stringdist_join()
- The result is visualized using mapview for an interactive map of cities colored by climate zone (kg_cl).
- The final dataset is saved as a GeoPackage file (results/cities_database_climatezones.gpkg), which can be used for further spatial analysis.

**`1_b_ugs_heat_metrics`**

- First load climate data for different time periods (2020, 2030, 2050, 2070, 2100): PROVIDE cooling degree hours, and urbclim max and min of daily mean T2
- We use GVI as urban green indicator (dubbed as variable out_ndvi_m)
- Analysis performed on 10 largest cities per region
- Summarize and transform the data into spatial objects (sf), preparing it for extraction of climate and other spatial information.
- Extract climate variables (e.g., cooling degree hours, temperature extremes) for the selected cities using exact_extract.
- Incorporate other spatial covariates such as building density/height, population density, poverty index, and water bodies.

**`1bb_ugs_statsmodels`**

- Fixed effects models to explore the relationship between GVI and climate vars (CDH) across cities, controlling for covariates (building height, population density, water, elevation, etc.).
- City-Level Analysis
- Visualize the results
- map plots