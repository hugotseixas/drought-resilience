

# Data collection to reproduce the results from the article: "Exploring the vegetation resilience concept with land surface model scenarios"

The whole raw output from the model could not be made available due to its size. So a small subset of the NOAH-MP output is available in this collection to provide a minimal reproducible example of the pre-processing scripts.

The descriptions of the files are listed below:

### /data/

 -> HRLDAS_setup_2015010100_d1 - Setup file for NOAH-MP simulations, it contains spatial data used by the model;
 
 -> netcdf_year.parquet - Tabular data containing yearly values of GPP and Precipitation from NOAH-MP, extracted from NetCDF files;
 
 -> precip_annual.csv / precip_monthly.csv - Annual and monthly precipitation data from GLDAS and stations nearby the study area;
 
 -> resilience_metrics.parquet - Tabular data containing the calculated resilience metrics;
 
 -> scenarios_years.txt - The sequence of years of each scenario to be simulated by NOAH-MP;

### /data/noah_out/

### /data/noah_out/merged/

 -> scen_XX_XXXX.nc - NetCDF files containing yearly values for each scenario;

### /data/noah_out/scen_XX/

-> YYYYMMDDHH.LDASOUT_DOMAIN1_sub - Output file from NOAH-MP (Only a subset of all files are available);

### /data/noah_out/scenarios/

-> scen_XX.nc - Concatenated yearly NetCDF files from NOAH-MP (From all the data);
