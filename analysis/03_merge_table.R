# HEADER ----------------------------------------------------------------------
#
# Title:        Merge data
# Description:  In this routine, we open NetCDF files extracted from NOAH-MP  
#               output and extract information of some variables to a tabular 
#               format, the final product is one table with information
##              of all scenarios produced by the model.
#
# Author:       Hugo Tameirao Seixas
# Contact:      tameirao.hugo@gmail.com
# Date:         2019-05-01
#
# Notes:        Versions of software and libraries used in this  
#               routine are detailed in the documentation located  
#               in the project directory.      
#
# LIBRARIES -------------------------------------------------------------------
#
library(tidync)
library(tibbletime)
library(lubridate)
library(fs)
library(arrow)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#

#
# MODEL AUXILIARY OUTPUT -------------------------------------------------

## Open HRLDAS setup file ----
setup <- tidync('./data/HRLDAS_setup_2015010100_d1')

## Extract information from HRLDAS setup file ----

### Extract vegetation type info from HRLDAS setup file ----
veg_type <- setup %>%
  activate(IVGTYP) %>% # Activate vegetation type dimensions
  hyper_tibble() %>% # Transform into a tibble
  select(-Time) %>% 
  ## Rename coordinate variables to match NOAH's output files
  rename(x = west_east, y = south_north, veg_type = IVGTYP) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number())

### Extract soil type info from HRLDAS setup file ----
soil_type <- setup %>%
  activate(ISLTYP) %>% # Activate soil type dimensions
  hyper_tibble() %>% # Transform into a tibble
  select(-Time) %>% 
  ## Rename coordinate variables to match NOAH's output files
  rename(x = west_east, y = south_north, soil_type = ISLTYP) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number())

### Extract coordinates info from HRLDAS setup file ----
coordinates <- setup %>%
  activate('D2,D3,D0') %>% # Activate coordinates dimensions and variables
  hyper_tibble() %>% 
  select(XLAT, XLONG, west_east, south_north) %>% 
  rename(x = west_east, y = south_north, lat = XLAT, lon = XLONG) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number())

## Join aux tables ----
aux <- 
  reduce(
    list(coordinates, soil_type, veg_type),
    full_join, 
    by = c("id", "x", "y")
  )

# NETCDF DATA -----------------------------------------------------------------

## Create scenarios list ----
scen_list <- dir_ls("data/noah_out/scenarios/")

## Characterize scenarios ----
scen_table <-
  tibble(
    scenario = str_extract(
      scen_list, 
      "(?<=data/noah_out/scenarios/).*(?=.nc)"
    ),
    drought_intensity = factor(
      x = c("N", rep(c("D1", "D2", "D3"), each = 16)), 
      levels = c("N", "D1", "D2", "D3")
    ),
    recovery_intensity = factor(
      c("N", rep(c("N", "W1", "W2", "W3"), each = 4, times = 3)), 
      levels = c("N", "W1", "W2", "W3")
    ),
    drought_duration = c(0, factor(rep(4:1, times = 12)))
  ) %>%
  unite(
    'scen_code', 
    drought_intensity:drought_duration, 
    sep = '-', 
    remove = FALSE
  )

## Merge nc files into one table ----
scen_year <-
  map_df(
    .x = scen_list,
    .f = 
      ~ {
        
        # Open NetCDF file corresponding to one scenario
        nc <- tidync(.x)
        
        # Transform data into tibble and calculate yearly values
        nc_table <- 
          nc %>%
          # Activate dimensions of variables of interest
          activate('D2,D3,D0') %>% 
          hyper_tibble() %>% # Transform the tidync object into a tibble
          mutate(Times = ymd(Times)) %>%
          rename(time = Times, precip = RAINRATE, gpp = GPP) %>% 
          # Remove one huge outlier that appeared in some scenarios
          mutate_at(vars(gpp), ~replace(., abs(.) > 2000, NA)) %>% 
          as_tbl_time(index = time) %>% 
          collapse_by(period = 'yearly') %>% # Group by year
          group_by(time, x, y) %>%
          summarise_at( # Calculate yearly sum
            vars(precip, gpp), 
            ~ sum(., na.rm = TRUE)
          ) %>%
          ungroup() %>% # Ungroup variables
          mutate(time = floor_date(time, unit = 'year')) %>% 
          as_tibble() %>% 
          mutate( # Create column with scenario name
            scenario = str_extract(
              .x, 
              "(?<=data/noah_out/scenarios/).*(?=.nc)"
            )
          )
        
      }
  )

# MERGE AND SAVE RESULTS ------------------------------------------------------

## Join tables ----
scen_year <- scen_year %>%
  left_join(scen_table, by = "scenario") %>%
  left_join(aux, by = c("x", "y"))

## Save the results as a parquet file ----
# (this file will be loaded in the next script: calculate_metrics.R)
scen_year %>%
  write_parquet('./data/netcdf_year.parquet')
