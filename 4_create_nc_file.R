## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
## Script name:   Create NetCDF file
##
## Description:   In this routine, we gather the data saved in the file 
##                resilience_metrics.txt, and compress it into a NetCDF format.
##                Although the information contained in both files are 
##                basically the same (with the addition of vegetation and soil
##                type information). The choice of creating a NetCDF file
##                is to facilitate its availability due to its smaller size.
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:  Fourth quarter 2019
## Last update:   2020-06-12
## Last tested:   2020-06-12
##
## Copyright (c) Hugo Tameirao Seixas, 2020
##
## ------------------------------------------------------------------------- ##
##
## Notes:         Versions of software and libraries used in this  
##                routine are detailed in the readme file located  
##                in the project directory.              
##
## ------------------------------------------------------------------------- ##
##
options(scipen = 6, digits = 4) # View outputs in non-scientific notation
##
## ------------------------------------------------------------------------- ##
##
## Load libraries:
##
library(tidync)
library(tibbletime)
library(lubridate)
library(ncdf4)
library(tidyverse)
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

#### ---------------------------------------------------------- Open files ####

####' ----- Open resilience components data #### 
res_metrics <- read_delim('./data/resilience_metrics.txt', delim = ',') 

####' ----- Open HRLDAS setup file #### 
setup <- tidync('./data/HRLDAS_setup_2015010100_d1')

#### -------------------------- Extract information from HRLDAS setup file ####

####' ----- Extract vegetation type info from HRLDAS setup file #### 
veg_type <- setup %>%
  activate(IVGTYP) %>% # Activate vegetation type dimensions
  hyper_tibble() %>% # Transform into a tibble
  select(-Time) %>% 
  ## Rename coordinate variables to match NOAH's output files
  rename(x = west_east, y = south_north) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number()) 

####' ----- Extract soil type info from HRLDAS setup file ####
soil_type <- setup %>%
  activate(ISLTYP) %>% # Activate soil type dimensions
  hyper_tibble() %>% # Transform into a tibble
  select(-Time) %>% 
  ## Rename coordinate variables to match NOAH's output files
  rename(x = west_east, y = south_north) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number())

####' ----- Extract coordinates info from HRLDAS setup file ####
coordinates <- setup %>%
  activate('D2,D3,D0') %>% # Activate coordinates dimensions and variables
  hyper_tibble() %>% 
  select(XLAT, XLONG, west_east, south_north) %>% 
  rename(x = west_east, y = south_north) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number())

#### ------------------------------ Merge and process data from both files ####

####' ----- Merge data #### 
metrics <- res_metrics %>%
  group_by(x, y) %>%
  group_nest() %>%
  arrange(x, y) %>%
  mutate(id = row_number()) %>%
  right_join(veg_type, by = c('id', 'x', 'y')) %>%
  right_join(soil_type, by = c('id', 'x', 'y')) %>%
  right_join(coordinates, by = c('id', 'x', 'y')) %>%
  rename(Vegetation = IVGTYP, Soil = ISLTYP, Lat = XLAT, Lon = XLONG) %>%
  select(-id) %>%
  unnest(cols = data)

####' ----- Remove unwanted land cover information #### 
metrics <- metrics %>%
  mutate_at( # Turn into NA data from unwanted land cover
    vars(Stability:Recovery, Precip_Stability:Temp_Stability), 
    ~replace(., Vegetation > 10, NA) # Check MODIS IGBP classes
  ) %>%
  mutate_at( # Replace NA to 0
    vars(Stability:Recovery, Precip_Stability:Temp_Stability), 
    ~replace(., NA_real_, 0) 
  )

####' ----- Organize data according to variable and scenario #### 
metrics <- metrics %>%
  mutate(
    Scen_Number = as.numeric(str_remove(Scenario, 'scen_')),
    Var_Num = factor(Variable, labels = c(1, 2, 3))
  ) %>%
  arrange(Lat, Lon, Variable, Scenario)

####' ----- Characterize scenarios #### 
metrics <- metrics %>%
  group_by(Scenario) %>%
  group_nest() %>%
  mutate(
    Drought_Intensity = factor(
      rep(c('D1', 'D2', 'D3'), each = 16), 
      levels = c('D1', 'D2', 'D3')
    ), 
    Recovery_Intensity = factor(
      rep(c('N', 'W1', 'W2', 'W3'), each = 4, times = 3), 
      levels = c('N', 'W3', 'W2', 'W1')
    ), 
    Drought_Duration = factor(rep(4:1, times = 12))
  ) %>%
  unite('id', Drought_Intensity:Drought_Duration, sep = '-') %>%
  unnest(cols = data)

rm(coordinates, res_metrics, setup, soil_type,veg_type)

#### -------------------------------------------------- Create NetCDF file ####

####' ----- Define spatial dimensions #### 
dim_lon <- ncdim_def(
  name = 'Lon', 
  units = '', 
  vals = as.double(pull(metrics %>% distinct(Lon)))
)

dim_lat <- ncdim_def(
  name = 'Lat', 
  units ='', 
  vals = as.double(pull(metrics %>% distinct(Lat)))
)

####' ----- Define scenarios and variable dimensions #### 
dim_scen <- ncdim_def(
  name = 'Scenario_Dimension', 
  units = '', 
  vals = as.integer(pull(metrics %>% distinct(Scen_Number)))
)

dim_var <- ncdim_def(
  name = 'Variable_Dimension', 
  units = '', 
  vals = as.integer(pull(metrics %>% distinct(Var_Num))), 
  create_dimvar = FALSE
)

dim_nchar_scen <- ncdim_def(
  name = 'Scenario_Character_Lenght', 
  units = '', 
  vals = 1:7, 
  create_dimvar = FALSE
)

dim_nchar_var <- ncdim_def(
  name = 'Variable_Character_Lenght', 
  units = '', 
  vals = 1:3, 
  create_dimvar = FALSE)

####' ----- Define variables #### 
var_impact <- ncvar_def(
  name = 'Impact', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_recovery <- ncvar_def(
  name = 'Recovery', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_recovery_base <- ncvar_def(
  name = 'Recovery_Base', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_recovery_impact <- ncvar_def(
  name = 'Recovery_Impact', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_stability <- ncvar_def(
  name = 'Stability', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_precip_impact <- ncvar_def(
  name = 'Precip_Impact', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_precip_recovery <- ncvar_def(
  name = 'Precip_Recovery', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_precip_recovery_base <- ncvar_def(
  name = 'Precip_Recovery_Base', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_precip_recovery_impact <- ncvar_def(
  name = 'Precip_Recovery_Impact', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_precip_stability <- ncvar_def(
  name = 'Precip_Stability', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_temp_stability <- ncvar_def(
  name = 'Temp_Stability', 
  units = '', 
  dim = list(dim_lat, dim_lon, dim_var, dim_scen)
)

var_vegetation <- ncvar_def(
  name = 'Vegetation', 
  units = '', 
  dim = list(dim_lat, dim_lon)
)

var_soil <- ncvar_def(
  name = 'Soil', 
  units = '', 
  dim = list(dim_lat, dim_lon)
)

var_var <- ncvar_def(
  name = 'Variable', 
  units = '', 
  dim = list(dim_nchar_var, dim_var), 
  prec = 'char'
)

var_scen <- ncvar_def(
  name = 'Scenario', 
  units = '', 
  dim = list(dim_nchar_scen, dim_scen), 
  prec = 'char'
)

var_id <- ncvar_def(
  name = 'id', 
  units = '', 
  dim = list(dim_nchar_scen, dim_scen), 
  prec = 'char'
)

####' ----- Create the NetCDF file #### 
nc_out <- nc_create(
  filename = './data/scenarios.nc', 
  vars = list(
    var_impact, 
    var_recovery, 
    var_recovery_base, 
    var_recovery_impact, 
    var_stability, 
    var_precip_impact, 
    var_precip_recovery, 
    var_precip_recovery_base, 
    var_precip_recovery_impact,
    var_precip_stability, 
    var_temp_stability,
    var_vegetation, 
    var_soil, 
    var_var, 
    var_scen, 
    var_id
  ),
  force_v4 = TRUE
)

####' ----- Add data to NetCDF file #### 
ncvar_put(
  nc = nc_out, 
  varid = var_impact, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Impact)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_recovery, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Recovery)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_recovery_base, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Recovery_Base)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_recovery_impact, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Recovery_Impact)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_stability, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Stability)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_precip_impact, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Precip_Impact)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_precip_recovery, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Precip_Recovery)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_precip_recovery_base, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Precip_Recovery_Base)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_precip_recovery_impact, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Precip_Recovery_Impact)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_precip_stability, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Precip_Stability)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_temp_stability, 
  vals = metrics %>% 
    arrange(Scenario, Variable, Lon, Lat) %>% 
    pull(Temp_Stability)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_soil, 
  vals = metrics %>% 
    group_by(Lon, Lat, Soil) %>% 
    group_nest() %>% 
    arrange(Lon, Lat) %>% 
    pull(Soil)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_vegetation, 
  vals = metrics %>% 
    group_by(Lon, Lat, Vegetation) %>% 
    group_nest() %>% 
    arrange(Lon, Lat) %>% 
    pull(Vegetation)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_var, 
  vals = metrics %>% 
    distinct(Variable) %>% 
    pull(Variable)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_scen, 
  vals = metrics %>% 
    distinct(Scenario) %>% 
    pull(Scenario)
  )

ncvar_put(
  nc = nc_out, 
  varid = var_id, 
  vals = metrics %>% 
    distinct(id) %>% 
    pull(id)
  )

## Close NetCDF file
nc_close(nc_out)

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##