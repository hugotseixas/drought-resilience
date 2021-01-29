## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
## Script name:   Process NetCDF data
##
## Description:   description 
##                 
##                
##                 
##                
##                 
##                
##                
##
## Author:        Hugo Tameirao Seixas
## Contact:       hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
##
## Date created:  Fourth quarter 2019
## Last update:   2020-06-15
## Last tested:   2020-06-15
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
library(ncdf4)
library(tidyverse)
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

#### ----------------------------------------------------------- Load data ####

####' ----- Load NetCDF file #### 
nc <- tidync('./data/scenarios.nc')

print(nc)

#### ------------------------------------------- Process and organize data ####

####' ----- Get vegetation and soil information #### 
veg_soil <- nc %>%
  activate('D0,D1') %>%
  hyper_tibble()

####' ----- transform NetCDF object into a tibble #### 
res_metrics <- nc %>%
  hyper_tibble()

####' ----- Get variable dimension #### 
variables <- nc_open('./data/scenarios.nc') %>%
  ncvar_get('Variable') %>%
  enframe() %>%
  select(-name) %>%
  bind_cols(res_metrics %>% distinct(Variable_Dimension)) %>%
  rename(Variable = value)

####' ----- Get scenarios dimension #### 
scen <- nc_open('./data/scenarios.nc') %>%
  ncvar_get('Scenario') %>%
  enframe() %>%
  select(-name) %>%
  bind_cols(res_metrics %>% distinct(Scenario_Dimension)) %>%
  rename(Scenario = value)

####' ----- Get scenarios dimension id #### 
id <- nc_open('./data/scenarios.nc') %>%
  ncvar_get('id') %>%
  enframe() %>%
  select(-name) %>%
  bind_cols(res_metrics %>% distinct(Scenario_Dimension)) %>%
  rename(id = value)

####' ----- Merge and organize data into one tibble #### 
res_metrics <- res_metrics %>%
  left_join(variables, by = 'Variable_Dimension') %>%
  left_join(scen, by = 'Scenario_Dimension') %>%
  left_join(id, by = 'Scenario_Dimension') %>%
  right_join(veg_soil, by = c('Lon', 'Lat')) %>%
  select(Variable:id, Lat:Lon, Vegetation:Soil, Impact:Temp_Stability) %>% 
  separate(
    id, 
    into = c('Drought_Intensity', 'Recovery_Intensity', 'Drought_Duration'), 
    sep = '-', 
    remove = FALSE
  ) %>%
  mutate(Recovery_Intensity = fct_relevel(
    Recovery_Intensity, 'W1', 'W2', 'W3', 'N'
  )) %>%
  na_if(0) %>%
  drop_na()

rm(veg_soil, id, nc, scen, variables)

####' ----- Filter data #### 
## Filter desired variable, remove data that presents Impact lower than 10%, 
## and remove undesired vegetation type
res_metrics_sub <- res_metrics %>%
  filter(
    Variable == 'GPP', 
    # Low values of impact have a big effect on some results, causing confusion
    Impact > 0.10, 
    Precip_Impact > 0.10, 
    Vegetation >= 7, # here we filter the most common veg types in the area
    Vegetation <= 10
  ) %>%
  mutate(Vegetation = factor(
    Vegetation, 
    labels = c('Open Shrublands', 'Woody Savannas', 'Savannas', 'Grasslands')
  ))

#### -------------------------------------------------------- Save results ####

####' ----- Save table as a comma delimited file  #### 
## This file will be loaded in the next scripts: create_plots.R
##                                               run_analysis.R
##                                               spatial_animations.R
res_metrics_sub %>%
  write_delim('./data/resilience_metrics_sub.txt', delim = ',')

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##