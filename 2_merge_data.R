## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
## Script name:   Merge data
##
## Description:   In this routine, we open NetCDF files extracted from NOAH-MP  
##                output and extract information of some variables to a tabular 
##                format, the final product is one table with information
##                of all scenarios produced by the model.              
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
library(tidyverse)
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

#### ---------------------------------------- Open and process NetCDF data #### 

####' ----- Function to process NetCDF data #### 
## Function to open NetCDF files, extract and organize information, 
## and finally merge everything into one tibble
merge_data <- function(scen) {
  
  ## Open NetCDF file corresponding to one scenario
  nc <- tidync(paste('./scenarios/', scen, sep = '')) 
  
  ## Transform data into tibble and calculate yearly values
  nc <- nc %>%
    activate('D2,D3,D0') %>% # Activate dimensions of variables of interest
    hyper_tibble() %>% # Transform the tidync object into a tibble
    mutate(Times = ymd(Times)) %>%
    rename(Time = Times, Precip = RAINRATE, Temp = TRAD) %>% 
    ## Transform daily sums to daily means to some variables 
    ## (check the pre-processing routine to understand why)
    mutate_at(vars(Temp, LAI, LH), ~./24) %>%
    ## Remove one huge outlier that appeared in some scenarios
    mutate_at(vars(GPP, LH, Temp), ~replace(., abs(.) > 2000, NA)) %>% 
    as_tbl_time(index = Time) %>% 
    collapse_by(period = 'yearly') %>%
    group_by(Time, x, y) %>%
    summarise_at(
      vars(Precip, Temp, GPP, LAI, LH), 
      list(sum = sum, mean = mean), 
      na.rm = TRUE
      ) %>% # Calculate sum and mean for the extracted variables
    select(-c(Precip_mean, Temp_sum, GPP_mean, LAI_sum, LH_sum)) %>%
    rename(
      Precip = Precip_sum, 
      Temp = Temp_mean, 
      GPP = GPP_sum, 
      LAI = LAI_mean, 
      LH = LH_mean
      ) %>%
    ungroup() %>% # Ungroup variables
    mutate(Time = floor_date(Time, unit = 'year')) %>% 
    as_tibble() %>% 
    group_by(Time) %>%
    mutate(Scenario = str_remove(scen, '.nc'))
  
  return(nc) # Return the resulting tibble
  
}

####' ----- Create a list of all NetCDF files from NOAH-MP ####
scen_list <- dir('./data/scenarios/') 

####' ----- Map the function "merge_data" to all files ####
scen_year <- {
  as_vector(scen_list) %>%
    map_df(merge_data)
}

#### -------------------------------------------------------- Save results ####

####' ----- Save the results as a comma delimited file #### 
## (this file will be loaded in the next script: calculate_metrics.R)
scen_year %>%
  write_delim('./data/netcdf_year.txt', delim = ',')

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##