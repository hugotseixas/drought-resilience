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
# OPEN AND PROCESS NETCDF DATA ------------------------------------------------

scen_list <- dir_ls("data/noah_out/scenarios/")

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

# SAVE RESULTS ----------------------------------------------------------------

# Save the results as a comma delimited file
# (this file will be loaded in the next script: calculate_metrics.R)
scen_year %>%
  write_parquet('./data/netcdf_year.parquet')
