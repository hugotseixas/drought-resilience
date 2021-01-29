# HEADER ----------------------------------------------------------------------
#
# Title:        Merge NetCDF data
# Description:  In this routine we calculate monthly sums of selected variables
#               from the daily outputs from NOAH-MP and concatenate them in
#               a single file for each scenario.
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
library(fs)
library(lubridate)
library(glue)
library(furrr)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
plan(multisession, workers = 10) # Plan parallel processing
#
## List all scenarios folders ----
dlist <- dir_ls('./data/noah_out/', regexp = "scen")

## Calculate monthly sums and concatenate files ----
walk(
  .x = dlist,
  function(dir) {
    
    # Get scenario number
    scen <- 
      str_extract(
        string = dir, 
        pattern = glue("(?<=./data/noah_out/).*")
      )
    
    cat(scen, "\n")
    
    # Get all the files of scenario
    flist <- dir_ls(dir)
    
    # Get the years
    ylist <- 
      unique(
        year(
          ymd_h(
            str_extract(
              string = flist, 
              pattern = glue("(?<={dir}/).*(?=.LDASOUT_DOMAIN1_sub)")
            )
          )
        )
      )
      
    # Calculate the sum for each month of each year
    future_walk(
      .x = ylist,
      function(year) {
        
        cat(year, "\n")
        
        system(
          glue(
            "cdo -s -monsum -selvar,GPP,LAI,LH,RAINRATE,TRAD -cat ",
            "{dir}/{year}* ./data/noah_out/merged/{scen}_{year}.nc"
          )
        )
        
      }
    )
    
    # Concatenate the monthly sum files
    system(
      glue(
        "cdo -s -cat ./data/noah_out/merged/{scen}* ",
        "./data/noah_out/scenarios/{scen}.nc"
      )
    )
    
  }
)
