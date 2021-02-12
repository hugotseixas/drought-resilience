# HEADER ----------------------------------------------------------------------
#
# Title:        Calculate metrics
# Description:  In this routine, we calculate different metrics of a system
#               state (related to vegetation and climatic states), and also
#               resilience components (stability, impact and recovery).
#               All these variables are calculated for each scenario,
#               each variable, and each cell.
#               Check the study document for more information about the 
#               methodology.
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
library(tibbletime)
library(lubridate)
library(arrow)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#

#
# LOAD AND ORGANIZE DATA ------------------------------------------------------

## Load data processed from merge_data script ----
scen_year <- read_parquet("./data/netcdf_year.parquet")

## Create information about the states of the ecosystem ----
# Before, during, and after a disturbance event
scen_states <- scen_year %>%
  distinct(scenario, time) %>%
  filter(scenario != "scen_01", year(time) >= 2003) %>%
  # Create the ecosystem states sequences for each scenario
  mutate(
    period = rep(
      c(
        "baseline", "baseline", "impact", "impact", "impact", 
        "impact", "recovery", "recovery", "recovery", "recovery",
        "baseline", "baseline", "impact", "impact", "impact", 
        "recovery", "recovery", "recovery", "recovery", "normal",
        "baseline", "baseline", "impact", "impact", "recovery", 
        "recovery", "recovery", "recovery", "normal", "normal",
        "baseline", "baseline", "impact", "recovery", "recovery", 
        "recovery", "recovery", "normal", "normal", "normal"
      ), 
      12
    )
  ) %>%
  mutate(year = year(time))

## Join tables ----
scen_year <- scen_year %>%
  filter(scenario != "scen_01") %>%
  left_join(scen_states, by = c("scenario", "time"))

# CALCULATE METRICS -----------------------------------------------------------

## Quantify the first state of the variables ----
# Before the disturbance, it serves as a baseline,
# and represents a "normal" state
s_to <- scen_year %>%
  gather(key = variable, value = value, gpp, precip) %>%
  filter(year == 2004) %>%
  rename(s_to = value) %>%
  select(scenario, x, y, variable, s_to)

## Get the lower variables values during the disturbance ----
# It represents the year with the higher disturbance in the time series
s_ti1 <- scen_year %>%
  gather(key = variable, value = value, gpp, precip) %>%
  group_by(scenario, x, y, variable) %>%
  slice(which(period == "impact")) %>%
  slice(which.min(value)) %>%
  ungroup() %>%
  select(scenario, x, y, variable, value, year) %>%
  rename(s_ti1 = value, ti1 = year)

## Get the variables values in last year of disturbance ----
s_ti2 <- scen_year %>%
  gather(key = variable, value = value, gpp, precip) %>%
  group_by(scenario, x, y, variable) %>%
  slice(which(period == "impact")) %>%
  slice(which.max(year)) %>%
  ungroup() %>%
  select(scenario, x, y, variable, value, year) %>%
  rename(s_ti2 = value, ti2 = year)

## Get the recovery year ----
# Year where the variables reaches the "normal" state 
# after the disturbance
s_tr <- scen_year %>%
  gather(key = variable, value = value, gpp, precip) %>%
  group_by(scenario, x, y, variable) %>%
  left_join(s_to, by = c("scenario", "x", "y", "variable")) %>%
  slice(which(period == "recovery")) %>%
  # Get the first occurrence when the ecosystem state
  # reaches the values of s_to in a margin of 5%
  slice(which(value >= 0.95 * s_to), which.min(year)) %>%
  select(scenario, x, y, variable, value, year) %>%
  rename(s_tr = value, tr = year) %>%
  slice(which.min(tr)) %>%
  ungroup()

## Calculate the stability of the variables ----
# This is considered as a resilience component in the study
s_stbl <- scen_year %>%
  gather(key = variable, value = value, gpp, precip) %>%
  group_by(scenario, x, y, variable) %>%
  # Remove the "normal" years as this can create a bias in the component 
  # (scenarios have different number of "normal" years)
  filter(period != "normal") %>% 
  summarise(stability = sd(value) / mean(value), .groups = "drop")

## Calculate the other resilience components ----
s_metrics <- 
  reduce(
    list(s_to, s_ti1, s_ti2, s_tr, s_stbl), 
    full_join, 
    by = c("scenario", "x", "y", "variable")
  ) %>%
  mutate(
    impact = 1 - (s_ti1 / s_to), 
    # We tested different formulas to calculate recovery
    recovery = s_tr / s_ti2, 
    recovery_base = (s_tr - s_ti2) / s_to, 
    recovery_impact = (s_tr - s_ti2) / (s_to - s_ti1)
    ) %>% 
  # Apply some rules to turn into NA cases that don't follow expectations
  # This is due to spatial variability of precipitation
  mutate_at(
    vars(impact, recovery, recovery_base, recovery_impact, stability), 
    ~ case_when(
      s_tr < s_ti2 ~ NA_real_, 
      s_ti1 > s_to ~ NA_real_, 
      s_ti2 > s_to ~ NA_real_, 
      TRUE ~ .
    )
  )

# SAVE RESULTS ----------------------------------------------------------------

## Save table as a comma delimited file ----
# This file will be loaded in the analysis scripts
s_metrics %>%
  inner_join(
    scen_year %>%
      distinct(
        x, y, lat, lon, id, scenario, scen_code,
        drought_intensity, recovery_intensity, drought_duration,
        soil_type, veg_type
      ),
    by = c("scenario", "x", "y")
  ) %>%
  write_parquet('./data/resilience_metrics.parquet')
