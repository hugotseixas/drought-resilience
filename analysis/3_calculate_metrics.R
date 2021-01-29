## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
## Script name:   Calculate metrics
##
## Description:   In this routine, we calculate different metrics of a system
##                state (related to vegetation and climatic states), and also
##                resilience components (stability, impact and recovery).
##                All these variables are calculated for each scenario,
##                each variable, and each cell.
##                Check the study document for more information about the 
##                methodology.
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

#### ---------------------------------------------- Load and organize data ####

####' ----- Load data processed from merge_data script #### 
scen_year <- read_delim('./data/netcdf_year.txt', delim = ',')

####' ----- Create information about the states of the ecosystem  #### 
## Before, during, and after a disturbance event
scen_year <- scen_year %>%
  group_by(Scenario, Time) %>%
  group_nest() %>%
  filter(Scenario != 'scen_01') %>% # Remove the scenario 01
  ## Create the ecosystem states sequences for each scenario
  mutate(Period = rep(c(
    'baseline', 'baseline', 'impact', 'impact', 'impact', 
    'impact', 'recovery', 'recovery', 'recovery', 'recovery',
    'baseline', 'baseline', 'impact', 'impact', 'impact', 
    'recovery', 'recovery', 'recovery', 'recovery', 'normal',
    'baseline', 'baseline', 'impact', 'impact', 'recovery', 
    'recovery', 'recovery', 'recovery', 'normal', 'normal',
    'baseline', 'baseline', 'impact', 'recovery', 'recovery', 
    'recovery', 'recovery', 'normal', 'normal', 'normal'
    ), 12)) %>%
  mutate(Year = year(Time)) %>%
  unnest(cols = data)

#### ---------- Calculate ecosystem state metrics for vegetation variables ####

####' ----- Quantify the first state of the ecosystem #### 
## Before the disturbance, it serves as a baseline, 
## and represents a "normal" state
s_to <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  filter(Year == 2004) %>% 
  ungroup() %>% 
  rename(s_to = Value) %>%  
  select(Scenario, x, y, Variable, s_to) 

####' ----- Get the lower ecosystem state during the disturbance #### 
## It represents the year with the higher disturbance in the time series
s_ti1 <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  slice(which(Period == 'impact')) %>% 
  slice(which.min(Value)) %>% 
  ungroup() %>%
  select(Scenario, x, y, Variable, Value, Year) %>%
  rename(s_ti1 = Value, ti1 = Year)

####' ----- Get the ecosystem state in last year of disturbance #### 
s_ti2 <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  slice(which(Period == 'impact')) %>% 
  slice(which.max(Year)) %>% 
  ungroup() %>%  
  select(Scenario, x, y, Variable, Value, Year) %>% 
  rename(s_ti2 = Value, ti2 = Year) 

####' ----- Get the recovery year #### 
## Year where the ecosystem state reaches the "normal" state 
## after the disturbance
s_tr <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  left_join(s_to, by = c('Scenario', 'x', 'y', 'Variable')) %>% 
  slice(which(Period == 'recovery')) %>% 
  ## Get the first occurrence when the ecosystem state 
  ## reaches the values of s_to in a margin of 5%
  slice(which(Value >= 0.95 * s_to), which.min(Year)) %>% 
  select(Scenario, x, y, Variable, Value, Year) %>% 
  rename(s_tr = Value, tr = Year) %>% 
  slice(which.min(tr)) %>% 
  ungroup()

####' ----- Calculate the stability of the ecosystem  #### 
## This is considered as a resilience component in the study
s_stbl <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>%
  ## Remove the "normal" years as this can create a bias in the component 
  ## (scenarios have different number of "normal" years)
  filter(Period != 'normal') %>% 
  summarise(Stability = sd(Value) / mean(Value))

####' ----- Calculate the other resilience components #### 
s_metrics <- s_to %>%
  right_join(s_ti1, by = c('Scenario', 'x', 'y', 'Variable')) %>% 
  right_join(s_ti2, by = c('Scenario', 'x', 'y', 'Variable')) %>% 
  right_join(s_tr, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  right_join(s_stbl, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  mutate(
    Impact = 1 - (s_ti1 / s_to), 
    ## We tested different formulas to calculate recovery
    Recovery = s_tr / s_ti2, 
    Recovery_Base = (s_tr - s_ti2) / s_to, 
    Recovery_Impact = (s_tr - s_ti2) / (s_to - s_ti1)
    ) %>% 
  ## Apply some rules to turn into NA cases that don't follow expectations
  ## This is due to spatial variability of precipitation
  mutate_at(
    vars(Impact, Recovery, Recovery_Base, Recovery_Impact, Stability), 
    ~case_when(
      s_tr < s_ti2 ~ NA_real_, 
      s_ti1 > s_to ~ NA_real_, 
      s_ti2 > s_to ~ NA_real_, 
      TRUE ~ .
      )
    ) 

#### ------------ Calculate ecosystem state metrics for climatic variables ####

## These are basically the same steps applied above, 
## but now on precipitation data

####' ----- Quantify the first state of the ecosystem #### 
p_to <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y) %>% 
  filter(Year == 2004) %>% 
  rename(p_to = Precip) %>% 
  ungroup() %>% 
  select(Scenario, x, y, Variable, p_to) 

####' ----- Get the lower ecosystem state during the disturbance #### 
p_ti1 <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  slice(which(Period == 'impact')) %>% 
  slice(which.min(Precip)) %>% 
  rename(p_ti1 = Precip) %>% 
  ungroup() %>%   
  select(Scenario, x, y, Variable, p_ti1)

####' ----- Get the precipitation in last year of disturbance #### 
p_ti2 <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  slice(which(Period == 'impact')) %>% 
  slice(which.max(Year)) %>% 
  rename(p_ti2 = Precip) %>% 
  ungroup() %>% 
  select(Scenario, x, y, Variable, p_ti2)

####' ----- Get the recovery year #### 
p_tr <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  ## Join table with the s_tr information, 
  ## it will serve to tell us the year of recovery
  right_join(s_tr, by = c('Scenario', 'x', 'y', 'Variable')) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  slice(which(Period == 'recovery')) %>% 
  # Extract the year in which the system was considered to be recovered
  slice(which(Year == tr)) %>% 
  rename(p_tr = Precip, tp = Year) %>% 
  ungroup() %>%
  select(Scenario, x, y, Variable, p_tr, tp) 

####' ----- Calculate precipitation stability ####
p_stbl <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  filter(Period != 'normal') %>% 
  summarise(Precip_Stability = sd(Precip) / mean(Precip)) 

####' ----- Calculate the other resilience components #### 
p_metrics <- p_to %>%
  right_join(p_ti1, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  right_join(p_ti2, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  right_join(p_tr, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  right_join(p_stbl, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  mutate(
    Precip_Impact = 1 - (p_ti1 / p_to), 
    Precip_Recovery = p_tr / p_ti2, 
    Precip_Recovery_Base = (p_tr - p_ti2) / p_to, 
    Precip_Recovery_Impact = (p_tr - p_ti2) / (p_to - p_ti1)
    ) %>% 
  mutate_at(
    vars(Precip_Impact, Precip_Recovery), 
    ~case_when(
      p_tr < p_ti2 ~ NA_real_, 
      p_ti1 > p_to ~ NA_real_, 
      p_ti2 > p_to ~ NA_real_, 
      TRUE ~ .
      )
    ) 

####' ----- Calculate temperature stability #### 
t_metrics <- scen_year %>%
  gather(key = Variable, value = Value, GPP, LAI, LH) %>% 
  group_by(Scenario, x, y, Variable) %>% 
  filter(Period != 'normal') %>% 
  summarise(Temp_Stability = sd(Temp) / mean(Temp)) 

#### ---------------------------------------------- Merge and save results ####

####' ----- Merge vegetation and climatic variables #### 
metrics <- s_metrics %>%
  right_join(p_metrics, by = c('Scenario', 'x', 'y', 'Variable')) %>%
  right_join(t_metrics, by = c('Scenario', 'x', 'y', 'Variable'))

####' ----- Save table as a comma delimited file  #### 
## This file will be loaded in the next script: create_nc_file.R
metrics %>%
  write_delim('./data/resilience_metrics.txt', delim = ',')

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##