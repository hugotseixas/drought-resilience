# HEADER ----------------------------------------------------------------------
#
# Title:         Precipitation analysis
# Description:   In this routine, we used precipitation data from 
#                meteorological stations from Petrolina and Juazeiro, 
#                and reanalysis data from GLDAS. 
#                The objective is to create a set of classes of years from 
#                GLDAS, based in the distribution of precipitation data from
#                meteorological stations. 
#                These classes were used to create scenarios of forcing data 
#                served as input to run NOAH-MP model.
#
# Author:       Hugo Tameirao Seixas
# Contact:      tameirao.hugo@gmail.com
# Date:         2019-09-01
#
# Notes:         Versions of software and libraries used in this  
#                routine are detailed in the documentation located  
#                in the project directory.      
#
# LIBRARIES -------------------------------------------------------------------
#
library(lubridate)
library(kableExtra)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
options(scipen = 6, digits = 4) # View outputs in non-scientific notation
#
# EXPLORING RAINFALL DATA -----------------------------------------------------

## Load precipitation files ----
precip_annual <- read_csv('./data/precip_annual.csv')
precip_monthly <- read_csv('./data/precip_monthly.csv')

## Plot time series of annual precipitation ----
precip_annual %>%
  select(-GLDAS2_0) %>% # Remove GLDAS 2.0 dataset
  mutate(Year = ymd(Year, truncated = 2)) %>%
  gather(key = Source, value = Precip, -Year) %>%
  filter(Year >= 1975) %>%
  drop_na() %>%
  ggplot(aes(x = Year, y = Precip)) +
  geom_line(aes(linetype = Source, color = Source)) +
  scale_color_manual(
    values = c('#940933', '#0D0D0DFF', '#0D0D0DFF'),
    labels = c('GLDAS 2.1', 'Juazeiro', 'Petrolina')
  ) +
  scale_linetype_manual(
    values = c('solid', 'solid', 'dashed'),
    labels = c('GLDAS 2.1', 'Juazeiro', 'Petrolina')
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  ylab("Precipitation (mm)") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.8),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(
    "./plots/Annual_Precipitation.svg",
    width = 150,
    height = 55,
    units = "mm",
    bg = "transparent"
  )

# CLASSIFY YEARS ACCORDING RAINFALL -------------------------------------------

## Calculate quantiles based on meteorological stations ----
(
  quant <- 
    precip_annual %>%
    select(Petrolina, Juazeiro) %>% # Only from meteorological stations
    gather(key = Source, value = Precip) %>%
    drop_na() %>%
    summarise(
      Precip = list(
        enframe(
          quantile(
            Precip,
            probs = c(0.05, 0.40, 0.60, 0.95),
            na.rm = TRUE
          ),
          name = 'Quantile',  
          value = 'Precip'
        )
      )
    ) %>% 
    unnest(cols = Precip)
)

## Classify years from meteorological stations ---- 
# Each year is classified as wet, dry, or normal. 
# Dry years are between 5% and 40%, Normal years between 40% and 60%
# and Wet years between 60% and 95%
(
  precip_annual <- 
    precip_annual %>%
    gather(key = Source, value = Precip, -Year) %>% 
    mutate(
      Condition = case_when(
        Precip >= pull(quant[1,2]) & Precip < pull(quant[2,2]) ~ 'Dry',
        Precip >= pull(quant[2,2]) & Precip <= pull(quant[3,2]) ~ 'Normal',
        Precip > pull(quant[3,2]) & Precip <= pull(quant[4,2]) ~ 'Wet'
      )
    )
)

## Get min/mean/max precip from stations ----
(
  station_summary <-
    precip_annual %>% 
    filter(!str_detect(Source, 'GLDAS')) %>% # Remove gldas data
    group_by(Condition) %>% 
    drop_na(Condition) %>%
    summarise(
      min = min(Precip, na.rm = TRUE), 
      mean = mean(Precip, na.rm = TRUE),
      max = max(Precip, na.rm = TRUE)
    )
)

# CLASSIFY YEARS FROM GLDAS 2.1 -----------------------------------------------
# (based on stations)

## Get dry years from GLDAS ----
(
  dry_years <- 
    precip_annual %>%
    filter(Source == 'GLDAS2_1', Condition == 'Dry') %>% 
    spread(key = Source, value = Precip) %>% 
    drop_na(GLDAS2_1) %>%
    # Get difference between "Dry" GLDAS years and minimum/mean/maximum 
    # precipitation of "Dry" years from meteorological stations
    mutate(
      Min_Dif = abs(GLDAS2_1 - station_summary[[1,2]]), 
      Mean_Dif = abs(GLDAS2_1 - station_summary[[1,3]]),
      Max_Dif = abs(GLDAS2_1 - station_summary[[1,4]]) 
    )
)

## Get normal years from GLDAS ---- 
(
  normal_years <- 
    precip_annual %>%
    filter(Source == 'GLDAS2_1', Condition == 'Normal') %>% 
    spread(key = Source, value = Precip) %>% 
    drop_na(GLDAS2_1) %>%
    # Get difference between "Normal" GLDAS years and mean 
    # precipitation of "Normal" years from meteorological stations
    mutate(Mean_Dif = abs(GLDAS2_1 - station_summary[[2,3]]))
)

## ----- Get wet years from GLDAS ----
(
  wet_years <- precip_annual %>%
    filter(Source == 'GLDAS2_1', Condition == 'Wet') %>% 
    spread(key = Source, value = Precip) %>%
    drop_na(GLDAS2_1) %>%
    # Get difference between "Wet" GLDAS years and minimum/mean/maximum 
    # precipitation of "Wet" years from meteorological stations
    mutate(
      Min_Dif = abs(GLDAS2_1 - station_summary[[3,2]]),
      Mean_Dif = abs(GLDAS2_1 - station_summary[[3,3]]),
      Max_Dif = abs(GLDAS2_1 - station_summary[[3,4]])
    )
)

# ANALYSE STATION AND GLDAS YEARS ---------------------------------------------

## Add condition information on monthly dataset ----
(
  precip_monthly <- 
    precip_monthly %>%
    gather(key = Source, value = Precip, -Year, -Month) %>%
    group_by(Source, Year) %>%
    nest() %>%
    right_join(
      precip_annual %>% select(-Precip), 
      by = c('Source', 'Year')
    ) %>% # Join with information from annual dataset
    unnest(cols = data)
)

# This section had some manual selections of years, which are specified in the 
# next chunk. The selection was based in the proximity between meteorological
# stations quantiles parameters and GLDAS precipitation, and the visual 
# inspection of the intra-annual time series.

# Selected years were chosen based on the proximity to Min, Mean and Max 
# values and their agreement with seasonal variation.

## Create table with GLDAS classified years ----
(
  year_class <- 
    tibble(
      Class = factor(
        c('D1', 'D2', 'D3', 'N', 'W1', 'W2', 'W3'), 
        levels = c('D1', 'D2', 'D3', 'N', 'W1', 'W2', 'W3')
      ),
      Year = c(2012, 2015, 2001, 2017, 2000, 2005, 2006)
    ) %>% 
    left_join(
      precip_annual %>% 
        filter(Source == 'GLDAS2_1') %>% 
        select(Year, Precip), 
      by = 'Year'
    ) %>% # Merge with precip information
    arrange(Class)
)

## Plot intra-annual variation of precip ----
precip_monthly %>%
  filter(!str_detect(Source, 'GLDAS')) %>%  # Remove GLDAS data
  mutate(Month = fct_relevel(
    Month, 
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
    )) %>% # Organize sequence of months
  drop_na() %>%
  ggplot() +
  facet_wrap(~Condition, ncol = 1) +
  geom_boxplot(aes(x = Month, y = Precip)) +
  geom_line(
    data = precip_monthly %>% filter(Source == 'GLDAS2_1') %>% drop_na(),
    aes(x = Month, y = Precip, group = Year), 
    alpha = 0.4
    ) +
  geom_line(
    data = precip_monthly %>% 
      filter(Source == 'GLDAS2_1') %>% 
      left_join(year_class %>% select(-Precip), by = 'Year') %>% 
      drop_na(),
    mapping = aes(x = Month, y = Precip, color = Class, group = Class)
    ) +
  scale_color_manual(values = c(
    '#642D33','#A84C56','#CA9399', '#4c4c4c', 
    '#0D2F70', '#1650BB', '#7395D6'
    )) +
  ylab('Precipitation (mm)') +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.title = element_blank(), 
    axis.title.x = element_blank()
    ) +
  ggsave('./plots/Precip_Boxplot.pdf', width = 15, height = 11, units = 'cm')

## Plot histogram of stations precip with GLDAS years ----
precip_annual %>%
  filter(!str_detect(Source, 'GLDAS')) %>%
  drop_na() %>%
  left_join(year_class, by = 'Year', suffix = c('_01', '_02')) %>% 
  ggplot() +
  stat_bin(aes(x = Precip_01), bins = 7, fill = '#9e9e9e') +
  geom_vline(
    aes(xintercept = Precip_02, color = Class), 
    linetype = 'dashed', 
    na.rm = TRUE
    ) +
  geom_text(
    aes(x = Precip_02, y = 22, label = Class, color = Class), 
    hjust = 1.2, 
    vjust = -0.2, 
    family = 'sans', 
    size = 2.8, 
    na.rm = TRUE
    ) +
  scale_color_manual(values = c(
    '#642D33','#A84C56','#CA9399', '#4c4c4c', 
    '#0D2F70', '#1650BB', '#7395D6'
    )) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  xlab('Precipitation (mm)') +
  theme_bw() +
  theme(
    text = element_text(family = 'sans', size = 11), 
    legend.position = 'none', 
    axis.title.y = element_blank()
    ) +
  ggsave('./plots/Precip_Hist.pdf', width = 15, height = 10, units = 'cm')

## Extract precipitation of meteorological stations ----
## Extract precipitation of meteorological stations for each condition 
## and metrics (calculated after quantiles separation)
(
  meteo_classes <- 
    precip_annual %>%
  filter(!str_detect(Source, 'GLDAS')) %>% # Remove GLDAS data
  drop_na() %>%
  group_by(Condition) %>%
  summarise(Mean = mean(Precip), Min = min(Precip), Max = max(Precip)) %>%
  gather(key = Metric, value = Precip, -Condition) %>% 
  mutate(
    Precip = if_else( # Remove min/max from "Normal" years quantile range
      condition = (Condition == 'Normal' & Metric != 'Mean'), 
      true = NA_real_, 
      false = Precip
    )
  ) %>% 
  drop_na() %>%
  mutate(
    Class = factor(
      c('D2', 'N', 'W2', 'D1', 'W3', 'D3', 'W1'), 
      levels = c('D1', 'D2', 'D3', 'N', 'W1', 'W2', 'W3')
    )
  ) %>%
  arrange(Class)
)

## Create table with years information ----
dry_years %>%
  bind_rows(normal_years, wet_years) %>%
  filter(Year %in% c(2012, 2015, 2001, 2017, 2000, 2005, 2006)) %>%
  mutate(
    Quantile = ecdf( # Estimate cumulative distribution function
      precip_annual %>%
        filter(Source %in% c("Petrolina", "Juazeiro")) %>%
        drop_na() %>%
        pull(Precip)
    )( # Estimate the quantiles for each selected year
      dry_years %>%
        bind_rows(normal_years, wet_years) %>%
        filter(Year %in% c(2012, 2015, 2001, 2017, 2000, 2005, 2006)) %>%
        pull(GLDAS2_1)
    )
  ) %>% 
  mutate(
    Class = factor( # Change order of classes
      c("D3", "D1", "D2", "N", "W1", "W2", "W3"),
      levels = c("D1", "D2", "D3", "N", "W1", "W2", "W3")
    )
  ) %>% 
  arrange(Class) %>%
  mutate(
    Stations = meteo_classes %>% pull(Precip),
    Metric = meteo_classes %>% pull(Metric)
  ) %>% # Get the stations precipitation (calculated from quantiles)
  select(Class, Year, GLDAS2_1, Stations, Metric, Quantile) %>%
  rename(GLDAS = GLDAS2_1) %>%
  arrange(Quantile) %>%
  kable("markdown")

# CREATE SCENARIOS REPRESANTATION ---------------------------------------------

# The scenarios were build manually with the information created above, 
# now we are going to create a plot to represent the set of 49 scenarios

## Organize scenarios information ----
scenarios <- 
  read_delim('./data/scenarios_years.txt', delim = ',', col_names = FALSE) %>% 
  mutate(Scen = dir('./data/scenarios/') %>% str_remove('.nc')) %>% 
  gather(key = Time, value = Year, - Scen) %>% 
  group_by(Time) %>%
  nest() %>%
  ungroup(cols = data) %>%
  mutate(Time = str_remove(Time, 'X')) %>%
  unnest(cols = data) %>%
  left_join(year_class %>% select(-Precip), by = 'Year') %>%
  mutate(
    Scen = Scen %>% 
      str_replace_all(c('_' = '[', 's' = 'S')) %>% 
      str_c(']') %>% 
      fct_rev(),
    Time = as.numeric(Time)
  )

## Plot scenarios scheme ----
# Create axis label information
lab_level <- levels(scenarios$Scen)[seq(1, length(scenarios$Scen), by = 4)]
# Plot scheme
scenarios %>%
  ggplot() +
  geom_raster(aes(x = Time, y = Scen, fill = Class)) +
  geom_vline(xintercept = 5.5, linetype = 'dashed') +
  geom_hline(yintercept = c(16.5, 32.5, 48.5), linetype = 'dashed') +
  scale_y_discrete(breaks = lab_level, labels = parse(text = lab_level)) +
  scale_x_continuous(breaks = seq(1, 13), expand = c(0, 0)) +
  scale_fill_manual(
    values = c(
      '#642D33','#A84C56','#CA9399', '#4c4c4c', '#0D2F70', '#1650BB', '#7395D6'
    )
  ) +
  labs(x = 'Year') +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.title = element_blank(), 
    axis.title.y = element_blank()
    ) +
  ggsave('./plots/Scenarios.pdf', width = 15, height = 10, units = 'cm')
