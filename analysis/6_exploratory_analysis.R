## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
## Script name:   Exploratory analysis
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
library(rlang)
library(rstatix)
library(cowplot)
library(tidyverse)
##
## ------------------------------------------------------------------------- ##
####* ------------------------------- CODE ------------------------------ *####
## ------------------------------------------------------------------------- ##

#### ----------------------------------------------------------- Load data ####

####' ----- Load data and change order of category variables #### 
res_metrics <- read_delim('./data/area_filtered.txt', delim = ',') %>%
  mutate(
    Recovery_Intensity = fct_relevel(
      Recovery_Intensity, 'W1', 'W2', 'W3', 'N'
    ),
    Vegetation = fct_relevel(
      Vegetation, 'Open Shrublands', 'Woody Savannas', 'Savannas', 'Grasslands'
    )
  )

####' ----- Filter data #### 

res_metrics <- res_metrics %>%
  filter(as.integer(Drought_Duration) <= 2, Soil == 7)

#### ------------------------------------------------ Exploratory analysis ####

####' ----- Scatter plot (recovery x impact) #### 
## Plot baseline based recovery against impact 
p1 <- res_metrics %>%
  ggplot(aes(x = Impact, y = Recovery_Base)) +
  geom_hex() +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = ''
    ) +
  labs(
    y = parse_expr('VR[b]~(Vegetation~Recovery[b])'),
    x = parse_expr('VI~(Vegetation~Impact)')
    )
## Plot impact based recovery against impact 
p2 <- res_metrics %>%
  ggplot(aes(x = Impact, y = Recovery_Impact)) +
  geom_hex() +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = ''
    ) +
  labs(
    y = parse_expr('VR[i]~(Vegetation~Recovery[i])'),
    x = parse_expr('VI~(Vegetation~Impact)')
    )
## Merge both plots
prow <- plot_grid(
  p1, p2, 
  labels = c('a', 'b'), 
  axis = 'btlr',
  align = 'hv',
  label_x = 0.16,
  label_y = 1.04,
  label_size = 11
  )
fp <- plot_grid(
  ggplot() + theme_nothing(), prow,
  ncol = 1,
  rel_heights = c(0.03, 1)
  )
  
####' ----- Save plot #### 
ggsave2('./plots/scatter_1.pdf', fp, height = 7.5, width = 15, units = 'cm')

####' ----- Scatter plot (precipitation x GPP components) #### 
## Plot precipitation recovery against GPP recovery (based on baseline)
p1 <- res_metrics %>%
  ggplot(aes(x = Precip_Recovery_Base, y = Recovery_Base)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = ''
  ) +
  labs(
    y = parse_expr('VR[b]~(Vegetation~Recovery[b])'), 
    x = parse_expr('PR[b]~(Precipitation~Recovery[b])')
  ) +
  xlim(0, 3.5) + ylim(0, 3.5)
## Plot precipitation recovery against GPP recovery (based on impact)
p2 <- res_metrics %>%
  ggplot(aes(x = Precip_Recovery_Impact, y = Recovery_Impact)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = ''
  ) +
  labs(
    y = parse_expr('VR[i]~(Vegetation~Recovery[i])'), 
    x = parse_expr('PR[i]~(Precipitation~Recovery[i])')
  ) +
  xlim(0, 14) + ylim(0, 14)
## Plot precipitation impact against GPP impact 
p3 <- res_metrics %>%
  ggplot(aes(x = Precip_Impact, y = Impact)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = ''
  ) +
  labs(
    y = parse_expr('VI~(Vegetation~Impact)'),
    x = parse_expr('PI~(Precipitation~Impact)')
    ) +
  xlim(0, 1) + ylim(0, 1)
## Plot precipitation stability against GPP stability 
p4 <- res_metrics %>%
  ggplot(aes(x = Precip_Stability, y = Stability)) +
  geom_hex() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = ''
  ) +
  labs(
    y = parse_expr('VS~(Vegetation~Stability)'),
    x = parse_expr('PS~(Precipitation~Stability)')
  ) +
  xlim(0, 1) + ylim(0, 1)
## Merge plots
fp <- plot_grid(
  ggplot() + theme_nothing(), ggplot() + theme_nothing(),
  p1, p2, p3, p4, 
  labels = c('', '', 'a', 'b', 'c', 'd'),
  rel_heights = c(0.03, 1, 1),
  ncol = 2,
  label_size = 11,
  label_x = 0.16,
  label_y = 1.04,
  align = 'v'
) 

####' ----- Save plot ####
ggsave2('./plots/scatter_2.pdf', fp, height = 15, width = 15, units = 'cm')

####' ----- Boxplots #### 
## Create a dummy table to set the axis ranges for the plot
dummy <- tibble(
  Variable = c(
    'VI~(Vegetation~Impact)', 'VR[b]~(Vegetation~Recovery[b])',
    'VR[i]~(Vegetation~Recovery[i])', 'VS~(Vegetation~Stability)', 
    'PI~(Precipitation~Impact)', 'PR[b]~(Precipitation~Recovery[b])',
    'PR[i]~(Precipitation~Recovery[i])', 'PS~(Precipitation~Stability)'
  ),
  max = c(1, 3.5, 13, 1, 1, 3.5, 13, 1), 
  min = c(0, 0, 0, 0, 0, 0, 0, 0),
  label = c('e', 'a', 'c', 'g', 'f', 'b', 'd', 'h'),
  position = c(0.9, 3.2, 11.5, 0.9, 0.9, 3.2, 11.5, 0.9)
) %>%
  gather(key = Metric, value = Value, max, min) %>%
  mutate(Variable = factor(
    Variable, 
    levels = c(
      'VI~(Vegetation~Impact)', 'VR[b]~(Vegetation~Recovery[b])',
      'VR[i]~(Vegetation~Recovery[i])', 'VS~(Vegetation~Stability)', 
      'PI~(Precipitation~Impact)', 'PR[b]~(Precipitation~Recovery[b])',
      'PR[i]~(Precipitation~Recovery[i])', 'PS~(Precipitation~Stability)'
    )
  ))
## Create the plot
fp <- res_metrics %>%
  select(Vegetation, Impact:Precip_Stability, -Recovery, -Precip_Recovery) %>%
  gather(key = Variable, value = Value, -Vegetation) %>%
  mutate(Variable = factor(
    Variable, 
    labels = c(
      'VR[b]~(Vegetation~Recovery[b])', 'PR[b]~(Precipitation~Recovery[b])', 
      'VR[i]~(Vegetation~Recovery[i])', 'PR[i]~(Precipitation~Recovery[i])', 
      'VI~(Vegetation~Impact)', 'PI~(Precipitation~Impact)', 
      'VS~(Vegetation~Stability)', 'PS~(Precipitation~Stability)'
    ),
    levels = c(
      'Recovery_Base', 'Precip_Recovery_Base', 'Recovery_Impact', 
      'Precip_Recovery_Impact', 'Impact', 'Precip_Impact', 'Stability', 
      'Precip_Stability'
    )
  )) %>%
  ggplot() +
  geom_blank(data = dummy, aes(y = Value)) +
  facet_wrap(
    ~Variable, 
    scales = 'free_y', 
    labeller = label_parsed, 
    ncol = 2) +
  geom_boxplot(aes(x = Vegetation, y = Value, fill = Vegetation)) +
  stat_summary(
    aes(x = Vegetation, y = Value), 
    fun = mean, 
    fill = "gray", 
    color = 'black', 
    geom = "point", 
    shape = 25, 
    size = 2) +
  geom_text(
    data = dummy, 
    aes(x = 0.6, y = position, label = label), 
    fontface = 'bold', 
    size = 4
    ) +
  scale_fill_manual(values = c('#948307', '#4d2d04', '#079467', '#7b9407')) +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    legend.position = 'bottom', 
    legend.title = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.title.y = element_blank()
  )

####' ----- Save plot #### 
ggsave(
  './plots/Vegetation_Boxplot.pdf', 
  fp,
  width = 15, 
  height = 17, 
  units = 'cm'
)  

####' ----- Exploratory metrics #### 
res_metrics %>%
  get_summary_stats(
    Impact:Precip_Stability,
    show = c('min', 'mean', 'median', 'max', 'sd')
  ) %>%
  arrange(variable) %>%
  gather(key = Metric, value = Value, n:sd) %>%
  mutate(Metric = factor(
    Metric, 
    levels = c('n', 'min', 'mean', 'median', 'max', 'sd')
  )) %>%
  spread(key = variable, value = Value)

rm(dummy, fp, p1, p2, p3, p4)

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##