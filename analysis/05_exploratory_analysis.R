# HEADER ----------------------------------------------------------------------
#
# Title:        Exploratory analysis
# Description:  
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
library(arrow)
library(rlang)
library(rstatix)
library(cowplot)
library(hexbin)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#
options(scipen = 6, digits = 4) # View outputs in non-scientific notation
#
# LOAD AND FILTER DATA --------------------------------------------------------

## Load data ----
res_metrics <- 
  read_parquet('./data/resilience_metrics.parquet')

## Filter data ----
res_metrics <- res_metrics %>%
  filter(
    impact > 0.10, # Low values of impact have a big effect on some results
    veg_type >= 7, veg_type <= 10, # Most common veg types in the area
    soil_type == 7, # Will analyze only one type of soil (predominant)
    drought_duration <= 2 # Only droughts of two or less years
  )

## Reorder and rename factors ----
res_metrics <- res_metrics %>%
  mutate(
    recovery_intensity = fct_relevel(
      recovery_intensity, 'W1', 'W2', 'W3', 'N'
    ),
    veg_type = factor(
      veg_type,
      labels = c('Open Shrublands', 'Woody Savannas', 'Savannas', 'Grasslands')
    )
  )

## Convert to wide format ----
res_metrics_wide <- res_metrics %>%
  pivot_wider(
    id_cols = c(id, scenario, veg_type),
    values_from = stability:recovery_impact,
    names_from = variable,
    names_glue = "{variable}_{.value}"
  )

# EXPLORATORY ANALISIS --------------------------------------------------------

## Exploratory metrics ----
res_metrics_wide %>%
  get_summary_stats(
    gpp_stability:precip_recovery_impact,
    show = c('min', 'mean', 'median', 'max', 'sd')
  ) %>%
  arrange(variable) %>%
  gather(key = metric, value = value, n:sd) %>%
  mutate(
    metric = factor(
      metric, 
      levels = c('n', 'min', 'mean', 'median', 'max', 'sd')
    )
  ) %>%
  spread(key = variable, value = value)

## Scatter plot (recovery x impact) ----

### Baseline based recovery ----
p1 <- res_metrics_wide %>%
  drop_na(gpp_impact, gpp_recovery_base) %>%
  ggplot(aes(x = gpp_impact, y = gpp_recovery_base)) +
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

### Impact based recovery ----
p2 <- res_metrics_wide %>%
  drop_na(gpp_impact, gpp_recovery_impact) %>%
  ggplot(aes(x = gpp_impact, y = gpp_recovery_impact)) +
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

### Merge plots in row ----
prow <- 
  plot_grid(
    p1, p2, 
    labels = c('a', 'b'), 
    axis = 'btlr',
    align = 'hv',
    label_x = 0.16,
    label_y = 1.04,
    label_size = 11
  )

### Final plot ----
fp <- 
  plot_grid(
    ggplot() + theme_nothing(), prow,
    ncol = 1,
    rel_heights = c(0.03, 1)
  )
  
### Save plot ----
ggsave2('./plots/scatter_1.pdf', fp, height = 7.5, width = 15, units = 'cm')

## Scatter plot (precipitation x GPP components) ----

### Baseline based recovery ----
p1 <- res_metrics_wide %>%
  drop_na(precip_recovery_base, gpp_recovery_base) %>%
  ggplot(aes(x = precip_recovery_base, y = gpp_recovery_base)) +
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
  xlim(-0.01, 3.5) + ylim(-0.01, 3.5)

### Impact based recovery ----
p2 <- res_metrics_wide %>%
  drop_na(precip_recovery_impact, gpp_recovery_impact) %>%
  ggplot(aes(x = precip_recovery_impact, y = gpp_recovery_impact)) +
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
  xlim(-0.1, 13) + ylim(-0.1, 13)
  
### Impact ----
p3 <- res_metrics_wide %>%
  drop_na(precip_impact, gpp_impact) %>% 
  ggplot(aes(x = precip_impact, y = gpp_impact)) +
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

### Stability ----
p4 <- res_metrics_wide %>%
  drop_na(precip_stability, gpp_stability) %>%
  ggplot(aes(x = precip_stability, y = gpp_stability)) +
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

### Final plot ----
fp <- plot_grid(
  ggplot() + theme_nothing(), ggplot() + theme_nothing(),
  p1, p2, p3, p4, 
  labels = c('', '', 'a', 'b', 'c', 'd'),
  rel_heights = c(0.03, 1, 1),
  ncol = 2,
  label_size = 11,
  label_x = 0.19,
  label_y = 1.04,
  align = 'v'
) 

### Save plot ----
ggsave2('./plots/scatter_2.pdf', fp, height = 15, width = 15, units = 'cm')

## Boxplots ----

### Dummy plot ----
## Create a dummy table to set the axis ranges for the plot
dummy <- 
  tibble(
    variable = c(
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
  gather(key = metric, value = value, max, min) %>%
  mutate(
    variable = factor(
      variable, 
      levels = c(
        'VI~(Vegetation~Impact)', 'VR[b]~(Vegetation~Recovery[b])',
        'VR[i]~(Vegetation~Recovery[i])', 'VS~(Vegetation~Stability)', 
        'PI~(Precipitation~Impact)', 'PR[b]~(Precipitation~Recovery[b])',
        'PR[i]~(Precipitation~Recovery[i])', 'PS~(Precipitation~Stability)'
      )
    )
  )

### Create boxplot ----
fp <- res_metrics_wide %>%
  drop_na() %>%
  pivot_longer(
    cols = gpp_stability:precip_recovery_impact,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!variable %in% c("gpp_recovery", "precip_recovery")) %>%
  mutate(
    variable = factor(
      variable, 
      labels = c(
        'VR[b]~(Vegetation~Recovery[b])', 'PR[b]~(Precipitation~Recovery[b])', 
        'VR[i]~(Vegetation~Recovery[i])', 'PR[i]~(Precipitation~Recovery[i])', 
        'VI~(Vegetation~Impact)', 'PI~(Precipitation~Impact)', 
        'VS~(Vegetation~Stability)', 'PS~(Precipitation~Stability)'
      ),
      levels = c(
        'gpp_recovery_base', 'precip_recovery_base', 
        'gpp_recovery_impact', 'precip_recovery_impact', 
        'gpp_impact', 'precip_impact', 
        'gpp_stability', 'precip_stability'
      )
    )
  ) %>%
  ggplot() +
  geom_blank(data = dummy, aes(y = value)) +
  facet_wrap(
    ~ variable, 
    scales = 'free_y', 
    labeller = label_parsed, 
    ncol = 2) +
  geom_boxplot(aes(x = veg_type, y = value, fill = veg_type)) +
  stat_summary(
    aes(x = veg_type, y = value), 
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

### Save plot ----
ggsave(
  './plots/Vegetation_Boxplot.pdf', 
  fp,
  width = 15, 
  height = 17, 
  units = 'cm'
)  

rm(dummy, fp, p1, p2, p3, p4)
