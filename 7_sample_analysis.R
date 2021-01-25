## ------------------------------------------------------------------------- ##
####* ------------------------------- HEADER ---------------------------- *####
## ------------------------------------------------------------------------- ##
##
#### Description ####
##
## Script name:   Sampling analysis
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
## Last update:   2020-06-17
## Last tested:   2020-06-17
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
#### Options ####
##
options(scipen = 6, digits = 4) # View outputs in non-scientific notation
##
## ------------------------------------------------------------------------- ##
##
#### Libraries ####
##
library(rlang)
library(rstatix)
library(kableExtra)
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

#### ---------------------------------------------------- Perform sampling ####

####' ----- Perform stratified sampling ####  

set.seed(2020)

## Get 400 observations for each vegetation type (10000 iterations)
sample <- bind_rows(
  replicate(
    10000, # Number of replications
    res_metrics %>% group_by(Vegetation) %>% slice_sample(n = 400), 
    simplify = FALSE
    ), 
  .id = "Obs"
  )
## Nest data
sample <- sample %>%
  group_by(Obs, Vegetation) %>%
  group_nest()

rm(res_metrics)
gc()

#### ----------------------------------------------- Create some functions ####

####' ----- Functions to fit and apply linear regression #### 
fit_lm_on_sample <- function(sample_data) {
  lm(formula, sample_data)
}

apply_lm_on_sample <- function() {
  
  sample_regression <- sample %>%
    mutate(model = map(data, fit_lm_on_sample), coef_info = map(model, tidy)) 
  
  return(sample_regression)
  
}

augment_lm <- function() {
  
  ## Augment model data (so we get the fitted values, residuals, etc...)
  augmented <- sample_regression %>%
    mutate(augmented = map(model, augment)) %>% 
    unnest(cols = augmented) %>%
    mutate(.fitted = if_else(.fitted < 0, 0, .fitted))
  
  return(augmented)
  
}

####' ----- Function to calculate confidence intervals #### 
reg_ic <- function(dependent, independent) {
  
  range <- augmented %>%
    group_by(Vegetation) %>% # 
    summarise(max = max(!!sym(independent)), min = min(!!sym(independent))) %>%
    group_nest(Vegetation) %>%
    # Create a sequence of 100 numbers that ranges from the min to the max
    # of the independent variable
    mutate(!!independent := map(data, ~seq(
      from = pull(.x %>% select(min)),
      to = pull(.x %>% select(max)),
      length.out = 100
      ))) %>% 
    select(-data) %>% 
    unnest(cols = !!independent) %>% 
    group_nest(Vegetation) 
  
  ## Apply models of each sampling iteration to the range of 
  ## values created above
  confidence_interval <- sample_regression %>%
    select(Vegetation, model) %>% 
    group_nest(Vegetation) %>% 
    right_join(range, by = 'Vegetation') %>% 
    unnest(cols = data.x) %>% 
    # Predict the dependent variable for the range of values of independent var
    mutate(Fitted = map2(model, data.y, predict)) %>% 
    unnest(cols = c(data.y, Fitted)) %>% 
    select(-model)
  
  ## Estimate confidence intervals for the regressions
  ci <- confidence_interval %>%
    group_by(Vegetation, !!sym(independent)) %>% 
    summarise(
      low = quantile(Fitted, 0.025),
      high = quantile(Fitted, 0.975)
      ) %>%
    mutate_at(vars(low, high), list(~if_else(. < 0, 0, .)))
  
  return(ci)
  
}

####' ----- Functions to create regression plots #### 
## Plots regression for Impact x Recovery
reg_plot_1 <- function(lab_x, lab_y) {
  
  p <- augmented %>%
    ggplot(aes(x = Impact)) +
    ## Linear regression using all the sampled data from each vegetation
    stat_smooth(
      aes(y = .fitted, color = Vegetation, group = Vegetation), 
      size = 0.5, 
      formula = y ~ x,
      method = 'lm'
    ) + 
    ## Estimated confidence interval of each vegetation type
    geom_ribbon(
      data = ci, 
      aes(
        x = Impact, 
        ymin = low, 
        ymax = high, 
        group = Vegetation, 
        fill = Vegetation
      ), 
      alpha = 0.3, 
      inherit.aes = FALSE
    ) + 
    scale_color_manual(values = c('#948307', '#4d2d04', '#079467', '#7b9407')) +
    scale_fill_manual(values = c('#948307', '#4d2d04', '#079467', '#7b9407')) +
    theme_bw() +
    theme(
      text = element_text(family = "sans", size = 11), 
      legend.position = ''
    ) +
    labs(x = lab_x, y = lab_y) +
    xlim(0, 1)
  
  return(p)
  
}

## Plots regressions for precipitation x vegetation components
reg_plot_2 <- function(independent, lab_x, lab_y, lim) {
  
  p <- augmented %>%
    ggplot(aes(x = !!sym(independent))) +
    ## Linear regression using all the sampled data from each vegetation
    stat_smooth(
      aes(y = .fitted, color = Vegetation, group = Vegetation), 
      size = 0.5, 
      method = 'lm', 
      formula = y ~ x, 
      se = FALSE
      ) + 
    ## Estimated confidence interval of each vegetation type
    geom_ribbon(
      data = ci, 
      aes(
        x = !!sym(independent), 
        ymin = low, 
        ymax = high, 
        group = Vegetation, 
        fill = Vegetation
        ), 
      alpha = 0.3, 
      inherit.aes = FALSE
      ) +
    geom_abline(
      intercept = 0, 
      slope = 1, 
      linetype = 'dashed', 
      size = 0.7, 
      color = 'black', 
      alpha = 0.8
      ) +
    scale_color_manual(values = c('#948307', '#4d2d04', '#079467', '#7b9407')) +
    scale_fill_manual(values = c('#948307', '#4d2d04', '#079467', '#7b9407')) +
    theme_bw() +
    theme(
      text = element_text(family = "sans", size = 11), 
      legend.position = ''
      ) +
    labs(x = lab_x, y = lab_y) +
    xlim(0, lim) + ylim(0, lim)
  
  return(p)
  
}

## Plots residuals distribution
res_dist_plot <- function() {
  
  sp1 <- augmented %>%
    group_by(Vegetation) %>%
    slice_sample(n = 3000) %>%
    ggplot(aes(x = .resid)) +
    geom_histogram(fill = 'gray', bins = 30) +
    theme_dark(10) +
    theme(
      text = element_text(family = "sans", size = 6), 
      axis.text.y  = element_blank(), 
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      plot.background = element_rect(fill = "transparent", color = NA), 
      aspect.ratio = 0.9
    ) +
    labs(x = 'Residuals', y = 'Count')
  
  return(sp1)
  
}

## Create residual x fitted scatter plot
res_fit_plot <- function() {
  
  sp2 <- augmented %>%
    group_by(Vegetation) %>%
    slice_sample(n = 3000) %>%
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_hex() +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_fill_gradient(low = "gray", high = "black") +
    theme_dark(10) +
    theme(
      text = element_text(family = "sans", size = 6), 
      legend.position = '', 
      axis.text.y  = element_blank(), 
      plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
      plot.background = element_rect(fill = "transparent", color = NA), 
      aspect.ratio = 0.9
    ) +
    labs(x = 'Fitted', y = 'Residuals')
  
  return(sp2)
  
} 

####' ----- Function to test difference between lm factors #### 
## Test differences between slopes
lm_diff_test <- function() {
  
  factors_diff <- sample_regression %>%
    unnest(cols = coef_info) %>%
    select(Obs, Vegetation, term, estimate) %>%
    spread(key = Vegetation, value = estimate) %>%
    mutate(
      'OS - WS' = `Open Shrublands` - `Woody Savannas`,
      'OS - S' = `Open Shrublands` - Savannas,
      'OS - G' = `Open Shrublands` - Grasslands,
      'WS - S' = `Woody Savannas` - Savannas,
      'WS - G' = `Woody Savannas` - Grasslands,
      'S - G' = Savannas - Grasslands
      ) %>%
    select('Obs', term, 'OS - WS':'S - G') %>%
    gather(key = Diff, value = Value, 'OS - WS':'S - G') %>%
    group_by(term, Diff) %>%
    filter(Value < quantile(Value, 0.975), Value > quantile(Value, 0.025)) %>%
    group_nest() %>%
    mutate(condition = map(
      data, 
      ~.x %>% 
        summarise(sign = max(Value) * min(Value)) %>% 
        mutate(Test = case_when(sign <= 0 ~ FALSE, sign >= 0 ~ TRUE))
      )) %>%
    unnest(cols = condition) %>%
    select(-sign) %>%
    unnest(cols = data)
  
  return(factors_diff)
}

####' ----- Function to test difference between medians #### 
median_diff_test <- function() {

  ## Test the differences for each components of resilience
  median_diff <- sample_components %>% 
    group_by(Obs, Vegetation) %>%
    summarise_at(vars(Impact:Stability), median) %>%
    gather(key = Metric, value = Value, Impact:Stability) %>%
    group_by(Obs, Vegetation) %>%
    mutate(id = row_number()) %>%
    spread(key = Vegetation, value = Value) %>%
    mutate(
      'OS - WS' = `Open Shrublands` - `Woody Savannas`,
      'OS - S' = `Open Shrublands` - Savannas,
      'OS - G' = `Open Shrublands` - Grasslands,
      'WS - S' = `Woody Savannas` - Savannas,
      'WS - G' = `Woody Savannas` - Grasslands,
      'S - G' = Savannas - Grasslands
    ) %>%
    select('Obs':id, 'OS - WS':'S - G') %>%
    gather(key = Diff, value = Value, 'OS - WS':'S - G') %>%
    group_by(Metric, Diff) %>%
    filter(Value < quantile(Value, 0.975), Value > quantile(Value, 0.025)) %>%
    group_nest() %>%
    mutate(condition = map(
      data, 
      ~.x %>% 
        summarise(sign = max(Value) * min(Value)) %>% 
        mutate(Test = case_when(sign <= 0 ~ FALSE, sign >= 0 ~ TRUE))
    )) %>%
    unnest(cols = condition) %>%
    select(-sign) %>%
    unnest(cols = data)
  
  return(median_diff)
  
}

#### ----------------------- Fit models, test differences and create plots #### 

####' ----- Impact x Recovery_Base #### 
## Set formula
formula <- 'Recovery_Base ~ Impact' 
## Apply linear model
sample_regression <- apply_lm_on_sample()
## Augment model data (so we get the fitted values, residuals, etc...)
augmented <- augment_lm() 
## Calculate confidence interval
ci <- reg_ic(dependent = 'Recovery_Base', independent = 'Impact')
## Create plot of regressions and its confidence intervals
p1 <- reg_plot_1(
  lab_x = parse_expr('VI~(Vegetation~Impact)'), 
  lab_y = parse_expr('VR[b]~(Vegetetion~Recovery[b])')
  )
## Create plot of residuals distribution
sp1 <- res_dist_plot()
## Create scatter plot of residuals against fitted values 
sp2 <- res_fit_plot()
## Create panel of subplots
spr1 <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')
## Get legend
legend <- get_legend(p1 + theme(
  legend.position = 'bottom', 
  legend.title = element_blank()
  ))

####' ----- Impact x Recovery_Impact  #### 
## Set formula
formula <- 'Recovery_Impact ~ Impact' 
## Apply linear regression
sample_regression <- apply_lm_on_sample()
## Augment model data (so we get the fitted values, residuals, etc...)
augmented <- augment_lm()
## Calculate confidence interval
ci <- reg_ic(dependent = 'Recovery_Impact', independent = 'Impact')
## Create plot of regressions and its confidence intervals
p2 <- reg_plot_1(
  lab_x = parse_expr('VI~(Vegetation~Impact)'), 
  lab_y = parse_expr('VR[i]~(Vegetetion~Recovery[i])')
  )
## Create plot of residuals distribution
sp1 <- res_dist_plot() 
## Create scatter plot of residuals against fitted values
sp2 <- res_fit_plot()
## Create panel of subplots
spr2 <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')

####' ----- Create first set of plots (Impact x Recovery) #### 
## Merge the plots created above
prow <- plot_grid(
  p1, p2, 
  labels = c('a', 'b'), 
  axis = 'lrbt',
  align = 'hv',
  label_size = 11, 
  label_x = 0.14,
  label_y = 1.03
  )
## Add legend below the plot
fp <- plot_grid(
  ggplot() + theme_nothing(), prow, legend, 
  ncol = 1, 
  rel_heights = c(0.025, 1, 0.1)
  )
## Add subplots
fp <- ggdraw() + 
  draw_plot(fp) + 
  draw_plot(spr1, x = 0.093, y = 0.32, width = 0.25) +
  draw_plot(spr2, x = 0.727, y = 0.32, width = 0.25)
## Save plot
ggsave2('./plots/reg_1.pdf', fp, height = 8, width = 15, units = 'cm')

## Clear objects
rm(
  ci, fp, legend, p1, p2, prow, 
  sample_regression, sp1, sp2, 
  spr1, spr2, augmented)
gc()

####' ----- Precipitation Decrease x Impact ####
## Set formula
formula <- 'Impact ~ Precip_Impact' 
## Apply linear regression
sample_regression <- apply_lm_on_sample()
## Augment model data (so we get the fitted values, residuals, etc...)
augmented <- augment_lm()
## Calculate confidence interval
ci <- reg_ic(dependent = 'Impact', independent = 'Precip_Impact')
## Create plot of regressions and its confidence intervals
p1 <- reg_plot_2(
  independent = 'Precip_Impact',
  lab_x = parse_expr('PI~(Precipitation~Impact)'), 
  lab_y = parse_expr('VI~(Vegetation~Impact)'),
  lim = 1
  )
## Create plot of residuals distribution
sp1 <- res_dist_plot() 
## Create scatter plot of residuals against fitted values
sp2 <- res_fit_plot()
## Create panel of subplots
spr1 <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')

## Test differences between lm factors
fd1 <- lm_diff_test() %>%
  mutate(Metric = 'Impact')

####' ----- Precipitation Increase Base x Recovery Base #### 
## Set formula
formula <- 'Recovery_Base ~ Precip_Recovery_Base' 
## Apply linear model
sample_regression <- sample %>%
  mutate(model = map(data, fit_lm_on_sample), coef_info = map(model, tidy))
## Augment model data (so we get the fitted values, residuals, etc...)
augmented <- augment_lm()
## Calculate confidence interval
ci <- reg_ic(dependent = 'Recovery_Base', independent = 'Precip_Recovery_Base')
## Create plot of regressions and its confidence intervals
p2 <- reg_plot_2(
  independent = 'Precip_Recovery_Base',
  lab_x = parse_expr('PR[b]~(Precipitation~Recovery[b])'),
  lab_y = parse_expr('VR[b]~(Vegetation~Recovery[b])'),
  lim = 2.5
  )
## Create plot of residuals distribution
sp1 <- res_dist_plot() 
## Create scatter plot of residuals against fitted values
sp2 <- res_fit_plot()
## Create panel of subplots
spr2 <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')

## Test differences between lm factors
fd2 <- lm_diff_test() %>%
  mutate(Metric = 'Recovery[base]')

####' ----- Precipitation Increase Impact x Recovery Impact ####
## Set formula
formula <- 'Recovery_Impact ~ Precip_Recovery_Impact' 
## Apply linear model
sample_regression <- apply_lm_on_sample()
## Augment model data (so we get the fitted values, residuals, etc...)
augmented <- augment_lm()
## Calculate confidence interval
ci <- reg_ic(
  dependent = 'Recovery_Impact', 
  independent = 'Precip_Recovery_Impact'
)
## Create plot of regressions and its confidence intervals
p3 <- reg_plot_2(
  independent = 'Precip_Recovery_Impact',
  lab_x = parse_expr('PR[i]~(Precipitation~Recovery[i])'),
  lab_y = parse_expr('VR[i]~(Vegetation~Recovery[i])'),
  lim = 12.5
  )
## Create plot of residuals distribution
sp1 <- res_dist_plot() 
## Create scatter plot of residuals against fitted values
sp2 <- res_fit_plot()
## Create panel of subplots
spr3 <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')

## Test differences between lm factors
fd3 <- lm_diff_test() %>%
  mutate(Metric = 'Recovery[impact]')

####' ----- Precipitation Stability x Stability ####
## Set formula
formula <- 'Stability ~ Precip_Stability' 
## Apply linear model
sample_regression <- apply_lm_on_sample()
## Augment model data (so we get the fitted values, residuals, etc...)
augmented <- augment_lm()
## Calculate confidence interval
ci <- reg_ic(dependent = 'Stability', independent = 'Precip_Stability')
## Create plot of regressions and its confidence intervals
p4 <- reg_plot_2(
  independent = 'Precip_Stability',
  lab_x = parse_expr('PS~(Precipitation~Stability)'), 
  lab_y = parse_expr('VS~(Vegetation~Stability)'),
  lim = 1
  )
## Create plot of residuals distribution
sp1 <- res_dist_plot() 
## Create scatter plot of residuals against fitted values
sp2 <- res_fit_plot()
## Create panel of subplots
spr4 <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')
## Get legend
legend <- get_legend(p4 + theme(legend.position = 'bottom'))

## Test differences between lm factors
fd4 <- lm_diff_test() %>%
  mutate(Metric = 'Stability') 

####' ----- Create second set of plots (Precip x Vegetation) ####
## Create panel of plots
prow <- plot_grid(
  p2, p3, p1, p4,
  labels = c('a', 'b', 'c', 'd'), 
  axis = 'bt',
  align = 'v',
  label_size = 11,
  label_x = 0.16,
  label_y = 1.035
  ) 
## Insert legend in the bottom of the plot
fp <- plot_grid(
  ggplot() + theme_nothing(), prow, legend, 
  ncol = 1,
  rel_heights = c(0.01, 1, 0.1)
  )
## Add subplots
fp <- ggdraw() + 
  draw_plot(fp) + 
  draw_plot(spr1, x = 0.23, y = -0.26, width = 0.25) +
  draw_plot(spr2, x = 0.23, y = 0.19, width = 0.25) +
  draw_plot(spr3, x = 0.605, y = 0.41, width = 0.25) +
  draw_plot(spr4, x = 0.73, y = -0.26, width = 0.25)
## Save plot
ggsave2('./plots/reg_2.pdf', fp, height = 15, width = 15, units = 'cm')

## Clear objects
rm(
  sample_regression, augmented, ci, formula,
  fp, p1, p2, p3, p4, legend, prow,  
  spr1, spr2, spr3, spr4, sp1, sp2
)
gc()

####' ----- Create third set of plots (lm factors diff test) #### 
## Create dummy data to make tag labels
dummy <- tibble(
  Metric = c(
    'Recovery[b]', 'Recovery[i]', 'Impact', 'Stability',
    'Recovery[b]', 'Recovery[i]', 'Impact', 'Stability'
    ),
  Term = c(
    'Intercept', 'Intercept', 'Intercept', 'Intercept',
    'Slope', 'Slope', 'Slope', 'Slope'
    ),
  Label = c(
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'
    )
  ) %>%
  mutate(
    Metric = factor(
      Metric, 
      levels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
    ))
## Create plot
fd1 %>% 
  bind_rows(fd2, fd3, fd4) %>%
  mutate(
    Metric = factor(
      Metric, 
      levels = c('Recovery[base]', 'Recovery[impact]', 'Impact', 'Stability'),
      labels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
    ),
    Term = case_when(term == '(Intercept)' ~ 'Intercept', TRUE ~ 'Slope')
  ) %>%
  ggplot() +
  facet_grid(Metric ~ Term, labeller = label_parsed) +
  geom_line(aes(x = Value, y = Diff, color = Test, size = Test)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_text(
    data = dummy, 
    aes(x = -0.36, y = 6.1, label = Label), 
    fontface = 'bold', 
    size = 4
    ) +
  scale_color_manual(values = c('gray', 'black')) +
  scale_size_manual(values = c(0.8, 1)) +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = '', 
    axis.title.y = element_blank()
  ) +
  labs(x = 'Slope Difference') +
  ggsave('./plots/factors_diff.pdf', width = 15, height = 15, units = 'cm')

####' ----- Calculate ratio between precip and veg components ####
sample_components <- sample %>%
  unnest(cols = data) %>%
  mutate(Impact = Impact/Precip_Impact,
         Recovery_Base = Recovery_Base/Precip_Recovery_Base,
         Recovery_Impact = Recovery_Impact/Precip_Recovery_Impact,
         Stability = Stability/Precip_Stability) %>%
  select(Obs:Recovery_Impact, Stability, -Recovery)

####' ----- Create fourth set of plots (median diff test) ####
## Medians bar plot
sample_components %>%
  group_by(Vegetation, Obs) %>%
  summarise_at(vars(Impact:Stability), median) %>%
  gather(key = Metric, value = Value, Impact:Stability) %>%
  group_by(Vegetation, Metric) %>%
  summarise(Median = median(Value),
            Low = quantile(Value, 0.025),
            High = quantile(Value, 0.975)) %>%
  mutate(Metric = factor(
    Metric, 
    levels = c('Recovery_Base', 'Recovery_Impact', 'Impact', 'Stability'), 
    labels = c('Recovery[base]', 'Recovery[impact]', 'Impact', 'Stability')
  )) %>%
  ggplot() +
  facet_wrap(~Metric, labeller = label_parsed) +
  geom_col(aes(x = Vegetation, y = Median, fill = Vegetation)) +
  geom_errorbar(aes(x = Vegetation, ymin = Low, ymax = High), width = 0.8) +
  scale_fill_manual(values = c('#948307', '#4d2d04', '#079467', '#7b9407')) +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.ticks.x = element_blank()
    ) +
  ggsave('./plots/median_cols.pdf', width = 15, height = 15, units = 'cm')
## Test difference between medians
median_diff <- median_diff_test()
## Create dummy data to make tag labels
dummy <- tibble(
  Metric = c(
    'Recovery[b]', 'Recovery[i]', 'Impact', 'Stability'
  ),
  Label = c(
    'a', 'b', 'c', 'd'
  )
  ) %>%
  mutate(Metric = factor(
    Metric,
    levels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
  ))
## Plot medians difference tests
median_diff %>%
  mutate(Metric = factor(
    Metric, 
    levels = c('Recovery_Base', 'Recovery_Impact', 'Impact', 'Stability'), 
    labels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
  )) %>%
  ggplot() +
  facet_wrap(~Metric, labeller = label_parsed) +
  geom_line(aes(x = Value, y = Diff, color = Test, size = Test)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_text(
    data = dummy, 
    aes(x = -0.15, y = 6.3, label = Label), 
    fontface = 'bold', 
    size = 4
  ) +
  scale_color_manual(values = c('gray', 'black')) +
  scale_size_manual(values = c(0.8, 1)) +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = '', 
    axis.title.y = element_blank()
  ) +
  labs(x = 'Median Difference') +
  ggsave('./plots/median_diff.pdf', width = 15, height = 15, units = 'cm')

## Clear objects
rm(fd1, fd2, fd3, fd4, median_diff, dummy)
gc()

## ------------------------------------------------------------------------- ##
####* ------------------------------- END ------------------------------- *####
## ------------------------------------------------------------------------- ##