# HEADER ----------------------------------------------------------------------
#
# Title:        Sampling analysis
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
library(glue)
library(rstatix)
library(kableExtra)
library(cowplot)
library(scales)
library(ggridges)
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
  ) %>%
  select(id, scenario, veg_type, variable, stability:recovery_impact)

## Reorder and rename factors ----
res_metrics <- res_metrics %>%
  mutate(
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

# PERFORM SAMPLING ------------------------------------------------------------

## Set sampling parameters ----

# Ensure reproducibility of results
set.seed(2021)

# Set number of samples to perform
rep_number <- 5000

# Set sample size
sample_size <- 500

## Perform sampling ----
sample <-
  map_dfr(
    .x = 1:rep_number,
    .f = 
      ~ {
        
        res_metrics_wide %>%
          group_by(veg_type) %>%
          slice_sample(n = sample_size) %>%
          mutate(sample_id = .x)
        
      }
  )

# PERFORM ANALYSIS ----

## Set the related variables ----
variables_table <-
  tibble(
    independent = c(
      "gpp_impact", "gpp_impact", 
      "precip_impact", "precip_recovery_base",
      "precip_recovery_impact", "precip_stability"
    ),
    dependent = c(
      "gpp_recovery_base", "gpp_recovery_impact",
      "gpp_impact", "gpp_recovery_base",
      "gpp_recovery_impact", "gpp_stability"
    )
  )

## Create a list of nested tibbles with paired variables ----
paired_variables <- variables_table %>%
  pmap(
    function(...) {
      
      variables <- tibble(...)
      
      # Select variables and nest rows
      variables <- sample %>%
        select(
          id, veg_type, scenario, 
          sample_id, variables$independent, variables$dependent
        ) %>%
        group_by(sample_id, veg_type) %>%
        group_nest()
      
      return(variables)
      
    }
  ) %>%
  set_names(
    glue("{variables_table$dependent} ~ {variables_table$independent}")
  )

## Fit and apply linear model ----
# Gets the linear model formula from each element name from list
reg_paired_variables <- paired_variables %>%
  imap(
    .f = ~ {
      
      # Name variable with the formula
      formula <- .y
      
      # Apply linear regression
      sample_regression <- .x %>%
        mutate(
          model = map(
            data, 
            function(sample_data) { lm(formula, sample_data) }
          ), 
          coef_info = map(model, tidy)
        ) %>%
        # Augment model data (so we get the fitted values, residuals, etc...)
        mutate(augmented = map(model, augment)) %>% 
        unnest(cols = augmented)
      
      return(sample_regression)
      
    }
  )

## Calculate confidence intervals ----
ci_paired_variables <- reg_paired_variables %>%
  imap(
    .f = ~ {
      
      # Get name of dependent variable
      dependent <- names(.x)[7]
      
      # Get name of independent variable
      independent <- names(.x)[8]
      
      range <- .x %>%
        group_by(veg_type) %>%
        summarise(
          max = max(!!sym(independent)), 
          min = min(!!sym(independent)),
          .groups = "keep"
        ) %>%
        group_nest() %>%
        # Create a sequence of 100 numbers that ranges from the min to the max
        # of the independent variable
        mutate(
          !!independent := map(
            data, 
            ~ seq(
              from = pull(.x %>% select(min)),
              to = pull(.x %>% select(max)),
              length.out = 100
            )
          )
        ) %>% 
        select(-data) %>% 
        unnest(cols = !!independent) %>% 
        group_by(veg_type) %>%
        group_nest() 
      
      # Apply models of each sampling iteration to the range of 
      # values created above
      confidence_interval <- .x %>%
        select(sample_id, veg_type, model) %>% 
        distinct(sample_id, veg_type, .keep_all = TRUE) %>%
        group_by(veg_type) %>%
        group_nest() %>% 
        right_join(range, by = 'veg_type') %>% 
        unnest(cols = data.x) %>% 
        # Predict the dependent variable for the range 
        # of values of independent var
        mutate(fitted = map2(model, data.y, predict)) %>% 
        unnest(cols = c(data.y, fitted)) %>% 
        select(-model)
      
      # Estimate confidence intervals for the regressions
      ci <- 
        confidence_interval %>%
        group_by(veg_type, !!sym(independent)) %>% 
        summarise(
          low = quantile(fitted, 0.025),
          high = quantile(fitted, 0.975),
          .groups = "keep"
        ) %>%
        mutate_at(vars(low, high), list( ~ if_else(. < 0, 0, .)))
      
      return(ci)
      
    }
  )

## Calculate mean average error ----
mae_paired_values <- reg_paired_variables %>%
  map_dfr(
    .id = "formula",
    .f = ~ {
      
      .x %>%
        group_by(veg_type) %>%
        summarise(
          mae = mean(abs(.resid), na.rm = TRUE)
        ) 
      
    }
  )

## Get the lm coefficients ----
reg_coef <-
  map2_dfr(
    .x = reg_paired_variables[3:6], 
    .y = c("Impact", "Recovery[b]", "Recovery[i]", "Stability"),
    .f = 
      ~ {
        
        .x %>%
          select(sample_id, veg_type, coef_info) %>%
          distinct(sample_id, veg_type, .keep_all = TRUE) %>%
          unnest(cols = coef_info) %>%
          select(sample_id, veg_type, term, estimate) %>%
          mutate(
            term = if_else(term == "(Intercept)", "Intercept", "Slope"),
            metric = .y
          )
        
      }
  )

## Calculate ratio between precip and gpp components ----
ratio_median <-
  map2_dfr(
    .x = reg_paired_variables[3:6], 
    .y = c("Impact", "Recovery[b]", "Recovery[i]", "Stability"),
    .f = 
      ~ {
        
        .x %>%
          select(1, 2, 7, 8) %>%
          rename(gpp = 3, precip = 4) %>%
          group_by(sample_id, veg_type) %>%
          mutate(ratio = gpp / precip) %>%
          summarise(median = median(ratio), .groups = "drop") %>%
          mutate(metric = .y)
        
      }
  )

# CREATE PLOTS ----------------------------------------------------------------

### Create residuals plots ----
residuals_plots <- 
  map(
    .x = reg_paired_variables,
    .f = ~ {
      
      sp1 <- .x %>%
        group_by(veg_type) %>%
        slice_sample(n = 3000) %>%
        ggplot(aes(x = .resid)) +
        geom_histogram(fill = 'gray', bins = 30) +
        theme_dark(10) +
        scale_x_continuous(breaks = breaks_pretty(3)) +
        theme(
          text = element_text(family = "sans", size = 5), 
          axis.text.y  = element_blank(), 
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA), 
          aspect.ratio = 0.9
        ) +
        labs(x = 'Residuals', y = 'Count')
      
      sp2 <- .x %>%
        group_by(veg_type) %>%
        slice_sample(n = 3000) %>%
        ggplot(aes(x = .fitted, y = .resid)) +
        geom_hex() +
        geom_hline(yintercept = 0, linetype = 'dashed', size = 0.3) +
        scale_x_continuous(breaks = breaks_pretty(2)) +
        scale_fill_gradient(low = "gray", high = "black") +
        theme_dark(10) +
        theme(
          text = element_text(family = "sans", size = 5), 
          legend.position = '',
          plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
          plot.background = element_rect(fill = "transparent", color = NA), 
          aspect.ratio = 0.9
        ) +
        labs(x = 'Fitted', y = 'Residuals')
      
      spr <- plot_grid(sp1, sp2, align = 'hv', axis = 'l')
      
      return(spr)
      
    }
  )

### Create first set of plots (gpp_impact x gpp_recovery) ----
regression_plots <- 
  pmap(
  .l = list(
    augmented = reg_paired_variables[1:2],
    ci = ci_paired_variables[1:2],
    lab_x = list('VI~(Vegetation~Impact)', 'VI~(Vegetation~Impact)'),
    lab_y = list(
      'VR[b]~(Vegetetion~Recovery[b])',
      'VR[i]~(Vegetetion~Recovery[i])'
    )
  ),
  function(augmented, ci, lab_x, lab_y) {
    
    p <- augmented %>%
      ggplot(aes(x = gpp_impact)) +
      # Linear regression using all the sampled data from each vegetation
      stat_smooth(
        aes(y = .fitted, color = veg_type, group = veg_type), 
        size = 0.5,
        formula = y ~ x,
        method = 'lm'
      ) +
      # Estimated confidence interval of each vegetation type
      geom_ribbon(
        data = ci, 
        aes(
          x = gpp_impact, 
          ymin = low, 
          ymax = high, 
          group = veg_type, 
          fill = veg_type
        ), 
        alpha = 0.3, 
        inherit.aes = FALSE
      ) + 
      scale_color_manual(
        values = c('#948307', '#4d2d04', '#079467', '#7b9407')
      ) +
      scale_fill_manual(
        values = c('#948307', '#4d2d04', '#079467', '#7b9407')
      ) +
      theme_bw() +
      theme(
        text = element_text(family = "sans", size = 11), 
        legend.position = ''
      ) +
      labs(x = parse_expr(lab_x), y = parse_expr(lab_y)) +
      xlim(0, 1)
    
    return(p)
    
  }
)

# Get legend
legend <- 
  get_legend(
    regression_plots[[1]] + 
      theme(legend.position = 'bottom', legend.title = element_blank())
  )

# Merge the plots created above
prow <- 
  plot_grid(
    regression_plots[[1]], regression_plots[[2]], 
    labels = c('a', 'b'), 
    axis = 'lrbt',
    align = 'hv',
    label_size = 11, 
    label_x = 0.20,
    label_y = 1.035
  )

# Add legend below the plot
fp <- 
  plot_grid(
    ggplot() + theme_nothing(), prow, legend, 
    ncol = 1, 
    rel_heights = c(0.025, 1, 0.1)
  )

# Add subplots
fp <- 
  ggdraw() + 
  draw_plot(fp) + 
  draw_plot(residuals_plots[[1]], x = 0.093, y = 0.32, width = 0.25) +
  draw_plot(residuals_plots[[2]], x = 0.727, y = 0.32, width = 0.25)

# Save plot
ggsave2('./plots/reg_1.pdf', fp, height = 8, width = 15, units = 'cm')

### Create second set of plots (gpp x precip) ----
regression_plots <- 
  pmap(
    .l = list(
      augmented = reg_paired_variables[3:6],
      ci = ci_paired_variables[3:6],
      lab_x = list(
        'PI~(Precipitation~Impact)', 
        'PR[b]~(Precipitation~Recovery[b])',
        'PR[i]~(Precipitation~Recovery[i])',
        'PS~(Precipitation~Stability)'
      ),
      lab_y = list(
        'VI~(Vegetation~Impact)',
        'VR[b]~(Vegetation~Recovery[b])',
        'VR[i]~(Vegetation~Recovery[i])',
        'VS~(Vegetation~Stability)'
      ),
      lim = list(1, 2.5, 12.5, 1)
    ),
    function(augmented, ci, lab_x, lab_y, lim) {
      
      # Get name of independent variable
      independent <- names(augmented)[8]
      
      p <- 
        augmented %>%
        ggplot(aes(x = !!sym(independent))) +
        ## Linear regression using all the sampled data from each vegetation
        stat_smooth(
          aes(y = .fitted, color = veg_type, group = veg_type), 
          size = 0.5, 
          method = 'lm', 
          formula = y ~ x, 
          se = FALSE
        ) + 
        # Estimated confidence interval of each vegetation type
        geom_ribbon(
          data = ci, 
          aes(
            x = !!sym(independent), 
            ymin = low, 
            ymax = high, 
            group = veg_type, 
            fill = veg_type
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
        scale_color_manual(
          values = c('#948307', '#4d2d04', '#079467', '#7b9407')
        ) +
        scale_fill_manual(
          values = c('#948307', '#4d2d04', '#079467', '#7b9407')
        ) +
        theme_bw() +
        theme(
          text = element_text(family = "sans", size = 11), 
          legend.position = ''
        ) +
        labs(x = parse_expr(lab_x), y = parse_expr(lab_y)) +
        xlim(0, lim) + ylim(0, lim)
      
      return(p)
      
    }
  )

# Get legend
legend <- 
  get_legend(
    regression_plots[[1]] + 
      theme(legend.position = "bottom", legend.title = element_blank())
  )

# Create panel of plots
prow <- 
  plot_grid(
    regression_plots[[2]], regression_plots[[3]], 
    regression_plots[[1]], regression_plots[[4]],
    labels = c('a', 'b', 'c', 'd'), 
    axis = 'bt',
    align = 'v',
    label_size = 11,
    label_x = 0.20,
    label_y = 1.037
  )

# Insert legend in the bottom of the plot
fp <- 
  plot_grid(
    ggplot() + theme_nothing(), prow, legend, 
    ncol = 1,
    rel_heights = c(0.01, 1, 0.1)
  )

# Add subplots
fp <- 
  ggdraw() + 
  draw_plot(fp) + 
  draw_plot(residuals_plots[[4]], x = 0.23, y = 0.19, width = 0.25) +
  draw_plot(residuals_plots[[5]], x = 0.605, y = 0.41, width = 0.25) +
  draw_plot(residuals_plots[[3]], x = 0.23, y = -0.26, width = 0.25) +
  draw_plot(residuals_plots[[6]], x = 0.73, y = -0.26, width = 0.25)

# Save plot
ggsave2('./plots/reg_2.pdf', fp, height = 15, width = 15, units = 'cm')

## Create coefficients distribution plot ----

# Create dummy tibble to name and give labels in the plot
dummy <- 
  tibble(
    metric = c(
      'Recovery[b]', 'Recovery[i]', 'Impact', 'Stability',
      'Recovery[b]', 'Recovery[i]', 'Impact', 'Stability'
    ),
    term = c(
      'Intercept', 'Intercept', 'Intercept', 'Intercept',
      'Slope', 'Slope', 'Slope', 'Slope'
    ),
    label = c(
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'
    ),
    x_position = c(rep(-0.405, 4), rep(0.55, 4))
  ) %>%
  mutate(
    metric = factor(
      metric, 
      levels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
    )
  )

# Create plot
reg_coef %>%
  mutate(
    metric = factor(
      metric, 
      levels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
    )
  ) %>%
  ggplot() +
  facet_grid(metric ~ term, labeller = label_parsed, scales = "free") +
  stat_density_ridges(
    aes(x = estimate, y = veg_type, fill = veg_type),
    alpha = 0.7,
    quantile_lines = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  geom_text(
    data = dummy, 
    aes(x = x_position, y = 5, label = label), 
    fontface = 'bold', 
    size = 4
  ) +
  scale_x_continuous(breaks = breaks_pretty(4)) +
  scale_fill_manual(
    values = c('#948307', '#4d2d04', '#079467', '#7b9407')
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = 'bottom',
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 45)
  ) +
  ggsave(
    "./plots/reg_coef_density.pdf", 
    height = 18, 
    width = 15, 
    units = 'cm'
  )

## Create ratio median distribution plot ----

# Create dummy tibble to name and give labels in the plot
dummy <- 
  tibble(
    metric = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability'),
    label = c('a', 'b', 'c', 'd'),
    x_position = c(0.95, 0.725, 1.25, 1.305)
  ) %>%
  mutate(
    metric = factor(
      metric, 
      levels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
    )
  )

# Create plot
ratio_median %>%
  mutate(
    metric = factor(
      metric, 
      levels = c('Recovery[b]', 'Recovery[i]', 'Impact', 'Stability')
    )
  ) %>%
  ggplot() +
  facet_wrap( 
    ~ metric, 
    labeller = label_parsed, 
    scales = "free_x", 
    ncol = 2
  ) +
  scale_x_continuous(breaks = breaks_pretty(3)) +
  stat_density_ridges(
    aes(x = median, y = veg_type, fill = veg_type),
    alpha = 0.7,
    quantile_lines = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  geom_text(
    data = dummy, 
    aes(x = x_position, y = 5.5, label = label), 
    fontface = 'bold', 
    size = 4
  ) +
  scale_fill_manual(
    values = c('#948307', '#4d2d04', '#079467', '#7b9407')
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "sans", size = 11), 
    legend.position = 'bottom',
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 45)
  ) +
  ggsave(
    "./plots/ratio_median_density.pdf", 
    height = 15, 
    width = 15, 
    units = 'cm'
  )
