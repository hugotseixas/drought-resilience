##### LOAD LIBRARIES #####

library(gganimate)
library(gifski)
library(cowplot)
library(png)
library(tidyverse)

library(showtext)
font_add_google('Source Sans Pro')
showtext_auto()

##### 1. LOAD DATA #####

area_filtered <- read_delim('./data/area_filtered.txt', delim = ',')

##### 5. AREA ANIMATION #####

area <- area_filtered %>%
  filter(as.integer(Drought_Duration) <= 2) %>%
  mutate(Scenario = str_replace(Scenario, 's', 'S')) %>%
  mutate(Scenario = str_replace(Scenario, '_', ' ')) %>%
  unite(col = Scen, Scenario, id, sep = '   ') %>%
  group_by(Scen, Lat, Lon) %>%
  summarise_at(vars(Impact, Recovery, Stability, Precip_Impact, Precip_Recovery, Precip_Stability), mean, na.rm = TRUE) %>%
  ungroup() 

##### __5.1. Recovery (baseline normalized) #####

anim_recovery <- area %>%
  unite(col = Coord, Lat, Lon, remove = FALSE) %>%
  ggplot() +
  geom_tile(aes(x = Lon, y = Lat, fill = Recovery, group = Coord)) +
  scale_fill_gradientn(colors = c('#4c4c4c', '#7395D6', '#1650BB', '#0D2F70')) +
  theme_cowplot() +
  theme(text = element_text(family = "Source Sans Pro", size = 14), aspect.ratio = 1, legend.title = element_blank()) +
  transition_states(states = Scen, transition_length = 0.75, state_length = 0.25) +
  labs(title = 'Recovery    {closest_state}') +
  ease_aes('cubic-in-out') +
  enter_fade(alpha = 0) + exit_fade(alpha = 0)

anim_recovery <- animate(anim_recovery, duration = 48, fps = 10, bg = 'transparent')

anim_save('./plots/animations/recovery_gif.gif', animation = anim_recovery)

##### __5.2. Precipitation Increase #####

anim_precip_recovery <- area %>%
  unite(col = Coord, Lat, Lon, remove = FALSE) %>%
  ggplot(aes(x = Lon, y = Lat, fill = Precip_Recovery, group = Coord)) +
  geom_tile() +
  scale_fill_gradientn(colors = c('#4c4c4c', '#7395D6', '#1650BB', '#0D2F70')) +
  theme_cowplot() +
  theme(text = element_text(family = "Source Sans Pro", size = 14), aspect.ratio = 1, legend.title = element_blank()) +
  transition_states(states = Scen, transition_length = 0.75, state_length = 0.25) +
  labs(title = "Precipitation Increase    {closest_state}") +
  ease_aes('cubic-in-out') +
  enter_fade(alpha = 0) + exit_fade(alpha = 0)

anim_precip_recovery <- animate(anim_precip_recovery, duration = 48, fps = 10, bg = 'transparent')

anim_save('./plots/animations/precip_recovery_gif.gif', animation = anim_precip_recovery)

##### __5.3. Impact #####

anim_impact <- area %>%
  unite(col = Coord, Lat, Lon, remove = FALSE) %>%
  ggplot(aes(x = Lon, y = Lat, fill = Impact, group = Coord)) +
  geom_tile() +
  scale_fill_gradientn(colors = c('#CA9399', '#A84C56', '#642D33')) +
  theme_cowplot() +
  theme(text = element_text(family = "Source Sans Pro", size = 14), aspect.ratio = 1, legend.title = element_blank()) +
  transition_states(states = Scen, transition_length = 0.75, state_length = 0.25) +
  labs(title = "{closest_state}     Impact") +
  ease_aes('cubic-in-out') +
  enter_fade(alpha = 0) + exit_fade(alpha = 0)

anim_impact <- animate(anim_impact, duration = 48, fps = 10, bg = 'transparent')

anim_save('./plots/animations/impact_gif.gif', animation = anim_impact)

##### __5.4. Precipitation Decrease #####

anim_precip_impact <- area %>%
  unite(col = Coord, Lat, Lon, remove = FALSE) %>%
  ggplot(aes(x = Lon, y = Lat, fill = Precip_Impact, group = Coord)) +
  geom_tile() +
  scale_fill_gradientn(colors = c('#CA9399', '#A84C56', '#642D33')) +
  theme_cowplot() +
  theme(text = element_text(family = "Source Sans Pro", size = 14), aspect.ratio = 1, legend.title = element_blank()) +
  transition_states(states = Scen, transition_length = 0.75, state_length = 0.25) +
  labs(title = "{closest_state}     Precipitation Decrease") +
  ease_aes('cubic-in-out') +
  enter_fade(alpha = 0) + exit_fade(alpha = 0)

anim_precip_impact <- animate(anim_precip_impact, duration = 48, fps = 10, bg = 'transparent')

anim_save('./plots/animations/precip_impact_gif.gif', animation = anim_precip_impact)

##### __5.5. Stability #####

anim_stability <- area %>%
  unite(col = Coord, Lat, Lon, remove = FALSE) %>%
  ggplot(aes(x = Lon, y = Lat, fill = Stability, group = Coord)) +
  geom_tile() +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_cowplot() +
  theme(text = element_text(family = "Source Sans Pro", size = 14), aspect.ratio = 1, legend.title = element_blank()) +
  transition_states(states = Scen, transition_length = 0.75, state_length = 0.25) +
  labs(title = "{closest_state}     Stability") +
  ease_aes('cubic-in-out') +
  enter_fade(alpha = 0) + exit_fade(alpha = 0)

anim_stability <- animate(anim_stability, duration = 48, fps = 10, bg = 'transparent')

anim_save('./plots/animations/stability_gif.gif', animation = anim_stability)

##### __5.6. Precipitation Stability #####

anim_precip_stability <- area %>%
  unite(col = Coord, Lat, Lon, remove = FALSE) %>%
  ggplot(aes(x = Lon, y = Lat, fill = Precip_Stability, group = Coord)) +
  geom_tile() +
  scale_fill_gradient(low = "gray", high = "black") +
  theme_cowplot() +
  theme(text = element_text(family = "Source Sans Pro", size = 14), aspect.ratio = 1, legend.title = element_blank()) +
  transition_states(states = Scen, transition_length = 0.75, state_length = 0.25) +
  labs(title = "{closest_state}     Precipitation Stability") +
  ease_aes('cubic-in-out') +
  enter_fade(alpha = 0) + exit_fade(alpha = 0)

anim_precip_stability <- animate(anim_precip_stability, duration = 48, fps = 10, bg = 'transparent')

anim_save('./plots/animations/precip_stability_gif.gif', animation = anim_precip_stability)
