library(tidyverse)
library(scales)
library(ggthemes)
library(rgdal)
library(rgeos)
library(maps)
library(ggrepel)

source("utils.R")

# ---- Data ----

midwest_svy <- readRDS("data/midwest_svy.rds")

# Summarize % who said "Yes" to Midwest (coded as 1)
midwest_by_state <- midwest_svy %>%
  group_by(state) %>%
  summarise(
    total_weight = sum(weight, na.rm = TRUE),
    midwest_yes_weight = sum(weight[midwest == 1], na.rm = TRUE),
    pct_midwest = midwest_yes_weight / total_weight
  ) %>%
  left_join(state_crosswalk, by = "state")

# ---- Map ----

states_map <- map_data("state")

# Join to map data
state_map_data <- states_map %>%
  left_join(midwest_by_state %>%
              mutate(region = tolower(state_name)), 
            by = "region")

# ---- Compute state centroids for labels ----

state_centroids <- state_map_data %>%
  group_by(region) %>%
  summarise(
    state_abbr = first(state_abbr, na_rm = T),
    long = mean(range(long)),
    lat = mean(range(lat)),
    pct_midwest = first(pct_midwest)
  ) %>%
  mutate(label = ifelse(is.na(pct_midwest), "", percent(pct_midwest, accuracy = 1)))

# ---- Plot ----

ggplot(state_map_data %>%
         mutate(fill = region %in% tolower(c(
           "Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"
         ))), aes(x = long, y = lat, group = group, fill = ifelse(fill, "red", "grey90"))) +
  geom_polygon(color = "gray20", size = 0.3) +
  geom_text(
    data = state_centroids %>%
      filter(
        region %in% tolower(c(
          "Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"
        ))
      ),
    aes(x = if_else(state_abbr == "MI", long + 1.5, long), 
        y = if_else(state_abbr == "MI", lat - 1.5, lat), 
        label = state_abbr),
    color = "white",
    inherit.aes = FALSE,
    size = 2,
    fontface = "bold",
    family = "Helvetica"
  ) +
  coord_fixed(1.3) +
  scale_fill_identity() +
  scale_color_identity() + 
  # scale_fill_viridis_c(
  #   name = "Population-weighted %\nof survey respondents:",
  #   labels = percent_format(accuracy = 1),
  #   na.value = "gray90"
  # ) +
  theme_map() +
  theme(
    legend.position = "top",
    legend.title.align = 0.5,
    legend.direction = "horizontal",
    text = element_text(family = "Helvetica"),
    plot.title = element_text(size = 16, face = "bold")
  )
ggsave("plots/state_midwest_map.png", height=5, width=7)

ggplot(state_map_data, aes(x = long, y = lat, group = group, fill = pct_midwest)) +
  geom_polygon(color = "gray20", size = 0.3) +
  geom_text(
    data = state_centroids,
    aes(x = if_else(state_abbr == "MI", long + 2.2, long), 
        y = if_else(state_abbr == "MI", lat - 1.5, lat), 
        label = paste0(state_abbr, ": ",label,""),
        color = ifelse(pct_midwest > 0.5, "black", "white")),
    inherit.aes = FALSE,
    size = 2,
    fontface = "bold",
    family = "Helvetica"
  ) +
  coord_fixed(1.3) +
  scale_color_identity() + 
  scale_fill_viridis_c(
    name = "Population-weighted %\nof survey respondents:",
    labels = percent_format(accuracy = 1),
    na.value = "gray90"
  ) +
  labs(
    title = "Percent of Americans Who Identify as Midwestern",
    caption = "Data: Middle West Review (2023)\nAuthor: Soubhik Barari"
  ) +
  theme_map() +
  theme(
    legend.position = "top",
    legend.title.align = 0.5,
    legend.direction = "horizontal",
    text = element_text(family = "Helvetica"),
    plot.title = element_text(size = 16, face = "bold")
  )
ggsave("plots/state_weighted_map.png", height=5, width=7)
