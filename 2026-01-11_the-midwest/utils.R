library(readxl)
library(here)
library(janitor)
library(tidyverse)
library(ggthemes)
library(stringr)
library(maps)
library(scales)
library(viridis)
library(janitor)

MIDWEST_STATES = 
  c("IL", "IN", "IA", "KS", "MI", "MN", "MO", 
    "NE", "ND", "OH", "SD", "WI")
MIDWEST_STATES_LABEL =
  paste0(state.name[match(MIDWEST_STATES, state.abb)], collapse=", ") |> str_replace(", ([A-za-z]*)$", ", and \\1")

# Crosswalk from survey coding
state_crosswalk <- tribble(
  ~state, ~state_abbr, ~state_name,
  3, "AR", "Arkansas",
  7, "CO", "Colorado",
  16, "IA", "Iowa",
  17, "ID", "Idaho",
  18, "IL", "Illinois",
  19, "IN", "Indiana",
  20, "KS", "Kansas",
  21, "KY", "Kentucky",
  27, "MI", "Michigan",
  28, "MN", "Minnesota",
  29, "MO", "Missouri",
  32, "MT", "Montana",
  34, "ND", "North Dakota",
  35, "NE", "Nebraska",
  41, "OH", "Ohio",
  42, "OK", "Oklahoma",
  44, "PA", "Pennsylvania",
  49, "SD", "South Dakota",
  50, "TN", "Tennessee",
  57, "WI", "Wisconsin",
  58, "WV", "West Virginia",
  59, "WY", "Wyoming"
)

plot_state_map <- function(state_data, value_col = "pct_midwest", title = "", caption = "") {
  require(ggplot2)
  require(dplyr)
  require(scales)
  require(maps)
  require(ggthemes)
  
  states_map <- map_data("state")
  
  # Join value to map
  state_map_data <- states_map %>%
    left_join(state_data %>% 
                left_join(state_crosswalk, by = "state") %>%
                mutate(region = tolower(state_name)), 
              by = "region")
  
  state_centroids <- state_map_data %>%
    group_by(region) %>%
    summarise(
      state_abbr = first(state_abbr, na.rm = TRUE),
      long = mean(range(long)),
      lat = mean(range(lat)),
      value = first(.data[[value_col]])
    ) %>%
    mutate(label = ifelse(is.na(value), "", percent(value, accuracy = 1)))
  
  ggplot(state_map_data, aes(x = long, y = lat, group = group, fill = .data[[value_col]])) +
    geom_polygon(color = "gray20", size = 0.3) +
    geom_text(
      data = state_centroids,
      aes(x = long, y = lat, label = paste0(state_abbr, ": ", label),
          color = ifelse(value > 0.5, "black", "white")),
      inherit.aes = FALSE,
      size = 3
    ) +
    coord_fixed(1.3) +
    scale_color_identity() +
    scale_fill_viridis_c(name = "Value", labels = percent_format(accuracy = 1), na.value = "gray90") +
    labs(title = title, caption = caption) +
    theme_map() +
    theme(
      legend.position = "top",
      legend.title.align = 0.5,
      legend.direction = "horizontal",
      plot.title = element_text(size = 16, face = "bold")
    )
}


plot_zip_dots <- function(zip_data, title = "", caption = "") {
  require(zipcodeR)
  require(ggplot2)
  require(dplyr)
  require(scales)
  
  zip_coords <- zipcodeR::zip_code_db %>%
    select(zip = zipcode, lat = latitude, lng = longitude)
  
  zip_points <- zip_data %>%
    mutate(zip = sprintf("%05s", zip)) %>%
    left_join(zip_coords, by = "zip") %>%
    filter(!is.na(lat) & !is.na(lng))
  
  ggplot(zip_points, aes(x = lng, y = lat, color = value)) +
    borders("state") +
    geom_point(size = 2, alpha = 0.7) +
    coord_fixed(1.3) +
    scale_color_viridis_c(labels = percent_format(accuracy = 1)) +
    theme_minimal() +
    labs(title = title, color = "Value", caption = caption)
}

plot_zip_polygons <- function(zip_data, state_prefixes = c("60"), title = "", caption = "") {
  require(tigris)
  require(sf)
  require(ggplot2)
  require(dplyr)
  require(scales)
  
  options(tigris_use_cache = TRUE, tigris_class = "sf")
  
  zctas <- zctas(cb = TRUE, year = 2020)
  zctas <- zctas %>%
    filter(substr(ZCTA5CE20, 1, 2) %in% state_prefixes)
  
  zip_data <- zip_data %>%
    mutate(zip = sprintf("%05s", zip))
  
  zcta_merged <- zctas %>%
    mutate(zip = ZCTA5CE20) %>%
    left_join(zip_data, by = "zip")
  
  ggplot(zcta_merged) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_viridis_c(name = "Value", labels = percent_format(accuracy = 1), na.value = "gray90") +
    coord_sf() +
    theme_minimal() +
    labs(title = title, fill = "Value", caption = caption)
}
