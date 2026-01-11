library(tigris)
library(sf)
library(ggplot2)
library(dplyr)
library(zipcodeR)

source("utils.R")

options(tigris_use_cache = TRUE)

# ---- Data ----

midwest_svy <- readRDS("data/midwest_svy.rds")


# ---- Parameters ----
STATE_ABB = MIDWEST_STATES

options(tigris_class = "sf", tigris_use_cache = TRUE)

# ---- Step 1: Get ZIP prefixes for these states ----
state_zips <- zipcodeR::zip_code_db %>%
  filter(state %in% STATE_ABB) %>%
  pull(zipcode)

zip_prefixes <- unique(substr(sprintf("%05s", state_zips), 1, 2))

# ---- Step 2: Load ZCTAs and filter by ZIP prefix ----
zctas_all <- zctas(cb = TRUE, year = 2020)
zctas_state <- zctas_all %>%
  filter(substr(ZCTA5CE20, 1, 2) %in% zip_prefixes)

# ---- Step 3: Prepare ZIP-level survey data ----
zip_data <- midwest_svy %>%
  mutate(zip = sprintf("%05s", zip)) %>%
  group_by(zip) %>%
  summarise(value = weighted.mean(midwesterner == 1, w = weight), .groups = "drop")

# ---- Step 4: Join ZCTA polygons with ZIP data ----
zcta_merged <- zctas_state %>%
  mutate(zip = ZCTA5CE20) %>%
  left_join(zip_data, by = "zip")

# ---- Step 5: Plot ----
# Get state geometries (only the ones in your STATE_ABB list)
state_borders <- states(cb = TRUE, year = 2020) %>%
  filter(STUSPS %in% STATE_ABB) %>%
  st_transform(st_crs(zcta_merged))  # match CRS

state_centroids <- state_borders %>%
  mutate(centroid = st_point_on_surface(geometry)) %>%
  as_tibble() %>%
  mutate(state_abbr = state_borders$STUSPS) %>%
  rowwise() %>% 
  mutate(X = st_coordinates(centroid)[,1], 
         Y = st_coordinates(centroid)[,2])

p <- ggplot(zcta_merged) +
  geom_sf(aes(fill = value), color = "white") +
  geom_sf(data = state_borders, fill = NA, color = "black", 
          linewidth = 1, size = 5) +
  geom_text(
    data = state_centroids,
    aes(x = X, y = Y, label = state_abbr),
    fontface = "bold",
    size = 12,
    family = "Helvetica"
  ) +
  scale_fill_viridis_c(na.value = "white",
                       name = "Population-weighted %\nof survey respondents:",
                       labels = percent_format(accuracy = 1)) +
  theme_void() +
  labs(
    title = paste("Percent of Americans Who Identify as Midwestern"),
    caption = "Data: Middle West Review (2023)\nAuthor: Soubhik Barari",
    fill = "Your Variable"
  ) +
  theme_map() +
  theme(
    legend.position = "top",
    legend.title.align = 0.5,
    legend.direction = "horizontal",
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size=20)
  )
fp <- paste0("plots/zcta_map_", paste0(STATE_ABB, collapse="-"), ".png")
ggsave(fp, p, width = 24, height = 12)
system(paste("open", fp))
