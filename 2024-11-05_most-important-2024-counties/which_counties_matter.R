
# Set up ------------------------------------------------------------------

library(ggplot2)
library(ggrepel)
library(scales)
library(tidyverse)
library(readxl)
library(tidycensus)
library(here)
library(janitor)

readRenviron(".Renviron")

save_path = file.path(dirname(rstudioapi::getSourceEditorContext()$path), "figures")

# Data --------------------------------------------------------------------

census_api_key(
  Sys.getenv("CENSUS_API_KEY"),
  install = TRUE, overwrite = TRUE
)

if (interactive()) {
  View(load_variables(2022, "acs5", cache = TRUE))
}

# ## voter reg
# reg_20 <- read_excel(here("data/leip/TurnoutData2020.xlsx"),
#            sheet = "County VTO") %>%
#   clean_names() %>%
#   slice(-1) %>%
#   select(county=1, state_code=2, reg_total=6, reg_active=7)

## county characteristics
data("fips_codes", package = "tidycensus")

state_fips <- fips_codes %>%
  select(state, state_code, state_name) %>%
  distinct()

acs_22y5_counties_raw <- get_acs(
  geography = "county",
  variables = list(hisp="B03003_003",
                   white = "B02001_002", 
                   black = "B02001_003",
                   foreign = "B05002_013",
                   hs = "B15003_017",
                   college = "B15003_022",
                   age_med = "B01002_001",
                   hhinc_med = "B19013_001",
                   total = "B01003_001"),
  survey = "acs5",
  year = 2022
) 

acs_22y5_counties <- acs_22y5_counties_raw %>%
  clean_names() %>%
  mutate(state_code = substr(geoid, 1, 2),
         name = gsub(",.*", "", name)) %>%
  left_join(state_fips, by = c("state_code" = "state_code")) %>%
  group_by(variable) %>%
  pivot_wider(id_cols = c("name","state"), 
              names_from = "variable", values_from = "estimate") %>%
  mutate(across(c("white","black","hisp","foreign","hs","college"), ~.x/total))

## county-level pres returns
pres_20 <- read_csv(here("data/medsl/County_Presidential_Election_Data_2020_0_0_2_c.csv"),
                    skip = 1) %>%
  clean_names() %>%
  rename(dvote=vote1, rvote=vote2) %>%
  mutate(fips = stringr::str_pad(fips, width=5, pad = "0")) %>%
  mutate(state_code = substr(fips, 1, 2)) %>%
  group_by(state_code) %>%
  mutate(statevote = sum(totalvote),
         statewinner = ifelse(sum(rvote) > sum(dvote), "Trump", "Biden")) %>%
  ungroup() %>%
  select(fips, state_code, name, dvote, rvote, totalvote, statevote, statewinner) %>%
  left_join(state_fips, by = c("state_code" = "state_code")) %>%
  mutate(dmargin = c(dvote-rvote)/c(dvote+rvote),
         stateshare = totalvote/statevote,
         countywinner = ifelse(dmargin > 0, "Biden Won", "Trump Won"),
         year = 2020)

pres_16 <- read_csv(here("data/medsl/County_Presidential_Election_Data_2016_0_0_2_c.csv"),
                    skip = 1) %>%
  clean_names() %>%
  rename(dvote=vote1, rvote=vote2) %>%
  mutate(fips = stringr::str_pad(fips, width=5, pad = "0")) %>%
  mutate(state_code = substr(fips, 1, 2)) %>%
  group_by(state_code) %>%
  mutate(statevote = sum(totalvote),
         statewinner = ifelse(sum(rvote) > sum(dvote), "Trump", "Clinton")) %>%
  ungroup() %>%
  select(fips, state_code, name, dvote, rvote, totalvote, statevote, statewinner) %>%
  left_join(state_fips, by = c("state_code" = "state_code")) %>%
  mutate(dmargin = c(dvote-rvote)/c(dvote+rvote),
         stateshare = totalvote/statevote,
         countywinner = ifelse(dmargin > 0, "Clinton Won", "Trump Won"),
         year = 2016)

pres_16_20 <- pres_16 %>%
  left_join(pres_20, by = c("fips","name","state_name","state_code","state"),
            suffix = c("16","20"))
  
# Plots -------------------------------------------------------------------

comp_imp_plot <- function(data) {
  data %>%
    ggplot(aes(y=1-abs(dmargin), x=stateshare)) +
    geom_point(aes(size=totalvote, color=countywinner)) +
    facet_wrap(~ paste0(state_name,"\n(", statewinner, " Won in ", year, ")"), scales = "free") +
    geom_text_repel(aes(label = name), box.padding=0.3, max.overlaps=5, size=4) +
    scale_color_manual(values=c("blue", "red"), name = "County Winner:") +
    scale_size(guide="none") +
    # scale_x_continuous(labels = \(.) sprintf("%s%d%%", ifelse(.>0,"+","-"), round(abs(.)*100)),
    #                    limits = rev) +
    scale_y_continuous(labels = \(.) sprintf("%d%%", round(100*(1-.))),
                       #limits = c(0.5, 1),
                       name = 'Competition in County\n(Absolute % Democratic Margin in 2020)') +
    scale_x_continuous(labels = percent_format(1),
                       name = 'Importance of County\n(% of State Votes)') +
    theme_bw() +
    labs(caption = "Source: MIT Election Data Science Lab\nAuthor: Soubhik Barari") +
    theme(legend.position = "top",
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          plot.title = element_text(size=16, face="bold"),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          strip.text = element_text(size=14))
}

pres_16 %>%
  filter(state %in% c("AZ","PA","WI","MI","NV","GA","NC")) %>%
  comp_imp_plot()

pres_20 %>%
  filter(state %in% c("AZ","PA","WI","MI","NV","GA","NC")) %>%
  comp_imp_plot()

# Sun Belt
# only a few big competitive counties matter,
# large shares of Hispanics / Foreign-Born
pres_20 %>%
  filter(state %in% c("AZ","NV")) %>%
  comp_imp_plot() + ggtitle("The Sun Belt: A Few Important and Competitive Counties",
                            subtitle = "Pivotal Bloc: Foreign-Born Hispanics")
ggsave(file.path(save_path, "sun_belt.png"), height=6, width=8)
acs_22y5_counties %>%
  filter(grepl("(Maricopa)", name), state %in% c("AZ"))
acs_22y5_counties %>%
  filter(grepl("(Clark|Washoe)", name), state %in% c("NV"))

# Rust Belt
# diploma divide / blue-collar white workers really matters
pres_20 %>%
  filter(state %in% c("MI","WI")) %>%
  comp_imp_plot() + ggtitle("The Midwest: Blue Strongholds and a Few Cliffhangers", 
                            subtitle = "Pivotal Bloc: Non-College Whites")
ggsave(file.path(save_path, "midwest.png"), height=6, width=8)


# note, though, that in Bucks County, you have a higher income than the rest of the state
# given the "gradient", must maintain
pres_20 %>%
  filter(state %in% c("PA")) %>%
  comp_imp_plot() + ggtitle("The Keystone State: The Full Political Spectrum on Display", 
                            subtitle = "Pivotal Bloc(s): Suburban Whites + Urban Blacks")
ggsave(file.path(save_path, "pa.png"), height=8, width=8)
acs_22y5_counties %>%
  filter(grepl("(Saginaw|Eaton)", name), 
         state %in% c("MI"))
acs_22y5_counties %>%
  filter(grepl("(Bucks|Erie|Northampton)", name), 
         state %in% c("PA"))
acs_22y5_counties %>%
  filter(grepl("(Kenosha|Winnebago|Brown|Racine)", name), 
         state %in% c("WI"))

# Bible Belt
# In GA, strong Dem turnout in a few big ATL metros won it for Biden
# In NC, despite strong Dem turnout in Charlotte and Raleigh counties, many medium-sized 
# red counties won it for Trump
# In both, not a whole lot of counties in the middle for R's to concentrate turnout (e.g. Macomb in MI or Waukesha in WI)
pres_20 %>%
  filter(state %in% c("GA","NC")) %>%
  comp_imp_plot() + ggtitle("The Bible Belt: Small Reds vs. Big Blues",
                            subtitle = "Pivotal Bloc(s): Middle-Class Blacks + Metro-Area College")
ggsave(file.path(save_path, "bible.png"), height=6, width=8)

acs_22y5_counties %>%
  filter(grepl("(Cobb|Gwinnett)", name), 
         state %in% c("GA"))
acs_22y5_counties %>%
  filter(grepl("(New Hanover|Nash|Cabarrus)", name), 
         state %in% c("NC"))


# Identify flippers -------------------------------------------------------


# Assuming your dataframe is named 'df' with columns:
# 'county', 'state', 'total_votes', 'dem_votes', 'rep_votes'

identify_flippers <- function(pres_df, state, swing = 0.03) {
  df = pres_df[pres_df$state == state,]
  
  # Step 1: Calculate the initial state-level totals
  state_dem_votes <- sum(df$dvote)
  state_rep_votes <- sum(df$rvote)
  
  # Step 2: Calculate the margin (difference between Democratic and Republican votes)
  initial_margin <- state_dem_votes - state_rep_votes
  
  # Step 3: Define a function to calculate the effect of a 3% change in either dem or rep votes
  check_flip <- function(row, swing) {
    # Calculate the effect of increasing Democratic votes by 3%
    new_state_dem_votes <- state_dem_votes + swing * as.numeric(row[["dvote"]])
    dem_flip_margin <- new_state_dem_votes - state_rep_votes
    
    # Calculate the effect of increasing Republican votes by 3%
    new_state_rep_votes <- state_rep_votes + swing * as.numeric(row[["rvote"]])
    rep_flip_margin <- state_dem_votes - new_state_rep_votes
    
    # Check if either change could result in a flip
    dem_flip_possible <- ifelse(initial_margin < 0, dem_flip_margin > 0, dem_flip_margin < 0)
    rep_flip_possible <- ifelse(initial_margin < 0, rep_flip_margin > 0, rep_flip_margin < 0)
    
    return(dem_flip_possible || rep_flip_possible)
  }
  
  # Step 4: Apply this function to each county in the dataframe
  df$critical_county <- apply(df, 1, check_flip, swing = swing)
  
  # Step 5: Filter for critical counties
  critical_counties <- df[df$critical_county == TRUE, ]
  
  # Display the critical counties
  critical_counties <- critical_counties[, c("name", "state", "dvote", "rvote", "totalvote")]
  print(critical_counties)
}

identify_flippers(pres_20, state = "NC", swing = 0.05)

