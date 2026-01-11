
# Preamble ----------------------------------------------------------------

here::set_here(".")

source("utils.R")

dir.create("data", showWarnings = F)
dir.create("plots", showWarnings = F)

REFRESH_CACHE = TRUE

# Survey Data --------------------------------------------------------------

if (file.exists("data/midwest_svy.rds") & !REFRESH_CACHE) {
  message("Reading cached data")
} else {
  
  # if (!file.exists(here::here("data/middle-west-review-qs.xlsx"))) {
  #   download.file("https://uimiddle.files.wordpress.com/2023/10/middle-west-review-qs.xlsx",
  #                 destfile = here::here("data/middle-west-review-qs.xlsx"))
  # }
  if (!file.exists(here::here("data/midwest-responses-state.xlsx"))) {
    download.file("https://uimiddle.files.wordpress.com/2023/10/midwest-responses-state.xlsx",
                  destfile = here::here("data/midwest-responses-state.xlsx"))
  }
  
  # midwest_responses_state <- read_excel(("data/midwest-responses-state.xlsx")) %>%
  #   janitor::clean_names()
  # glimpse(midwest_responses_state)
  
  midwest_responses_q <- read_excel(("data/middle-west-review-qs.xlsx")) %>%
    janitor::clean_names()
  glimpse(midwest_responses_q)
  
  midwest_responses_codebook <- 
    read_excel("data/middle-west-review-qs.xlsx", sheet = 2, col_names = c("val", "label"))
  
  midwest_responses_q %>%
    group_by(zip) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  
  # install.packages("labelled")
  library(labelled)
  library(dplyr)
  
  # Get indices of each question (label == "Q")
  question_starts <- which(midwest_responses_codebook$val == "Q")
  
  # Add a final row index to delimit the last block
  question_blocks <- c(question_starts, nrow(midwest_responses_codebook) + 1)
  
  # Create a list of variable-to-label mappings
  codebook_labels <- list()
  
  for (i in seq_along(question_starts)) {
    q_start <- question_blocks[i]
    q_end <- question_blocks[i + 1] - 1
    
    question_text <- midwest_responses_codebook$label[q_start]
    
    value_block <- midwest_responses_codebook[(q_start + 1):q_end, ] %>%
      filter(!is.na(val), !is.na(label))
    
    # Coerce to numeric safely
    value_block <- value_block %>%
      mutate(val_num = suppressWarnings(as.numeric(val))) %>%
      filter(!is.na(val_num))
    
    if (nrow(value_block) > 0) {
      value_map <- setNames(value_block$val_num, value_block$label)
      codebook_labels[[question_text]] <- value_map
    }
  }
  
  
  # Define a mapping from question text to variable names
  question_to_var <- c(
    "Do you consider yourself to currently live in the Midwest?" = "midwest",
    "Do you consider yourself to be a Midwesterner?" = "midwesterner",
    "Do you consider yourself to be conservative, moderate, or liberal?" = "ideology",
    "What is your party registration?" = "partyregaff",
    "What is your age range?" = "age",
    "For statistical purposes only, can you please tell me your ethnicity?" = "race",
    "Who did you vote for in the 2020 election?" = "otehistory",
    "What is the highest level of education you have attained?" = "education",
    "Can you please tell me your gender?" = "gender"
  )
  
  # Apply value labels
  for (q in names(question_to_var)) {
    print(q)
    var <- question_to_var[[q]]
    
    if (var %in% names(midwest_responses_q) && q %in% names(codebook_labels)) {
      midwest_responses_q[[var]] <- set_value_labels(midwest_responses_q[[var]], codebook_labels[[q]])
      midwest_responses_q[[var]] <- set_variable_labels(midwest_responses_q[[var]], q)
    }
  }
  
  midwest_responses_q$state <-
    labelled::set_value_labels(midwest_responses_q$state, 
                               setNames(
                                 unique(state_crosswalk$state),
                                 unique(state_crosswalk$state_abbr)
                               ))
  
  # View value labels
  val_labels(midwest_responses_q$midwesterner)
  
  # View variable label
  var_label(midwest_responses_q$midwesterner)
  
  saveRDS(midwest_responses_q, file = "data/midwest_svy.rds")
}

midwest_svy <- readRDS("data/midwest_svy.rds")

# Population Data ---------------------------------------------------------

## Pres Vote
pres_2020 <- read_csv(
  "../data/medsl/County_Presidential_Election_Data_2020_0_0_2_c.csv"
) %>% janitor::clean_names()
head(pres_2020)

midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
midwest_counties <- tigris::counties(cb = TRUE, year = 2020) %>%
  filter(STATE_NAME %in% midwest_states) %>%
  as.data.frame()

midwest_state_pres_vote <- pres_2020 %>% 
  filter(geographic_subtype == "County") %>%
  mutate(fips = str_pad(fips, width = 5, pad = "0")) %>%
  separate(fips, into = c("STATEFP", "COUNTYFP"), sep = 2, remove = FALSE) %>%
  inner_join(midwest_counties %>%
               select(STATE_NAME, STATEFP, COUNTYFP), by = c("STATEFP", "COUNTYFP")) %>%
  group_by(STATE_NAME) %>%
  summarise(total_vote = sum(as.numeric(total_vote)),
            biden_vote = sum(as.numeric(joseph_r_biden_jr)),
            trump_vote = sum(as.numeric(donald_j_trump)))

midwest_pres_vote <- midwest_state_pres_vote %>%
  summarise(total_vote = sum(as.numeric(total_vote)),
            biden_vote = sum(as.numeric(biden_vote)),
            trump_vote = sum(as.numeric(trump_vote))) %>%
  transmute(biden_pct = biden_vote/total_vote,
         trump_pct = trump_vote/total_vote,
         other_pct = (total_vote-(biden_vote+trump_vote))/total_vote) %>%
  pivot_longer(everything())

## Ideology
ces_2020 <- haven::read_dta(
  "../data/cces/CES20_Common_OUTPUT_vv.dta"
)
midwest_partyid <- ces_2020 %>%
  filter(inputstate %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)) %>%
  select(pid3, commonpostweight) %>%
  labelled::unlabelled() %>%
  mutate(pid3 = if_else(pid3 %in% c("Independent","Other","Not sure"), "Independent/other", pid3)) %>%
  count(pid3, wt = commonpostweight) %>%
  mutate(party_pct = n / sum(n)) %>%
  arrange(desc(party_pct))

midwest_ideology <- ces_2020 %>%
  filter(inputstate %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)) %>%
  mutate(
    ideo5 = recode(
      labelled::to_character(ideo5),
      "Very conservative" = "Conservative",
      "Very liberal" = "Liberal"
    )
  ) %>%
  count(ideo5, wt = commonpostweight) %>%
  mutate(ideo_pct = n / sum(n)) %>%
  arrange(desc(ideo_pct))

## Census Demos
library(tidycensus)
v <- load_variables(2022, "acs5", cache = TRUE)

age_vars <- c(
  total = "B01001_001",
  male_18_24 = "B01001_007",
  female_18_24 = "B01001_031",
  male_25_34 = "B01001_008",
  female_25_34 = "B01001_032",
  male_35_44 = "B01001_009",
  female_35_44 = "B01001_033",
  male_45_54 = "B01001_010",
  female_45_54 = "B01001_034",
  male_55_64 = "B01001_011",
  female_55_64 = "B01001_035",
  male_65_74 = "B01001_012",
  female_65_74 = "B01001_036",
  male_75_84 = "B01001_013",
  female_75_84 = "B01001_037",
  male_85_over = "B01001_014",
  female_85_over = "B01001_038"
)

sex_vars <- c(
  male_total = "B01001_002",
  female_total = "B01001_026"
)

educ_vars <- c(
  less_than_hs = "B15003_002",
  hs_grad = "B15003_017",
  some_college = "B15003_018",
  bachelors = "B15003_022",
  graduate = "B15003_023"
)

race_ethnicity_vars <- c(
  white = "B02001_002",
  black = "B02001_003",
  asian = "B02001_005",
  hispanic = "B03003_003",
  other = "B02001_008"
)

acs_data <- get_acs(
  geography = "county",
  variables = c(
    #age_vars, 
    sex_vars, 
    educ_vars, race_ethnicity_vars
    ),
  year = 2020,
  survey = "acs5",
  output = "wide"
)

head(acs_data)

midwest_demos <- acs_data %>%
  inner_join(midwest_counties %>%
               select(STATE_NAME, GEOID), by = "GEOID") %>%
  pivot_longer(cols = -c(GEOID, NAME, STATE_NAME), 
               names_to = "var", values_to = "val") %>%
  mutate(type = if_else(str_detect(var, "E$"), "Estimate", "Margin")) %>%
  filter(type == "Estimate") %>%
  mutate(var_type = case_when(
    grepl("(male_|female_)\\d{2}", var) ~ "age",
    var %in% c("male_totalE", "female_totalE") ~ "gender",
    var %in% c("less_than_hsE", "hs_gradE", "some_collegeE", "bachelorsE", "graduateE") ~ "education",
    var %in% c("whiteE", "blackE", "asianE", "hispanicE", "otherE") ~ "race",
    var %in% c("totalE") ~ "total"
  )) %>%
  mutate(var = case_when(
    var %in% c("male_18_24E", "female_18_24E") ~ "18-24",
    var %in% c("male_25_34E", "female_25_34E") ~ "25-29",
    var %in% c("male_35_44E", "female_35_44E") ~ "30-39",
    var %in% c("male_45_54E", "female_45_54E") ~ "40-49",
    var %in% c("male_55_64E", "female_55_64E") ~ "50-59",
    var %in% c("male_65_74E", "female_65_74E") ~ "60-69",
    var %in% c("male_75_84E", "female_75_84E", "male_85_overE", "female_85_overE") ~ "70 or more years",
    TRUE ~ var
  )) %>%
  summarise(val = sum(val, na.rm = T), .by = c(var, var_type)) %>%
  # manual correction
  summarise(val = if_else(var == "70 or more years", 2.35e6 + 1.72e6 + 1.30e6 + 1.41e6, sum(val, na.rm = TRUE)), .by = c(var, var_type)) %>%
  as.data.frame()

### manually copying in ACS age
age_df <- data.frame(
  age_group = c(
    "85+", "80-84", "75-79", "70-74", "67-69", "65-66", "62-64", "60-61",
    "55-59", "50-54", "45-49", "40-44", "35-39", "30-34", "25-29", "22-24",
    "21", "20", "18-19", "15-17", "10-14", "5-9", "0-4"
  ),
  count = c(
    1410000, 1300000, 1720000, 2350000, 1800000, 1450000, 2380000, 1760000,
    4730000, 4860000, 4410000, 4200000, 4100000, 4400000, 4430000, 2800000,
    990000, 1000000, 1860000, 2720000, 4440000, 4370000, 4180000
  ),
  pct = c(
    2.08, 1.93, 2.55, 3.47, 2.67, 2.15, 3.52, 2.60,
    7.00, 7.18, 6.52, 6.20, 6.06, 6.50, 6.54, 4.13,
    1.46, 1.48, 2.75, 4.01, 6.56, 6.46, 6.18
  )
)

midwest_adult_ages <- age_df %>%
  # keep adults only
  filter(!age_group %in% c("0-4", "5-9", "10-14", "15-17")) %>%
  
  # recode to target age bins
  mutate(age_bin = case_when(
    age_group %in% c("18-19", "20", "21", "22-24") ~ "18-24",
    age_group == "25-29"                          ~ "25-29",
    age_group %in% c("30-34", "35-39")            ~ "30-39",
    age_group %in% c("40-44", "45-49")            ~ "40-49",
    age_group %in% c("50-54", "55-59")            ~ "50-59",
    age_group %in% c("60-61", "62-64", "65-66", "67-69") ~ "60-69",
    age_group %in% c("70-74", "75-79", "80-84", "85+") ~ "70 or more years",
    TRUE ~ NA_character_
  )) %>%
  
  # aggregate counts
  group_by(age_bin) %>%
  summarise(
    count = sum(count),
    .groups = "drop"
  ) %>%
  
  # compute adult percentages
  mutate(
    pct = count / sum(count)
  )

## Merge Together
midwest_popdata <- bind_rows(
  midwest_pres_vote %>%
    transmute(var="otehistory", val=name, pct_pop=value),
  midwest_ideology %>%
    transmute(var="ideology", val=ideo5, pct_pop=ideo_pct),
  midwest_partyid %>% 
    transmute(var="partyregaff", val=pid3, pct_pop=party_pct),
  midwest_demos %>%
    group_by(var_type, var) %>%
    summarise(n = sum(val)) %>%
    mutate(pct = n /sum(n)) %>%
    ungroup() %>%
    select(var=var_type, val=var, pct_pop=pct),
  midwest_adult_ages %>%
    transmute(var="age", val=age_bin, pct_pop=pct)
) %>% 
  as.data.frame()

