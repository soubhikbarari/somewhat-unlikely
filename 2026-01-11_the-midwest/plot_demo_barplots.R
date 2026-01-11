library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(rlang)

source("utils.R")

# ---- Helpers ----

# Function to summarize weighted % Yes by group
summarize_pct_yes_by_groups <- function(data, outcome_var, group_vars, weight_var = "weight") {
  outcome_var <- rlang::ensym(outcome_var)
  weight_var <- rlang::ensym(weight_var)
  
  purrr::map_dfr(group_vars, function(group_var) {
    group_sym <- rlang::sym(group_var)
    
    data_clean <- data %>%
      mutate(
        outcome_raw = !!outcome_var,
        weight_val = !!weight_var,
        group_level = .data[[group_var]]
      ) %>%
      labelled::unlabelled() %>%
      mutate(outcome = as.numeric(outcome_raw) == 1) %>%
      filter(!is.na(group_level))
    
    data_clean %>%
      group_by(group_level) %>%
      filter(!is.na(outcome), !is.na(weight_val)) %>%
      summarise(
        group_var = group_var,
        pct_yes = weighted.mean(outcome, w = weight_val, na.rm = TRUE),
        n_yes = sum(outcome),
        n = sum(!is.na(outcome)),
        .groups = "drop"
      ) %>%
      mutate(
        group_level = factor(group_level, levels = unique(data_clean$group_level))
      )
  })
}


# Function to plot the summary
# plot_pct_yes_summary <- function(summary_df, title) {
#   ggplot(summary_df, aes(x = fct_rev(group_level), y = pct_yes)) +
#     geom_col(fill = "#6C8C52") +  # Muted midwestern green
#     geom_text(
#       aes(label = scales::percent(pct_yes, accuracy = 1)),
#       hjust = -0.1,
#       size = 3
#     ) +
#     scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
#     coord_flip() +
#     facet_grid(group_var ~ ., scales = "free_y", space = "free_y") +
#     labs(
#       x = NULL,
#       y = "% who say Yes",
#       title = title,
#       caption = "Weighted proportions by demographic group"
#     ) +
#     theme_minimal(base_family = "Helvetica") +
#     theme(
#       strip.text = element_text(face = "bold"),
#       text = element_text(family = "Helvetica"),
#       plot.title = element_text(size = 16, face = "bold")
#     )
# }

plot_pct_yes_summary <- function(summary_df, title, subtitle, ylab,
                                 size_col = NULL, pop_col = NULL,
                                 lim_min = 0.65, lim_max = 0.9) {
  # Map group_var to nicer labels
  group_var_labels <- c(
    age = "Age",
    race = "Race/Ethnicity",
    gender = "Gender",
    education = "Education",
    ideology = "Ideology",
    partyregaff = "Party Affiliation",
    otehistory = "2020 Vote Choice"
  )
  
  p <- summary_df %>%
    mutate(
      group_var_label = group_var_labels[group_var],
      group_level = paste0(group_level, " (n = ", formatC(n, big.mark = ","), ")"),
      n_resp = paste0("(n = ", formatC(n, big.mark = ","), ")")
    ) %>%
    mutate(group_level = fct_rev(fct_inorder(group_level))) %>%
    
    ggplot(aes(x = group_level, y = pct_yes, group = group_var_label)) +
    
    # Main plot layers
    geom_line(aes(color = group_var_label), linewidth = 1)
  if (is.null(size_col))
    p <- p + geom_point(aes(color = group_var_label), size = 8, fill = "white", shape = 21)
  else 
    p <- p + geom_point(aes(color = group_var_label, size = !!sym(size_col)), fill = "white", shape = 21)
  
  p <- p + 
    geom_text(aes(label = scales::percent(pct_yes, accuracy = 1)),
              vjust = 0.5, hjust = 0.3, size = 4, fontface = "bold")
    # geom_text(aes(label = group_level), hjust = 1.35, size = 3.5) +
    # geom_text(aes(label = n_resp), hjust = -0.35, size = 3.5)
    
  if (!is.null(pop_col)) {
    p <- p + 
      geom_line(aes(y = !!sym(pop_col)), color = "black", linewidth = 1) +
      geom_point(aes(y = !!sym(pop_col), color = group_var_label), size = 8, fill = "white", shape = 21) +
      geom_text(aes(label = scales::percent(!!sym(pop_col), accuracy = 1)),
                vjust = 0.5, hjust = 0.3, size = 4, fontface = "bold")
  }
  
    # Custom strip labels
  p <- p + 
    geom_text(
      data = . %>% distinct(group_var, group_var_label),
      aes(x = Inf, y = lim_min, label = paste("By", group_var_label)),
      inherit.aes = FALSE,
      hjust = 0, vjust = -0.2,
      fontface = "bold",
      size = 5
    ) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(lim_min, lim_max)) +
    coord_flip(clip = "off") +
    facet_grid(group_var_label ~ ., scales = "free_y", space = "free_y") +
    
    labs(
      x = NULL,
      y = ylab,
      title = title,
      subtitle = paste0(subtitle, "\n"),
      caption = "Data: Middle West Review (2023)\nAuthor: Soubhik Barari"
    ) +
    theme_minimal(base_family = "Helvetica") +
    theme(
      strip.text.y = element_blank(),
      # panel.grid = element_blank(),
      # axis.text.y = element_blank(),
      strip.background = element_blank(),
      panel.spacing.y = unit(1, "lines"),
      text = element_text(family = "Helvetica"),
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "none"
    )
  return(p)
}

# ---- Data ----

midwest_svy <- readRDS("data/midwest_svy.rds")

# ---- Plots ----

group_var_labels <- c(
  age = "Age",
  race = "Race/Ethnicity",
  gender = "Gender",
  education = "Education",
  ideology = "Ideology",
  partyregaff = "Party Registration",
  otehistory = "2020 Vote Choice"
)

## Identification 
id_summary_df <- midwest_svy %>%
  mutate(education = labelled::to_character(education)) %>%
  mutate(education = case_when(
    grepl("Vocational", education) ~ "Some college",
    grepl("Associate Degree", education) ~ "Some college",
    TRUE ~ education
  )) %>%
  filter(labelled::unlabelled(state) %in% MIDWEST_STATES) %>%
  summarize_pct_yes_by_groups(
    outcome_var = "midwesterner",
    group_vars = c("age", "race", "gender", "education", "ideology", "partyregaff", "otehistory")
  )

p_id <- id_summary_df %>%
  mutate(group_level = fct_rev(fct_inorder(group_level))) %>%
  plot_pct_yes_summary(title = "Who Identifies as a Midwesterner?",
                       ylab = "% (population-weighted) who answer 'Yes' to\n'Do you consider yourself to be a Midwesterner?'",
                       lim_min = 0.65, lim_max = 0.9,
                       subtitle = glue::glue(
                         "Among respondents from {length(MIDWEST_STATES)} Census-designated Midwest states:\n{str_wrap(MIDWEST_STATES_LABEL, width=70, whitespace_only = T)}"
                       ))
p_id
fp <- paste0("plots/demos_id.png")
ggsave(fp, p_id, width = 7, height = 10)
system(paste("open", fp))

## Live
live_summary_df <- midwest_svy %>%
  mutate(education = labelled::to_character(education)) %>%
  mutate(education = case_when(
    grepl("Vocational", education) ~ "Some college",
    grepl("Associate Degree", education) ~ "Some college",
    TRUE ~ education
  )) %>%
  filter(labelled::unlabelled(state) %in% MIDWEST_STATES) %>%
  summarize_pct_yes_by_groups(
    outcome_var = "midwest",
    group_vars = c("age", "race", "gender", "education", "ideology", "partyregaff", "otehistory")
  )

p_live <- live_summary_df %>%
  mutate(group_level = fct_rev(fct_inorder(group_level))) %>%
  plot_pct_yes_summary(title = "Which Midwesterners Say They\nActually Live in the Midwest",
                       ylab = "% (population-weighted) who answer 'Yes' to\n'Do you consider yourself to currently live in the Midwest?'",
                       lim_min = 0.75, lim_max = 0.95,
                       subtitle = glue::glue(
                         "Among respondents from {length(MIDWEST_STATES)} Census-designated Midwest states:\n{str_wrap(MIDWEST_STATES_LABEL, width=70, whitespace_only = T)}"
                       ))
p_live
fp <- paste0("plots/demos_live.png")
ggsave(fp, p_live, width = 7, height = 10)
system(paste("open", fp))

## Merge with Population Size
midwest_popdata_harmonized <- midwest_popdata %>%
  mutate(val = case_when(
    var == "otehistory" & val == "biden_pct" ~ "Joe Biden",
    var == "otehistory" & val == "trump_pct" ~ "Donald Trump",
    var == "otehistory" & val == "other_pct" ~ "Someone else",
    var == "age" & val == "18 to 24 years" ~ "18-24",
    var == "age" & val == "25 to 29 years" ~ "25-29",
    var == "age" & val == "30 to 39 years" ~ "30-39",
    var == "age" & val == "40 to 49 years" ~ "40-49",
    var == "age" & val == "50 to 59 years" ~ "50-59",
    var == "age" & val == "60 to 69 years" ~ "60-69",
    var == "age" & val == "70 years and over" ~ "70 or more years",
    var == "education" & val == "bachelorsE" ~ "College graduate",
    var == "education" & val == "graduateE" ~ "Postgraduate or higher",
    var == "education" & val == "hs_gradE" ~ "High school or less",
    var == "education" & val == "less_than_hsE" ~ "High school or less",
    var == "education" & val == "some_collegeE" ~ "Some college",
    var == "gender" & val == "female_totalE" ~ "Female",
    var == "gender" & val == "male_totalE" ~ "Male",
    var == "race" & val == "asianE" ~ "Asian",
    var == "race" & val == "blackE" ~ "Black or African American",
    var == "race" & val == "hispanicE" ~ "Hispanic or Latino of any race",
    var == "race" & val == "whiteE" ~ "White or Caucasian",
    var == "race" & val == "otherE" ~ "Other or multiple races",
    TRUE ~ val
  ))

# % national registered to vote in 2020 (in Midwest)
# https://www.kff.org/state-health-policy-data/state-indicator/number-of-voters-and-voter-registration-in-thousands-as-a-share-of-the-voter-population/?currentTimeframe=2&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
pct_eligible_us_adult_reg_vote_2020 <- 0.744
pct_vote_as_share_of_voters_2020 <- 0.684

library(ggrepel)

p_sizes <- id_summary_df %>% #as.data.frame()
  left_join(midwest_popdata_harmonized,
            by = c("group_var"="var", "group_level"="val")) %>% 
  mutate(pct_pop = case_when(
    grepl("Not registered", group_level) ~ pct_eligible_us_adult_reg_vote_2020, 
    grepl("Did not vote", group_level) ~ pct_vote_as_share_of_voters_2020,
    TRUE ~ pct_pop
  )) %>%
  filter(!grepl("Not registered", group_level), !grepl("Did not vote", group_level)) %>%
  ggplot(aes(x=pct_pop, y=pct_yes)) + 
  geom_smooth(method ="lm") +
  geom_point() +
  geom_text_repel(aes(label=group_level), size=3) +
  scale_x_continuous(name="% of the Population in Midwest States", labels=scales::percent_format(1)) +
  scale_y_continuous(name="% who Identify as Midwestern (Population-Weighted)", labels=scales::percent_format(1)) + 
  labs(title="Midwestern Identity Increases by Group Size",
       caption="Data: Middle West Review (2023), ACS (2020), CES (2020), KFF (2020)\nAuthor: Soubhik Barari") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"))
p_sizes
fp <- paste0("plots/demos_size.png")
ggsave(fp, p_sizes, width = 5, height = 6)
system(paste("open", fp))




p_sizes2 <- id_summary_df %>% #as.data.frame()
  left_join(midwest_popdata_harmonized,
            by = c("group_var"="var", "group_level"="val")) %>% 
  mutate(pct_pop = case_when(
    grepl("Not registered", group_level) ~ pct_eligible_us_adult_reg_vote_2020, 
    grepl("Did not vote", group_level) ~ pct_vote_as_share_of_voters_2020,
    TRUE ~ pct_pop
  )) %>%
  mutate(non_civic = grepl("Not registered", group_level) | grepl("Did not vote", group_level)) %>%
  ggplot(aes(x=pct_pop, y=pct_yes)) + 
  geom_smooth() +
  geom_point(aes(color=if_else(non_civic, "red", "black"))) +
  geom_text_repel(aes(label=group_level, color=if_else(non_civic, "red", "black"),
                      size=if_else(non_civic, 4, 3))) +
  scale_color_identity() +
  scale_size_identity() +
  scale_x_continuous(name="% of the Population in Midwest States", labels=scales::percent_format(1)) +
  scale_y_continuous(name="% who Identify as Midwestern (Population-Weighted)", labels=scales::percent_format(1)) + 
  labs(title="Midwestern Identity Increases by Group Size",
       subtitle="But but breaks down among the civically detached.",
       caption="Data: Middle West Review (2023), ACS (2020), CES (2020), KFF (2020)\nAuthor: Soubhik Barari") +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none")
p_sizes2
fp <- paste0("plots/demos_size2.png")
ggsave(fp, p_sizes2, width = 5, height = 6)
system(paste("open", fp))
