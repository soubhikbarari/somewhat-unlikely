
# Preamble ----------------------------------------------------------------

library(tidyverse)
options(readr.show_col_types = FALSE)

# Read data ---------------------------------------------------------------

# Load and label each file
read_cfb <- function(path, candidate_name) {
  read_csv(path) %>%
    transmute(candidate = candidate_name,
              employer = EMPNAME,
              occupation = OCCUPATION,
              amount = AMNT)
}

# Replace with your actual file paths
adams <- read_cfb("data/adams_CFB.csv", "Adrienne Adams")
cuomo <- read_cfb("data/cuomo_CFB.csv", "Andrew Cuomo")
lander <- read_cfb("data/lander_CFB.csv", "Brad Lander")
mamdani <- read_cfb("data/mamdani_CFB.csv", "Zohran Mamdani")

# Combine all
all_donations <- bind_rows(adams, cuomo, lander, mamdani)

# Keywords ----------------------------------------------------------------

# Function to classify by keywords
classify_industry <- function(occ, emp) {
  text <- paste(toupper(occ), toupper(emp))
  
  case_when(
    str_detect(text, "LAW|ATTORNEY|LEGAL|LAWYER|FIRM") ~ "Legal",
    str_detect(text, "REAL ESTATE|REALTOR|DEVELOPER|PROPERTY") ~ "Real Estate",
    str_detect(text, "BANK|FINANCE|INVEST|CPA|ACCOUNTANT|WALL STREET") ~ "Finance",
    str_detect(text, "TEACH|SCHOOL|PROFESSOR|EDUCATION|UNIVERSITY|CUNY|NYU") ~ "Education",
    str_detect(text, "DOCTOR|NURSE|MEDICAL|HOSPITAL|HEALTH|PHARM") ~ "Healthcare",
    str_detect(text, "UNION|SEIU|UFT|IBEW|TEAMSTERS") ~ "Labor/Union",
    str_detect(text, "TECH|SOFTWARE|ENGINEER|IT|DATA|CYBER") ~ "Technology",
    str_detect(text, "RETIRED") ~ "Retired",
    str_detect(text, "NOT EMPLOYED|UNEMPLOYED|NONE") ~ "Unemployed",
    str_detect(text, "GOVERNMENT|CITY|STATE|PUBLIC|MAYOR|SENATOR") ~ "Government",
    str_detect(text, "CONSULT|ADVISOR") ~ "Consulting",
    str_detect(text, "NONPROFIT|ADVOCACY|ORGANIZER|COMMUNITY") ~ "Nonprofit/Advocacy",
    str_detect(text, "ARTIST|WRITER|MUSIC|FILM|CREATIVE") ~ "Arts & Media",
    str_detect(text, "BUSINESS|OWNER|ENTREPRENEUR|CEO|EXECUTIVE") ~ "Business",
    TRUE ~ "Other/Unclassified"
  )
}

all_donations <- all_donations %>%
  mutate(
    industry = classify_industry(occupation, employer),
    donor_size = case_when(
      amount < 100 ~ "< $100",
      amount > 1000 ~ "> $1000",
      TRUE ~ "$100–$1000"
    )
  )

# Summarise ---------------------------------------------------------------

industry_summary <- all_donations %>%
  group_by(candidate, industry) %>%
  summarise(
    total_amount = sum(amount, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>% 
  arrange(candidate, desc(total_amount))
industry_summary

# Visualise ---------------------------------------------------------------

candidate_levels <- c("Andrew Cuomo", "Zohran Mamdani", "Brad Lander", "Adrienne Adams")

## Occupations ------------------------------------------------------------

library(tidyverse)
library(wordcloud)
library(RColorBrewer)
library(stringr)

# Preprocess and count words in occupation
occupation_counts <- all_donations %>%
  filter(!is.na(occupation)) %>%
  mutate(occupation = tolower(occupation) |> str_replace_all("not employed", "unemployed")) %>%
  separate_rows(occupation, sep = "[^a-zA-Z]+") %>%
  filter(occupation != "unemployed") %>%
  filter(str_length(occupation) > 2) %>%  # remove short/common words like "at", "in"
  count(occupation, sort = TRUE)

wordcloud(words = occupation_counts$occupation,
          freq = occupation_counts$n,
          min.freq = 5,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

candidate_occupations <- all_donations %>%
  filter(!is.na(occupation)) %>%
  mutate(occupation = tolower(occupation) |> str_replace_all("not employed", "unemployed")) %>%
  separate_rows(occupation, sep = "[^a-zA-Z]+") %>%
  filter(str_length(occupation) > 3) %>%
  filter(occupation != "unemployed") %>%
  count(candidate, occupation, sort = TRUE)

# Plot one word cloud per candidate using base R layout
candidates <- unique(candidate_occupations$candidate)

par(mfrow = c(2, 2))  # adjust for number of candidates
for (cand in candidates) {
  dat <- candidate_occupations %>% filter(candidate == cand)
  wordcloud(words = dat$occupation, freq = dat$n, 
            scale = c(3, 0.5), max.words = 100,
            colors = brewer.pal(8, "Dark2"), random.order = FALSE)
  title(cand)
}

# Employers ---------------------------------------------------------------

candidate_employers <- all_donations %>%
  filter(!is.na(employer)) %>%
  mutate(employer = tolower(employer) |> str_replace_all("(nyu|new york university)", "NYU")) %>%
  separate_rows(employer, sep = "[^a-zA-Z]+") %>%
  filter(str_length(employer) >= 3) %>%
  filter(!(employer %in% c("self-employed", "employed", "self employed", "self",
                           "new", "york", "not", "nyc", "the", "inc", "llc", "for"))) %>%
  count(candidate, employer, sort = TRUE)

# Plot one word cloud per candidate using base R layout
candidates <- unique(candidate_employers$candidate)
par(mfrow = c(2, 2))  # adjust for number of candidates
for (cand in candidates) {
  dat <- candidate_employers %>% filter(candidate == cand)
  wordcloud(words = dat$employer, freq = dat$n, 
            scale = c(3, 0.5), max.words = 100,
            colors = brewer.pal(8, "Dark2"), random.order = FALSE)
  title(cand)
}

## Industry ---------------------------------------------------------------

industry_summary %>%
  mutate(candidate = factor(candidate, levels = candidate_levels)) %>%
  filter(!grepl("(Unclassified|Other)", industry)) %>%
  ggplot(aes(x = reorder(industry, total_amount), y = total_amount, fill = candidate)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Donation Totals by Industry and Candidate",
    x = "Industry", y = "Total Donation Amount ($)"
  ) +
  scale_fill_viridis_d(name = "Candidate:") +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ",", prefix = "$")) +
  theme_bw() +
  labs(caption = "Source: NYC Campaign Finance Board\nAuthor: Soubhik Barari") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14))
ggsave("plots/don_ind.png", height=7, width=8)

industry_summary %>%
  filter(!grepl("(Unclassified|Other)", industry)) %>%
  mutate(candidate = factor(candidate, levels = candidate_levels)) %>%
  ggplot(aes(x = reorder(industry, total_amount), y = total_amount, fill = candidate)) +
  geom_col(position = "dodge") +
  facet_grid(candidate ~ .) +
  labs(
    title = "Donations to NYC Mayoral Candidates by Industry",
    x = "Industry", y = "Total Donation Amount"
  ) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ",", prefix = "$")) +
  scale_fill_viridis_d(name = "Candidate:") +
  theme_bw() +
  labs(caption = "Source: NYC Campaign Finance Board\nAuthor: Soubhik Barari") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
ggsave("plots/don_totl.png", height=8, width=6.8)

industry_summary %>%
  filter(!grepl("(Unclassified|Other)", industry)) %>%
  mutate(candidate = factor(candidate, levels = candidate_levels)) %>%
  ggplot(aes(x = reorder(industry, n), y = n, fill = candidate)) +
  geom_col(position = "dodge") +
  facet_grid(candidate ~ .) +
  labs(
    title = "Number of Donors for NYC Mayoral Candidates by Industry",
    x = "Industry", y = "Total Number of Donors"
  ) +
  scale_y_continuous() +
  scale_fill_viridis_d(name = "Candidate:") +
  labs(caption = "Source: NYC Campaign Finance Board\nAuthor: Soubhik Barari") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
ggsave("plots/don_num.png", height=8, width=6.8)

industry_summary %>%
  filter(!grepl("(Unclassified|Other)", industry)) %>%
  mutate(candidate = factor(candidate, levels = candidate_levels)) %>%
  ggplot(aes(x = reorder(industry, total_amount/n), y = total_amount/n, fill = candidate)) +
  geom_col(position = "dodge") +
  facet_grid(candidate ~ .) +
  labs(
    title = "Average Donation Amount for NYC Mayoral Candidates by Industry",
    x = "Industry", y = "Average Donation Amount"
  ) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ",", prefix = "$")) +
  scale_fill_viridis_d(name = "Candidate:") +
  theme_bw() +
  labs(caption = "Source: NYC Campaign Finance Board\nAuthor: Soubhik Barari") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
ggsave("plots/don_avg.png", height=8, width=6.8)


## Donation Size ----------------------------------------------------------

donor_split <- all_donations %>%
  group_by(candidate, donor_size) %>%
  summarise(total = sum(amount, na.rm = TRUE), 
            count = n(), .groups = "drop") %>%
  group_by(candidate) %>%
  mutate(pct_total = count/sum(count))

donor_split$candidate <- factor(donor_split$candidate, levels = candidate_levels)
donor_split$donor_size <- factor(donor_split$donor_size, levels = c("< $100", "$100–$1000", "> $1000"))

ggplot(donor_split, aes(x = candidate, y = pct_total, fill = donor_size)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Small vs. Big Donors to NYC Mayoral Candidates",
    y = "Share of Total Donations", x = NULL
  ) +
  labs(caption = "Source: NYC Campaign Finance Board\nAuthor: Soubhik Barari") +
  scale_fill_brewer(palette = "Set2", name = "Donation:") +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )
