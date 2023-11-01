# Visualize state legislative margins and outcomes (Kousser and Gamm 2021)
#
# Paper:
# https://www.cambridge.org/core/journals/american-political-science-review/article/life-literacy-and-the-pursuit-of-prosperity-party-competition-and-policy-outcomes-in-50-states/4DD3750D110D228E18ABDBD9F30E089C

# Preamble ----------------------------------------------------------------

library(dataverse)
library(tidyverse)
library(geofacet)
library(ggrepel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

YEAR_START = 1910
STATE_EX = "AL"

readRenviron("../.Renviron")

theme_blog <- theme(legend.position = "top",
                    legend.title = element_text(size = 20),
                    legend.text = element_text(size = 18),
                    axis.text = element_text(size = 18),
                    axis.title = element_text(size = 20),
                    plot.title = element_text(size = 28, face = "bold"),
                    strip.text = element_text(colour="white"),
                    strip.background = element_rect(fill="black"))

# Data --------------------------------------------------------------------

if (!file.exists("data/kousser_gamm_21.rds")) {
  kousser_gamm_21 <- dataverse::get_dataframe_by_name("PoliticsProsperityJune2021.tab",
                                                      dataset = "10.7910/DVN/1HOKCH",
                                                      .f = readr::read_tsv)
  saveRDS(kousser_gamm_21, file = "kousser_gamm_21.rds")
} else {
  kousser_gamm_21 <- readRDS("data/kousser_gamm_21.rds")
}

unique(kousser_gamm_21$year)

# Legislative competition over time ---------------------------------------

kousser_gamm_21 %>%
  mutate(st = state.abb[match(state, state.name)],
         leg_margin = leg_margin/100) %>%
  filter(between(year, 1910, 2020), !is.na(st)) %>%
  select(st, state, year, leg_margin, leg_d_pct) %>%
  ggplot(aes(x=year, y=leg_margin)) +
  geom_line(aes(colour=leg_d_pct), size=2) +
  scale_color_gradient(low="red", high="blue", name = "% of Democrats in Legislature:") +
  scale_x_continuous(breaks=c(1920, 1960, 2010), name = "") +
  scale_y_continuous(labels = scales::percent_format(1), 
                     breaks = c(0.1, 0.5, 0.9),
                     name = "Legislative margin of majority (as a %)") +
  #facet_wrap(~ state, ncol = 5) +
  facet_geo(~ st, grid = "us_state_grid2") +
  ggtitle("State legislatures in the U.S. have gotten more\ncompetitive in the last 100 years") +
  labs(caption="Source: Kousser and Gamm (2021)\nAuthor: Soubhik Barari") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 28, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(face="bold", size=12, color="white"))
ggsave("figures/kousser_gamm_21_map.jpg", height=10, width=10)
system("open figures/kousser_gamm_21_map.jpg")

# Legislative competition vs. a few example issues ------------------------

kousser_gamm_21 %>% 
  filter(year >= YEAR_START) %>%
  mutate(st = state.abb[match(state, state.name)],
         leg_margin = leg_margin/100) %>%
  mutate(leg_d = ifelse(leg_d_pct > 50, "Dem.", "Rep.")) %>%
  filter(!is.na(leg_d)) %>%  
  ggplot(aes(x=leg_margin, y=infantmortality, group=leg_d, fill=leg_d, color=leg_d)) + 
  geom_point(aes(size=totalpopulation), shape=21, alpha=0.5) +
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label=ifelse(st == "WI", paste(st, "in", year), NA)), 
                  size=5, box.padding=2, color = "white", max.overlaps=10, segment.color="black") +
  scale_y_continuous(trans = "log10", name="Infant mortality (deaths per live births)") +
  scale_x_continuous(label = scales::percent_format(1), name="Legislative margin in state-year") +    
  scale_color_manual(values=c("blue","red"), name="Majority party in state legislature on year:") +
  scale_fill_manual(values=c("blue","red"), name="Majority party in state legislature on year:") +
  guides(size="none", fill="none") +
  labs(caption="Source: Kousser and Gamm (2021)\nAuthor: Soubhik Barari") +
  theme_bw() +
  ggtitle("More competitive state legislatures have\nhad lower infant mortality rates") +
  theme_blog
ggsave("figures/kousser_gamm_21_infant.jpg", height=9, width=9)
system("open figures/kousser_gamm_21_infant.jpg")

kousser_gamm_21 %>% 
  filter(year >= YEAR_START) %>%
  mutate(st = state.abb[match(state, state.name)],
         leg_margin = leg_margin/100) %>%
  mutate(leg_d = ifelse(leg_d_pct > 50, "Dem.", "Rep.")) %>%
  filter(!is.na(leg_d)) %>%
  ggplot(aes(x=leg_margin, y=at_birth_life_expectancy, group=leg_d, fill=leg_d, color=leg_d)) + 
  geom_point(aes(size=totalpopulation), shape=21, alpha=0.5) +
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label=ifelse(st == "SC", paste(st, "in", year), NA)), 
                   size=5, box.padding=2, color = "white", max.overlaps=10, segment.color="black") +
  scale_y_continuous(trans = "log10", name="Life expectancy (at birth)") +
  scale_x_continuous(label = scales::percent_format(1), name="Legislative margin in state-year") +  
  scale_color_manual(values=c("blue","red"), name="Majority party in state legislature on year:") +
  scale_fill_manual(values=c("blue","red"), name="Majority party in state legislature on year:") +
  guides(size="none", fill="none") +
  labs(caption="Source: Kousser and Gamm (2021)\nAuthor: Soubhik Barari") +
  theme_bw() +
  ggtitle("More competitive state legislatures have\nhigher life expectancies") +
  theme_blog
ggsave("figures/kousser_gamm_21_life.jpg", height=9, width=9)
system("open figures/kousser_gamm_21_life.jpg")

# Legislative competition vs. a bunch of issues ---------------------------

kousser_gamm_21 %>% 
  filter(year >= YEAR_START) %>%
  mutate(st = state.abb[match(state, state.name)],
         leg_margin = leg_margin/100,
         leg_d = ifelse(leg_d_pct > 50, "Dem.", "Rep.")) %>%
  filter(!is.na(leg_d)) %>%  
  select(st, year, leg_margin, leg_d, 
         `Per Capita Income`=per_capita_income,
         `Education Spending (per capita)`=Education_pc, 
         `Health/Sanitation Spending`=HealthSewerSanitation_pc, 
         `Transportation Spending`=Transportation_pc,
         `life Service Spending`=lifeService_pc, 
         `Public Safety Spending`=PublicSafety_pc, 
         `Illiteracy`=illiteracy,
         `Infant Mortality`=infantmortality, 
         totalpopulation) %>%
  gather(key="area", value="val", -st, -year, -leg_margin, -leg_d, -totalpopulation) %>%
  mutate(label = ifelse(st == "WI", paste(st, "in", year), NA)) %>%
  # mutate(val = ifelse(grepl("(_pc|income|infant)", area), log(val), val)) %>%
  ggplot(aes(x=leg_margin, y=val, group=leg_d, fill=leg_d, color=leg_d)) + 
  geom_point(aes(size=totalpopulation), shape=21, alpha=0.5) +
  geom_smooth(method = "lm") +
  geom_label_repel(aes(label=label), 
                   size=5, 
                   max.overlaps=10, 
                   box.padding=0.5,
                   segment.size=0.75,
                   segment.color="black",
                   color="white") +
  facet_wrap(~ area, scales = "free", ncol=2) +
  scale_y_continuous(trans = "log10", name="") +
  scale_x_continuous(label = scales::percent_format(1), name="Legislative margin in state-year") +
  scale_color_manual(values=c("blue","red"), name="Majority party in state legislature on year:") +
  scale_fill_manual(values=c("blue","red"), name="Majority party in state legislature on year:") +
  guides(size="none", fill="none") +
  labs(caption="Source: Kousser and Gamm (2021)\nAuthor: Soubhik Barari") +
  theme_bw() +
  ggtitle("More competitive state legislatures spend more on\nhuman capital and have better policy outcomes") +
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),        
        plot.title = element_text(size = 25, face = "bold"),
        strip.text = element_text(size=16, colour="white", face="bold"),
        strip.background = element_rect(fill="black"))
ggsave("figures/kousser_gamm_21_grid.jpg", height=14, width=9.5)
system("open figures/kousser_gamm_21_grid.jpg")

# Electoral competition vs. polarization ----------------------------------

# dataverse::dataset_files(dataset = "10.7910/DVN/Z7H44N")

load("data/nrstw_17_heter_dists/merged public and legislators.RData") # 1993 - 2015
table(legis.m$s$year)
table(house.m.all$year)
table(senate.m.all$year)

load("data/nrstw_17_heter_dists/state polarization.RData")
sd.st
st.data

#TODO

