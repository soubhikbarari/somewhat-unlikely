# Visualize historical Congressional margins

library(tidyverse)
library(ggrepel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data --------------------------------------------------------------------

terms <- read_csv("../data/congress/congress_terms.csv")
control <- read_csv("../data/congress/us_government_control_1857-2016.csv")
legis_curr <- read_csv("../data/congress/legislators-current-terms.csv")
legis_past <- read_csv("../data/congress/legislators-historical-terms.csv")
house_elections <- read_csv("../data/medsl/1976-2022-house.csv")
senate_elections <- read_csv("../data/medsl/1976-2020-senate.csv")

# House margins -----------------------------------------------------------

if (!"house_margins" %in% ls()) {
  house_margins <- house_elections %>%
    filter(stage == "GEN", !(runoff %in% TRUE), !(special %in% TRUE)) %>%
    select(year, state, district, party, candidate, candidatevotes, totalvotes) %>%
    mutate(st=state.abb[match(tolower(state), tolower(state.name))],
           district=as.numeric(district)) %>%
    group_by(year, st, district) %>% 
    filter(n() == 2 & (any(grepl("DEM", party)) & any(grepl("REP", party)))) %>%
    mutate(candidatevotes=candidatevotes/totalvotes) %>%
    summarise(win_margin=max(candidatevotes) - min(candidatevotes), 
              dem_margin=candidatevotes[grepl("DEM", party)] - candidatevotes[grepl("REP", party)],
              .groups="drop")
}

house_margins_year <- house_margins %>%
  group_by(election_year=year) %>%
  summarise(avg_win_margin=mean(win_margin), .groups="drop") %>%
  inner_join(control %>%
              mutate(election_year=year_start - 1) %>%
              select(election_year, house_reps, house_dems) %>%
              arrange(-election_year) %>%
              bind_rows(., 
                data.frame(election_year=c(2016,2018,2020,2022),
                           house_reps=c(236,195,213,221),
                           house_dems=c(196,233,216,212))
              ) %>% 
               mutate(avg_maj_margin=abs(house_reps-house_dems)/435))

ggplot(house_margins_year, aes(x=avg_win_margin, y=avg_maj_margin)) +
  geom_point() + geom_smooth()

cor(house_margins_year$avg_win_margin, lead(house_margins_year$avg_maj_margin), 
    use="pairwise.complete.obs") #0.28

house_margins_year %>%
  ggplot(aes(x=election_year)) + 
  
  geom_point(data=house_margins,
             aes(x=year, y=win_margin), color="white", fill="purple", 
             shape=21, size=4, alpha=0.05) +
  
  geom_line(aes(y=avg_win_margin), color="purple", size=2) +
  
  geom_ribbon(data=house_margins %>%
                group_by(year) %>%
                summarise(upr=quantile(win_margin, 0.75),
                          lwr=quantile(win_margin, 0.25)),
              aes(x=year, ymin=lwr, ymax=upr), fill="purple", 
              shape=21, size=4, alpha=0.5) +  
  
  #geom_point(aes(y=avg_win_margin), color="white", fill="purple", shape=21, size=4) +
  geom_text(data=data.frame(x=1984, y=0.6, label="25-50th percentiles of\nwin margins in\nHouse election"),
                  aes(x=x, y=y, label=label), colour="purple") +
  
  geom_line(aes(y=avg_maj_margin), color="brown", size=2) +
  geom_point(aes(y=avg_maj_margin), color="white", fill="brown", shape=21, size=4) +
  geom_text(data=data.frame(x=1988, y=0.1, label="Margin of\nresulting House majority"),
                  aes(x=x, y=y, label=label), colour="brown") +
  
  geom_text_repel(data=data.frame(x=2010, y=0.5),
                  aes(x=x, y=y),  
                  color = "purple", label = "Win margin in\nindividual race", 
                  box.padding = 0.9, nudge_x = .5, nudge_y = .2
  ) +
  
  scale_x_continuous(name="Election Year") +
  scale_y_continuous(labels=scales::percent_format(1), name="Margin") +
  labs(caption="Source: MIT Election Lab/Wikipedia\nAuthor: Soubhik Barari") + 
  ggtitle("Electoral competition has a large spread\nin the House") + 
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=24))
ggsave("figures/house_margins_year_all.jpg", height=5, width=8)
system("open figures/house_margins_year_all.jpg")

# Senate margins ----------------------------------------------------------

if (!"senate_margins" %in% ls()) {
  senate_margins <- senate_elections %>%
    filter(stage == "gen", special == FALSE) %>%
    mutate(st=state.abb[match(tolower(state), tolower(state.name))]) %>%
    select(year, st, party=party_simplified, candidate, candidatevotes, totalvotes) %>%
    group_by(year, st) %>% 
    filter(grepl("DEM", party) | grepl("REP", party)) %>%
    filter(n() == 2) %>%
    mutate(candidatevotes=candidatevotes/totalvotes) %>%
    summarise(win_margin=max(candidatevotes) - min(candidatevotes), 
              dem_margin=candidatevotes[grepl("DEM", party)] - candidatevotes[grepl("REP", party)],
              .groups="drop")
}

senate_margins_year <- senate_margins %>%
  group_by(election_year=year) %>%
  summarise(avg_win_margin=mean(win_margin), .groups="drop") %>%
  inner_join(control %>%
               mutate(election_year=year_start - 1) %>%
               select(election_year, senate_reps, senate_dems) %>%
               arrange(-election_year) %>%
               bind_rows(.,
                 data.frame(election_year=c(2016,2018,2020,2022),
                            senate_reps=c(51,53,50,49),
                            senate_dems=c(47,45,48,48))
               ) %>% 
               mutate(avg_maj_margin=abs(senate_reps-senate_dems)/435))

ggplot(senate_margins_year, aes(x=avg_win_margin, y=avg_maj_margin)) +
  geom_point() + geom_smooth()

cor(senate_margins_year$avg_win_margin, senate_margins_year$avg_maj_margin,
    use="pairwise.complete.obs") #-0.21

ggplot(senate_margins_year, 
       aes(x=election_year)) + 
  
  geom_line(aes(y=avg_win_margin), color="purple", size=2) +
  geom_point(aes(y=avg_win_margin), color="white", fill="purple", shape=21, size=4) +
  geom_text(data=data.frame(x=2010, y=0.34, label="Average win margin in\nSenate election"),
            aes(x=x, y=y, label=label), colour="purple") +
  
  geom_line(aes(y=avg_maj_margin), color="brown", size=2) +
  geom_point(aes(y=avg_maj_margin), color="white", fill="brown", shape=21, size=4) +
  geom_text(data=data.frame(x=1987, y=0.08, label="Margin of\nresulting Senate majority"),
            aes(x=x, y=y, label=label), colour="brown") +
  
  scale_x_continuous(name="Election Year") +
  scale_y_continuous(labels=scales::percent_format(1), name="Margin") +
  ggtitle("Electoral and party competition in the Senate\n(kind of) track each other (on average)") + 
  labs(caption="Source: MIT Election Lab/Wikipedia\nAuthor: Soubhik Barari") + 
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=24))
ggsave("figures/senate_margins_year.jpg", height=5, width=8)
system("open figures/senate_margins_year.jpg")


ggplot(senate_margins_year, 
       aes(x=election_year)) + 
  
  # geom_point(data=senate_margins,
  #            aes(x=year, y=win_margin), color="white", fill="purple", 
  #            shape=21, size=4, alpha=0.1) +
  
  geom_line(aes(y=avg_win_margin), color="purple", size=2) +
  
  geom_ribbon(data=senate_margins %>%
                group_by(year) %>%
                summarise(upr=quantile(win_margin, 0.75),
                          lwr=quantile(win_margin, 0.25)),
              aes(x=year, ymin=lwr, ymax=upr), fill="purple", 
              shape=21, size=4, alpha=0.2) +
  
  geom_point(aes(y=avg_win_margin), color="white", fill="purple", shape=21, size=4) +
  geom_text(data=data.frame(x=2010, y=0.34, label="Average win margin in\nSenate election"),
            aes(x=x, y=y, label=label), colour="purple") +

  geom_line(aes(y=avg_maj_margin), color="brown", size=2) +
  geom_point(aes(y=avg_maj_margin), color="white", fill="brown", shape=21, size=4) +
  geom_text(data=data.frame(x=1987, y=0.08, label="Margin of\nresulting Senate majority"),
            aes(x=x, y=y, label=label), colour="brown") +
  
  scale_x_continuous(name="Election Year") +
  scale_y_continuous(labels=scales::percent_format(1), name="Margin") +
  ggtitle("Electoral competition has a large spread\nin the Senate") + 
  labs(caption="Source: MIT Election Lab/Wikipedia\nAuthor: Soubhik Barari") + 
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size=24))
ggsave("figures/senate_margins_year_all.jpg", height=5, width=8)
system("open figures/senate_margins_year_all.jpg")
