library(tidyverse)
library(ggplot2)

# Toggle to show/hide annotations and vertical lines
add_annotations <- FALSE

# Helper: annotate + vline if toggle is TRUE
add_event <- function(date, label, y, angle = 20, color = "gray30", lty = 2) {
  list(
    if (add_annotations) annotate("text", x = as.POSIXct(date), y = y, label = label, angle = angle, hjust = 0.5),
    if (add_annotations) geom_vline(xintercept = as.numeric(as.POSIXct(date)), lwd = 0.8, lty = lty, color = color)
  )
}

# National coverage -------------------------------------------------------
d <- bind_rows(
  read_csv("data/cuomo-natl.csv") %>% mutate(cand = "Andrew\nCuomo"),
  read_csv("data/mamdani-natl.csv") %>% mutate(cand = "Zohran\nMamdani"),
  read_csv("data/lander-natl.csv") %>% mutate(cand = "Brad\nLander"),
  read_csv("data/adams-natl.csv") %>% mutate(cand = "Adrienne\nAdams")
)

p_natl <- d %>%
  mutate(cand = as_factor(cand), date = as.POSIXct(date)) %>%
  ggplot(aes(x = date, y = count, color = cand, fill = cand)) +
  geom_point(color = "white", shape = 23) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Candidate:") +
  scale_fill_viridis_d(name = "Candidate:") +
  add_event("2025-03-05", "Adrienne Adams Launches\nCampaign", max(d$count) - 10, color = "gold2", lty = 1) +
  add_event("2025-03-01", "Cuomo Launches\nCampaign", 90) +
  add_event("2025-05-21", "DOJ Announces\nCuomo Investigation", 80) +
  add_event("2025-06-17", "Lander Arrested", max(d$count) - 15, color = "darkgreen", lty = 3) +
  add_event("2025-02-18", "Hochul Meets\nLocal Leaders", 70) +
  add_event("2025-06-04", "Debate 1", 60, color = "blue") +
  add_event("2025-06-12", "Debate 2", 60, color = "blue") +
  scale_x_datetime(labels = scales::date_format("%b %d, %Y"), name = "") +
  scale_y_continuous(labels = scales::comma_format(), name = "Stories in U.S. national news") +
  labs(title = "National Media Coverage of NYC Mayoral Candidates",
       caption = "Author: Soubhik Barari (2025)\nSource: Media Cloud Explorer") +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))

ggsave("plots/media_nyc_natl.png", plot = p_natl, height = 5, width = 8)


## Late national (June 22) ------------------------------------------------
d_late <- bind_rows(
  read_csv("data/cuomo-natl-late.csv") %>% mutate(cand = "Andrew\nCuomo"),
  read_csv("data/mamdani-natl-late.csv") %>% mutate(cand = "Zohran\nMamdani"),
  read_csv("data/lander-natl-late.csv") %>% mutate(cand = "Brad\nLander"),
  read_csv("data/adams-natl-late.csv") %>% mutate(cand = "Adrienne\nAdams")
)

p_natl_late <- d_late %>%
  mutate(cand = as_factor(cand), date = as.POSIXct(date)) %>%
  ggplot(aes(x = date, y = count, color = cand, fill = cand)) +
  geom_point(color = "white", shape = 23) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Candidate:") +
  scale_fill_viridis_d(name = "Candidate:") +
  scale_x_datetime(labels = scales::date_format("%b %d, %Y"), name = "") +
  scale_y_continuous(labels = scales::comma_format(), name = "Stories in U.S. national news") +
  labs(title = "National Media Coverage of NYC Mayoral Candidates",
       caption = "Author: Soubhik Barari (2025)\nSource: Media Cloud Explorer") +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))

ggsave("plots/media_nyc_natl_late.png", plot = p_natl, height = 5, width = 8)




# Local coverage ----------------------------------------------------------
d_local <- bind_rows(
  read_csv("data/cuomo-nyc.csv") %>% mutate(cand = "Andrew\nCuomo"),
  read_csv("data/mamdani-nyc.csv") %>% mutate(cand = "Zohran\nMamdani"),
  read_csv("data/lander-nyc.csv") %>% mutate(cand = "Brad\nLander"),
  read_csv("data/adams-nyc.csv") %>% mutate(cand = "Adrienne\nAdams")
)

p_local <- d_local %>%
  mutate(cand = as_factor(cand), date = as.POSIXct(date)) %>%
  ggplot(aes(x = date, y = count, color = cand, fill = cand)) +
  geom_point(color = "white", shape = 23) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Candidate:") +
  scale_fill_viridis_d(name = "Candidate:") +
  add_event("2025-03-05", "Adrienne Adams\nLaunches Campaign", max(d_local$count) - 10, color = "gold2", lty = 1) +
  add_event("2025-03-01", "Cuomo Launches\nCampaign", max(d_local$count) - 10) +
  add_event("2025-04-02", "Eric Adams\nAnnounces Independent\nRun", max(d_local$count) - 10) +
  add_event("2025-05-21", "DOJ Announces\nCuomo Investigation", max(d_local$count) - 15) +
  add_event("2025-06-17", "Lander Arrested", max(d_local$count) - 15, color = "darkgreen", lty = 3) +
  add_event("2025-02-18", "Hochul Meets\nLocal Leaders", max(d_local$count) - 20) +
  add_event("2025-06-04", "Debate 1", max(d_local$count) - 25, color = "blue") +
  add_event("2025-06-12", "Debate 2", max(d_local$count) - 25, color = "blue") +
  scale_x_datetime(labels = scales::date_format("%b %d, %Y"), name = "") +
  scale_y_continuous(labels = scales::comma_format(), name = "Stories in NY state/local news") +
  labs(title = "Local Media Coverage of NYC Mayoral Candidates",
       caption = "Author: Soubhik Barari (2025)\nSource: Media Cloud Explorer") +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))

ggsave("plots/media_nyc_local.png", plot = p_local, height = 5, width = 8)
