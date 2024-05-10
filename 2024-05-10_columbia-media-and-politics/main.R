
library(ggplot2)
library(tidyverse)

# Media Cloud plot --------------------------------------------------------

read_csv("data/columbia_conflict_media_cloud.csv") %>%
  group_by(date) %>%
  mutate(stories = ifelse(row_number()==1, 
                          "Columbia University", 
                          "Israel-Hamas Conflict")) %>%
  ggplot(aes(x=date, y=count, color=stories, fill=stories)) +
  geom_point(color="white", shape=23) +
  geom_line(size=1.5) +
  annotate("text", label="Encampment Announced",
           x=as.POSIXct("2024-04-10"), y=1000, angle=90) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-04-14")),
             lwd=1, lty=3, color="black") +
  scale_x_datetime(labels = scales::date_format(format = "%b %d %Y"),
                   name = "") +
  scale_y_continuous(trans = scales::log10_trans(),
                     labels = scales::comma_format(),
                     name = "Number of stories in U.S. national news outlets") +
  scale_fill_manual(values = c("lightblue3", "pink2"),
                    name = "Stories:") +
  scale_color_manual(values = c("lightblue3", "pink2"),
                     name = "Stories:") +
  labs(title = "Media Coverage of Columbia and Israel-Hamas",
       caption = "Author: Soubhik Barari (2024)\nSource: Media Cloud\n\nStories about the Israel-Hamas conflict were identified by the search terms 'Israel', 'Gaza', 'Palestine', 'Hamas' excluding any\nmentions of 'Columbia'. Do not interpret this causally!") +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))
ggsave("figures/columbia_media_cloud.png", height=5, width=8)

# Google Trends plot ------------------------------------------------------

read_csv("data/columbia_conflict_google_trends.csv", skip = 2) %>%
  gather(key = "Searches", value = "Count", -Day) %>%
  mutate(Searches = gsub("\\: \\(.*", "", Searches)) %>%
  ggplot(aes(x=as.POSIXct(Day), y=Count, color=Searches, fill=Searches)) +
  geom_point(color="white", shape=23) +
  geom_line(size=1.5) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-04-14")),
             lwd=1, lty=3,
             color="black") +
  annotate("text", label="Encampment Announced",
           x=as.POSIXct("2024-04-10"), y=10, angle=90) +
  scale_x_datetime(labels = scales::date_format(format = "%b %d %Y"),
                   name = "") +
  scale_y_continuous(trans = scales::log10_trans(),
                     labels = scales::comma_format(),
                     name = "Normalized % of Google searches") +
  scale_fill_manual(values = c("lightblue3", "pink2"),
                    name = "Searches:") +
  scale_color_manual(values = c("lightblue3", "pink2"),
                     name = "Searches:") +
  facet_grid(Searches ~ .) +
  labs(title = "Online Search Attention to Columbia and the Israel-Palestinian Conflict",
       caption = "Author: Soubhik Barari (2024)\nSource: Google Trends\n\nNo Google Trends entity directly identified the on-going Israel-Hamas conflict. Do not interpret these results causally!") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size=12),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0))
ggsave("figures/columbia_google_trends.png", height=5.5, width=8)
