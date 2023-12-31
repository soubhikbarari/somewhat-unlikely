# Preamble ----------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(ggplot2)
library(ggthemes)
library(stringr)
library(rgdal)
library(rgeos)

data(fips_codes)

# Data --------------------------------------------------------------------

cnty_fpath_prefix <- "../data/medsl/County_Presidential_Election_Data_"

cnty_vote_df <- data.frame()
for (yyyy in c("1976", "1980", "2008", "2012", "2016", "2020")) {
  cnty_vote_yyyy_df <- paste0(cnty_fpath_prefix,yyyy) %>%
    list.files(path="data", .) %>%
    first() %>%
    file.path("data", .) %>%
    read_csv(show_col_types = FALSE) %>% 
    slice(-1) %>%
    pivot_longer(-c(1:4), names_to = "candidate", values_to = "vote") %>%
    rename_with(~gsub(" ","_",tolower(.x))) %>%
    mutate(year = yyyy,
           fips = sprintf("%05d", as.numeric(fips))) %>%
    mutate_at(c("vote", "total_vote", "year"), ~as.numeric(.x))
  cnty_vote_df <- bind_rows(cnty_vote_df, cnty_vote_yyyy_df)
}

# Plots -------------------------------------------------------------------

carter_reagan_df <- cnty_vote_df %>%
  filter(year %in% c(1976, 1980),
         geographic_subtype == "County",
         grepl("(Ford|Carter|Reagan)", candidate)) %>%
  mutate(candidate = str_extract(candidate, "(Ford|Carter|Reagan)"),
         vote = vote/total_vote) %>%
  pivot_wider(id_cols = -total_vote, 
              names_from = c("candidate","year"), 
              values_from = "vote") %>%
  mutate(D_R = (Carter_1976>Ford_1976) & (Reagan_1980>Carter_1980),
         D_D = (Carter_1976>Ford_1976) & (Reagan_1980<Carter_1980),
         R_D = (Carter_1976<Ford_1976) & (Reagan_1980<Carter_1980),
         R_R = (Carter_1976<Ford_1976) & (Reagan_1980>Carter_1980))

obama_romney_df <- cnty_vote_df %>%
  filter(year %in% c(2008, 2012),
         geographic_subtype == "County",
         grepl("(Obama|McCain|Romney)", candidate)) %>%
  mutate(candidate = str_extract(candidate, "(Obama|McCain|Romney)"),
         vote = vote/total_vote) %>%
  pivot_wider(id_cols = -total_vote, 
              names_from = c("candidate","year"), 
              values_from = "vote") %>%
  mutate(D_R = (Obama_2008>McCain_2008) & (Romney_2012>Obama_2012),
         D_D = (Obama_2008>McCain_2008) & (Romney_2012<Obama_2012),
         R_D = (Obama_2008<McCain_2008) & (Romney_2012<Obama_2012),
         R_R = (Obama_2008<McCain_2008) & (Romney_2012>Obama_2012))

make_corr_plot <- function(df, x, y) {
  corners <- data.frame(
    xpos = c(-Inf, -Inf, Inf, Inf), 
    ypos =  c(-Inf, Inf, -Inf, Inf), #left-bottom, left-top, right-bottom, right-top
    text = c(paste0(round(mean(df$R_R,na.rm=T)*100, 1),"% of counties\nstuck to the Republican"),
             paste0(round(mean(df$R_D,na.rm=T)*100, 1),"% of counties\nswitched from Rep. to Dem."),
             paste0(round(mean(df$D_R,na.rm=T)*100, 1),"% of counties\nswitched from Dem. to Rep."),
             paste0(round(mean(df$D_D,na.rm=T)*100, 1),"% of counties\nstuck to the Democrat")),
    # hjustvar = c(0,0,1,1), vjustvar = c(0,1,0,1))   #original placement in each corner
    hjustvar = c(-.25,   #shifts bottom left 'Text' to the right; make more negative to move it further right
                 -.25,   #shifts top left 'tExt' to the right; make more negative to move it further right
                 1.25,   #shifts bottom right 'teXt' to the left; make more positive to move it further left
                 1.25),  #shifts top right 'texT' to the left; make more positive to move it further left
    vjustvar = c(-0.75,    #shifts bottom left 'Text' upward; make more negative to move it further up
                 1.75,     #shifts top left 'tExt' downward; make more positive to move it further down
                 -0.75,    #shifts bottom right 'teXt' upward; make more negative to move it further up
                 1.75)     #shifts top right 'texT' downward; make more positive to move it further down
  )
  
  df %>%
    mutate(color = case_when(D_D       ~ "blue",
                             D_R | R_D ~ "purple",
                             R_R       ~ "red")) %>%
    ggplot(aes(x={{x}}, y={{y}})) +
    geom_vline(aes(xintercept=0.5), lty=2) +
    geom_hline(aes(yintercept=0.5), lty=2) +
    geom_point(aes(fill=color, color=color), alpha=0.5) +
    scale_color_identity() +
    scale_x_continuous(labels = scales::percent_format(1), limits=c(0, 1)) +
    scale_y_continuous(labels = scales::percent_format(1), limits=c(0, 1)) +
    geom_text(data = corners, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = text)) +
    # annotate("label", x=0.1, y=0.05, label=paste0(round(mean(df$R_R,na.rm=T)*100, 1),"% of counties\nstuck to the Republican"), hjust=0.5) +
    # annotate("label", x=0.1, y=1, label=paste0(round(mean(df$R_D,na.rm=T)*100, 1),"% of counties\nswitched from Rep. to Dem."), hjust=0.25) +
    # annotate("label", x=0.9, y=0.05, label=paste0(round(mean(df$D_R,na.rm=T)*100, 1),"% of counties\nswitched from Dem. to Rep."), hjust=0.75) +
    # annotate("label", x=0.9, y=1, label=paste0(round(mean(df$D_D,na.rm=T)*100, 1),"% of counties\nstuck to the Democrat"), hjust=0.75) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size=12,  family="Helvetica"),
          plot.title = element_text(size=16, family="Helvetica", face="bold"))
}

## Carter-Reagan swing ----------------------------------------------------

make_corr_plot(carter_reagan_df, x=Carter_1976, y=Carter_1980) +
  labs(title="1980 Election: As Many Counties Stuck with Carter as Swung to Reagan", 
       caption="Author: Soubhik Barari (2023)\nSource: Dave Leip's Atlas of U.S. Presidential Elections",
       y="County vote share for Jimmy Carter (1980)",
       x="County vote share for Jimmy Carter (1976)")
ggsave("figures/carter_reagan.png", height=6, width=10)

cor(carter_reagan_df$Carter_1976, carter_reagan_df$Carter_1980, use = "pairwise.complete.obs")

## Obama-Romney swing -----------------------------------------------------

make_corr_plot(obama_romney_df, x=Obama_2008, y=Obama_2012) +
  labs(title="2012 Election: Nearly All Counties Voted the Same Direction as in 2008", 
       caption="Author: Soubhik Barari (2023)\nSource: Dave Leip's Atlas of U.S. Presidential Elections",
       x="County vote share for Barack Obama (2008)",
       y="County vote share for Barack Obama (2012)")
ggsave("figures/obama_romney.png", height=6, width=10)

cor(obama_romney_df$Obama_2008, obama_romney_df$Obama_2012, use = "pairwise.complete.obs")

# Maps --------------------------------------------------------------------

states_map <- map_data('state')
counties_map <- map_data('county') %>%
  left_join(fips_codes %>%
              mutate(fips = paste0(state_code, county_code),
                     state_name = tolower(state_name),
                     county = gsub(" county","",tolower(county))) %>%
              select(region=state_name, subregion=county, fips))

obama_trump_biden_df <- cnty_vote_df %>%
  filter(year %in% c(2012, 2016, 2020),
         geographic_subtype == "County",
         grepl("(Obama|Romney|Clinton|Trump|Biden)", candidate)) %>%
  mutate(candidate = str_extract(candidate, "(Obama|Romney|Clinton|Trump|Biden)"),
         vote = vote/total_vote) %>%
  pivot_wider(id_cols = -total_vote, 
              names_from = c("candidate","year"), 
              values_from = "vote") %>%
  mutate(Obama_Trump_Biden = 
           (Obama_2012>Romney_2012) & (Trump_2016>Clinton_2016) & (Biden_2020>Trump_2020),
         Obama_Trump = 
           (Obama_2012>Romney_2012) & (Trump_2016>Clinton_2016),
         Trump_Biden = 
           (Trump_2016>Clinton_2016) & (Biden_2020>Trump_2020))
mean(obama_trump_biden_df$Obama_Trump_Biden,na.rm=T)
mean(obama_trump_biden_df$Obama_Trump,na.rm=T)
mean(obama_trump_biden_df$Trump_Biden,na.rm=T)

make_map <- function(counties_df) {
  ggplot() +
    ## states
    geom_polygon(data = states_map, 
                 fill = NA, color = "#2b2b2b",
                 aes(x = long, y = lat, group = group), size=0.5) +		
    ## counties
    geom_polygon(data = counties_map,
                 fill = NA, color = "#2b2b2b",
                 aes(x = long, y = lat, group = group), size=0.05) +
    ## pivot counties
    geom_polygon(data = counties_map %>%
                   inner_join(counties_df %>%
                                rename(county=geographic_name),
                              by = join_by(fips)), 
                 color = "#2b2b2b",
                 aes(x = long, y = lat, group = group, fill = fill), size=0.05) +
    ggthemes::theme_map() +
    theme(legend.position = "top",
          legend.justification = "center",
          legend.title.align = 0.5,
          legend.direction = "horizontal",
          plot.title = element_text(size=16, family="Helvetica", face="bold"))
}

## Obama-Trump counties ---------------------------------------------------

obama_trump_stats <- obama_trump_biden_df %>%
  summarise(n=sum(Obama_Trump,na.rm=T), 
            pct=mean(Obama_Trump,na.rm=T))

obama_trump_map <- make_map(obama_trump_biden_df %>%
                              filter(Obama_Trump) %>%
                              mutate(Clinton_2016 = Clinton_2016/(Clinton_2016+Trump_2016),
                                     Obama_2012 = Obama_2012/(Obama_2012+Romney_2012), 
                                     fill = -100*(Clinton_2016-Obama_2012))) +
  scale_fill_gradient(low="white", high="red", 
                      name="% Swing Towards Trump in 2016:",
                      breaks=c(5, 10, 15, 20, 25),
                      labels=c("+5","+10","+15","+20","+25"),
                      guide=guide_colorbar(frame.colour="black", 
                                           ticks.colour="black",
                                           title.vjust=0.75)) +
  labs(title=sprintf("%d (%d%%) Counties Were Both Won by Obama in 2012 and Trump in 2016", 
                     obama_trump_stats$n, round(obama_trump_stats$pct*100,0)), 
       caption="Author: Soubhik Barari (2023)\nSource: Dave Leip's Atlas of U.S. Presidential Elections")
obama_trump_map
ggsave("figures/obama_trump_map.png", height=6, width=8)
system("open figures/obama_trump_map.png")

## Trump-Biden counties ---------------------------------------------------

trump_biden_stats <- obama_trump_biden_df %>%
  summarise(n=sum(Trump_Biden,na.rm=T), 
            pct=mean(Trump_Biden,na.rm=T))

trump_biden_map <- make_map(obama_trump_biden_df %>%
                              filter(Trump_Biden) %>%
                              mutate(Clinton_2016 = Clinton_2016/(Clinton_2016+Trump_2016),
                                     Biden_2020 = Biden_2020/(Biden_2020+Trump_2020), 
                                     fill = 100*(Biden_2020-Clinton_2016))) +
  scale_fill_gradient(low="white", high="blue", 
                      name="% Swing Towards Biden in 2020:", 
                      breaks=c(2, 4, 6),
                      labels=c("+2","+4","+6"),                      
                      guide=guide_colorbar(frame.colour="black", 
                                           ticks.colour="black",
                                           title.vjust=0.75)) +
  labs(title=sprintf("%d (%d%%) Counties Were Won by Both Trump in 2016 and Biden in 2020", 
                     trump_biden_stats$n, round(trump_biden_stats$pct*100,0)), 
       caption="Author: Soubhik Barari (2023)\nSource: Dave Leip's Atlas of U.S. Presidential Elections")
trump_biden_map
ggsave("figures/trump_biden_map.png", height=6, width=8)
system("open figures/trump_biden_map.png")

## Obama-Trump-Biden counties ---------------------------------------------

obama_trump_biden_stats <- obama_trump_biden_df %>%
  summarise(n=sum(Obama_Trump_Biden,na.rm=T), 
            pct=mean(Obama_Trump_Biden,na.rm=T))

obama_trump_biden_map <- make_map(obama_trump_biden_df %>%
                                    filter(Obama_Trump_Biden) %>%
                                    mutate(Trump_2016 = Trump_2016/(Clinton_2016+Trump_2016),
                                           Biden_2020 = Biden_2020/(Biden_2020+Trump_2020), 
                                           Obama_2012 = Obama_2012/(Obama_2012+Romney_2012),
                                           fill = 100*((Trump_2016-Romney_2012)+(Biden_2020-Trump_2016))/2)) +
  scale_fill_gradient(low="white", high="red", 
                      name="Average % Swing Towards Challenger in 2016 and 2020:", 
                      breaks=c(0, 2, 4, 6),
                      labels=c("<+1", "+2","+4","+6"),                      
                      guide=guide_colorbar(frame.colour="black", 
                                           ticks.colour="black",
                                           title.vjust=0.75)) +
  labs(title=expression(bold("28 (<1%) Counties Voted Obama in 2012, Trump in 2016, ")~bolditalic("and")~bold(" Biden in 2020")), 
       caption="Author: Soubhik Barari (2023)\nSource: Dave Leip's Atlas of U.S. Presidential Elections") 
obama_trump_biden_map
ggsave("figures/obama_trump_biden_map.png", height=6, width=8.25)
system("open figures/obama_trump_biden_map.png")

