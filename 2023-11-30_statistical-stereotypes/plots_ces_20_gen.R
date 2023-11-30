# Enumerate and visualise statistical stereotypes by age group (generation) 
# using the Cooperative Election Study (CES) Responses (2020)

# Preamble ----------------------------------------------------------------

library(tidyverse)
library(ggrepel)

## exclude items with less than 50% plurality position
EXCLUDE_LESS_THAN_50_PCT = TRUE

clean_label <- function(.) {
  #str_wrap(., width = wd) %>%
  gsub("\\'", "\\\\\'", .) %>%
    paste0("atop('",.,"')") %>%
    gsub("atop\\(\\'", "atop(bold('", .) %>%
    gsub(" - ", ":') ~ '", .) %>%
    gsub(" -- ", ":') ~ '", .) %>%
    gsub("\\n", "', '", .)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!dir.exists("figures")) dir.create("figures")

# Prep --------------------------------------------------------------------

ces_20 <- here::here("data/cces/CES20_Common_OUTPUT_vv.dta") %>%
  haven::read_dta()

ces_20 <- ces_20 %>%
  mutate(age = 2020-birthyr) %>%
  mutate(gen4 = case_when(
    between(age, 18, 29) ~ "18-29",
    between(age, 30, 49) ~ "30-49",
    between(age, 50, 64) ~ "50-64",
    between(age, 65, 999) ~ "65+"
  ))

ces_20_qx <- colnames(ces_20) %>%
  sapply(function(.) attr(ces_20[[.]], "label")) %>%
  stack() %>%
  select(col=ind, qlabel=values)

ces_20_items <- list(
  ## attitudes
  "racial.sexual.agree"="440",
  "racial.resent"="441",
  #"approval"="320",
  #"media.use"="300b",
  ## policy positions
  "tax.increase"="421",
  "health.care"="327",
  "gun.control"="330",
  "immigration"="331",
  "abortion"="332",
  "environment"="333",
  "policing"="334",
  "trade"="338",
  "troops"="420"
)
ces_20_items_rgx <- paste0("(",paste0(unlist(ces_20_items),collapse="|"),")")
ces_20_items_rgx_excl <- "(timing|flag|other|r$)"

ces_20 <- ces_20 %>%
  ## turn 5-point agreement into agree/disagree
  mutate_at(vars(matches(ces_20_items_rgx_excl), -matches(ces_20_items_rgx_excl)), 
            ~case_when(grepl("disapprove",.x,ignore.case=T) ~ "Disapprove",
                       grepl("approve",.x,ignore.case=T) ~ "Approve",
                       grepl("neither agree",.x,ignore.case=T) ~ "Neutral",
                       grepl("disagree",.x,ignore.case=T) ~ "Disagree",
                       grepl("agree",.x,ignore.case=T) ~ "Agree",
                       grepl("not selected",.x,ignore.case=T) ~ "No",
                       grepl("selected",.x,ignore.case=T) ~ "Yes",
                       TRUE ~ .x))

# Code gen splits on "stance" items ---------------------------------------

ces_20_gen4_items <- ces_20 %>%
  ## turn numerics into labels
  labelled::unlabelled() %>%
  ## pivot
  select(commonweight, gen4, matches(ces_20_items_rgx)) %>%
  gather(key=col, value=v, -commonweight, -gen4) %>%
  group_by(gen4, col, v) %>%
  summarise(count = sum(commonweight, na.rm=T), .groups = "drop") %>%
  group_by(gen4, col) %>%
  mutate(prop = 100*(count/sum(count))) %>%
  left_join(ces_20_qx, by = c("col")) %>%
  filter(!grepl(ces_20_items_rgx_excl, col)) %>%
  filter(!grepl("other", qlabel)) %>%
  arrange(qlabel, v, gen4)
View(ces_20_gen4_items)

# Code gen majority position on items -------------------------------------

ces_20_gen4_items_maj <- ces_20_gen4_items %>%
  group_by(col, qlabel, gen4) %>%
  ## for simplicity, remove these types of questions
  filter(!grepl("None of the above", qlabel)) %>%
  filter(!is.na(v)) %>%  
  arrange(desc(prop)) %>% {
    if (EXCLUDE_LESS_THAN_50_PCT)
      filter(., row_number()==1 & prop >= 51)
    else
      filter(., row_number()==1)
  } %>%
  arrange(col, qlabel, gen4)

# Code majority/minority position for each respondent ---------------------

ces_20_maj.2 <- ces_20 %>%
  select(caseid, gen4, commonweight,
         matches(unique(ces_20_gen4_items_maj$col))) %>%
  labelled::unlabelled() %>%
  gather(key=col, value=v, -caseid, -gen4, -commonweight) %>%
  left_join(ces_20_gen4_items_maj %>%
              select(qlabel, gen4, col, v) %>%
              mutate(maj=1),
            by = join_by(gen4, col, v)) %>%
  mutate(maj=ifelse(is.na(maj), 0, 1)) %>%
  group_by(gen4, commonweight, caseid) %>%
  summarise(pct.maj = mean(maj), 
            n.items = n(),
            .groups = "drop")

# Plot --------------------------------------------------------------------

## Histogram --------------------------------------------------------------

ces_20_maj.2 %>%
  ggplot(aes(x=pct.maj, fill=gen4)) +
  geom_density(col="white", alpha=0.75) +
  geom_vline(xintercept=0.5, lty=3, alpha=0.5) + 
  scale_fill_discrete(name = "Age Group\n(Generation):",
                      labels = c("18-29\n(Gen Z)", "30-49\n(Millennial)","50-64\n(Gen X)","65+\n(Boomer)")) +
  scale_x_continuous(label=scales::percent_format(1),
                     limits=c(0,1),
                     name="% of positions aligned with party majority") +
  # annotate("text", x=0.9, 3, label="Democrats", color="blue") +
  # annotate("text", x=0.78, 3.1, label="Republicans", color="red") +
  # annotate("text", x=0.45, 2.25, label="Independents", color="darkgrey") +
  theme_classic() +
  labs(title="", 
       caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme(legend.position = "top",
        plot.title = element_text(face="bold"),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

## Power distribution -----------------------------------------------------

ces_20_gen4_maj <- seq(0, 1, by = 0.05) %>%
  map(~ces_20_maj.2 %>%
        group_by(gen4) %>%
        summarise(q = .x,
                  p = mean(pct.maj >= .x),
                  p.wt = sum((commonweight) * (pct.maj >= .x))/sum(commonweight),
                  .groups = "drop")) %>%
  bind_rows() %>%
  mutate(gen4.label = case_when(gen4 == "18-29" ~ "Gen Z",
                                gen4 == "30-49" ~ "Millennial",
                                gen4 == "50-64" ~ "Gen X",
                                gen4 == "65+" ~ "Boomer"))

ces_20_gen4_maj %>%
  ggplot(aes(x=q, y=p, color=gen4)) +
  geom_line(aes(lty=gen4), size=1) +
  scale_linetype_discrete(name = "Age Group\n(Generation):",
                          labels = c("18-29\n(Gen Z)", "30-49\n(Millennial)","50-64\n(Gen X)","65+\n(Boomer)")) +
  scale_color_discrete(name = "Age Group\n(Generation):",
                       labels = c("18-29\n(Gen Z)", "30-49\n(Millennial)","50-64\n(Gen X)","65+\n(Boomer)")) +
  scale_x_continuous(label=scales::percent_format(1),
                     name="...with % (or more) of positions aligned with generational majority") +
  scale_y_continuous(label=scales::percent_format(1),
                     name="% of respondents...") +
  geom_vline(xintercept=0.5, lty=3, alpha=0.5) + 
  ## annotations
  geom_point(data = ces_20_gen4_maj %>%
               filter(q == 0.5, gen4 %in% c("18-29","65+")),
             size = 3) +
  geom_text_repel(data = ces_20_gen4_maj %>%
                    filter(q == 0.5, gen4 %in% c("18-29","65+")),
                  aes(label = sprintf("atop(bold('%0.0f%%')~'of %ss\\' positions',italic('mostly')~'align with generation')",
                                      p*100, gen4.label)),
                  parse = TRUE,
                  box.padding = 1, nudge_y = -0.23, nudge_x = -0.5, min.segment.length = 0.1) +
  ## annotations
  geom_point(data = ces_20_gen4_maj %>%
               filter(q == 0.75, gen4 %in% c("18-29","65+")),
             size = 3) +
  geom_text_repel(data = ces_20_gen4_maj %>%
                    filter(q == 0.75, gen4 %in% c("18-29","65+")),
                  aes(label = sprintf("atop(bold('%0.0f%%')~'of %ss\\' positions',italic('strongly')~'align* with generation')",
                                      p*100, gen4.label)),
                  parse = TRUE,
                  box.padding = 1, nudge_y = 0, nudge_x = -0.5, min.segment.length = 0.1) +
  theme_minimal() +
  labs(title="Very few Americans are generational stereotypes", 
       caption = "*Here, strongly align means hold the majority opinion of co-generational respondents on 75% or more policy/attitude items\nSource: CCES (2020)\nAuthor: Soubhik Barari") +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 22, face = "bold"))
ggsave("figures/ces_20_gen_power.jpg", height=7, width=8)
system("open figures/ces_20_gen_power.jpg")


## Cascading bar plot -----------------------------------------------------
## Show how joint distribution of being 'in majority' changes as we add more
## questions

### Boomers ----

ces_20_majv_boomers <- ces_20 %>%
  select(caseid, gen4, commonweight,
         matches(unique(ces_20_pid3_items_maj$col))) %>%
  ## just Boomers for demonstration
  filter(gen4 %in% c("65+")) %>%
  labelled::unlabelled() %>%
  gather(key=col, value=v, -caseid, -gen4, -commonweight) %>%
  left_join(ces_20_gen4_items_maj %>%
              select(gen4, col, v, maj.prop=prop) %>%
              mutate(maj=1),
            by = join_by(gen4, col, v)) %>%
  mutate(maj=ifelse(is.na(maj), 0, 1)) %>%
  filter(!grepl(ces_20_items_rgx_excl, qlabel, ignore.case=T)) %>%
  filter(!grepl(ces_20_items_rgx_excl, col, ignore.case=T)) %>%    
  arrange(desc(maj.prop))

ces_20_majv_boomers_items <- ces_20_gen4_items_maj %>% 
  filter(gen4 == "65+") %>% 
  arrange(desc(prop)) %>%
  distinct(col) %>% 
  pull()
ces_20_majv_boomers_casc <- 1:length(ces_20_majv_boomers_items) %>%
  map(function(.x) {
    cat("\n [",.x,"/",length(ces_20_majv_boomers_items),"]")
    ces_20_majv_boomers %>%
      filter(col %in% ces_20_majv_boomers_items[1:.x]) %>%
      group_by(caseid) %>%
      summarise(all.maj = all(maj==1),
                commonweight = first(commonweight),
                .groups = "drop") %>%
      summarise(pct.joint.maj = sum(all.maj*commonweight)/sum(commonweight),
                #pct.joint.maj = mean(all.maj),
                col = ces_20_majv_boomers_items[.x],
                .groups = "drop")
  }) %>%
  bind_rows() %>%
  left_join(ces_20_gen4_items_maj %>% 
              rename(pct.mgn.maj = prop) %>%
              mutate(pct.mgn.maj = pct.mgn.maj/100) %>%
              filter(gen4 == "65+"))

ces_20_majv_boomers_casc %>%
  #mutate(qlabel = ifelse(row_number() != 1, paste("+", qlabel), qlabel)) %>%
  arrange(pct.joint.maj) %>%
  mutate(qlabel = as_factor(qlabel)) %>%
  gather(key="stat", value="val", pct.joint.maj, pct.mgn.maj) %>%
  mutate(stat = case_when(
    stat == "pct.joint.maj" ~ "Hold position\nand all other positions\nabove it",
    stat == "pct.mgn.maj" ~ "Hold position",
  )) %>%
  ggplot(aes(y=qlabel, x=val, fill=val)) +
  facet_grid(~ stat) +
  geom_bar(stat="identity") +
  geom_vline(xintercept=0.5, lty=1, alpha=0.5) +
  geom_text(aes(label=case_when(val > 0.5 ~ sprintf("%0.0f%%", val*100),
                                val > 0.45 ~ "<50%",
                                TRUE ~ "")), 
            color="white", nudge_x=-0.2) +  
  scale_y_discrete(name = "") +
  scale_fill_gradient(low="white", high="#F8766D") +
  scale_x_continuous(label=scales::percent_format(1),
                     sec.axis = dup_axis(),
                     position = "top",
                     name="% of Boomer respondents who...") +
  labs(title="XX", 
       caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"),
        axis.title.x.top = element_text(size=12),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title.x.bottom = element_blank(),
        legend.position = "none")

### Zoomers ----

ces_20_gen4_items_maj %>%
  filter(gen4 == "18-29") %>%
  arrange(desc(prop)) %>%
  ungroup() %>%
  select(qlabel, v, prop) %>%
  as.data.frame()

ces_20_majv_zoomers <- ces_20 %>%
  select(caseid, gen4, commonweight,
         matches(unique(ces_20_gen4_items_maj$col))) %>%
  ## just Zoomers for demonstration
  filter(gen4 %in% c("18-29")) %>%
  labelled::unlabelled() %>%
  gather(key=col, value=v, -caseid, -gen4, -commonweight) %>%
  left_join(ces_20_gen4_items_maj %>%
              select(gen4, col, v, maj.prop=prop) %>%
              mutate(maj=1),
            by = join_by(gen4, col, v)) %>%
  mutate(maj=ifelse(is.na(maj), 0, 1)) %>%
  filter(!grepl(ces_20_items_rgx_excl, qlabel, ignore.case=T)) %>%
  filter(!grepl(ces_20_items_rgx_excl, col, ignore.case=T)) %>%   
  arrange(desc(maj.prop))

ces_20_majv_zoomers_items <- ces_20_gen4_items_maj %>% 
  filter(gen4 == "18-29") %>% 
  arrange(desc(prop)) %>%
  distinct(col) %>%
  pull()
ces_20_majv_zoomers_casc <- 1:length(ces_20_majv_zoomers_items) %>%
  map(function(.x) {
    cat("\n [",.x,"/",length(ces_20_majv_zoomers_items),"]")
    ces_20_majv_zoomers %>%
      filter(col %in% ces_20_majv_zoomers_items[1:.x]) %>%
      group_by(caseid) %>%
      summarise(all.maj = all(maj==1),
                commonweight = first(commonweight),
                pct.mgn.maj = last(na.omit(maj.prop))/100,
                .groups = "drop") %>%
      summarise(pct.joint.maj = sum(all.maj*commonweight)/sum(commonweight),
                #pct.joint.maj = mean(all.maj),
                pct.mgn.maj = first(pct.mgn.maj),
                col = ces_20_majv_zoomers_items[.x],
                .groups = "drop")
  }) %>%
  bind_rows() %>%
  left_join(ces_20_gen4_items_maj %>% 
              filter(gen4 == "18-29"))

## since (possibly) public-facing, create pretty labels
if (!file.exists(here::here("data","cces","ces_20_majv_casc_z_labels.csv"))) {
  write_csv(distinct(ces_20_majv_zoomers_casc, col, qlabel),
            file = here::here("data","cces","ces_20_majv_casc_z_labels.csv"))
  system(sprintf("open '%s'", here::here("data","cces","ces_20_majv_casc_z_labels.csv")))
  readline("Enter clean `qlabel` for questions")
} 
ces_20_majv_casc_zoomer_labels <- read_csv(
  here::here("data","cces","ces_20_majv_casc_z_labels.csv")
)

ces_20_majv_zoomers_casc %>%
  anti_join(ces_20_majv_casc_zoomer_labels, by = "col")

ces_20_majv_zoomers_casc <- ces_20_majv_zoomers_casc %>%
  select(-qlabel) %>%
  left_join(ces_20_majv_casc_zoomer_labels,
            by = "col")

ces_20_majv_zoomers_casc_clean <- ces_20_majv_zoomers_casc %>%
  #mutate(qlabel = ifelse(row_number() != 1, paste("+", qlabel), qlabel)) %>%
  arrange(pct.joint.maj) %>%
  rowwise() %>%
  mutate(qlabel = gsub(" -", paste0(" (",v,") -"), qlabel)) %>%
  mutate(qlabel.clean = clean_label(qlabel)) %>%
  mutate(qlabel = as_factor(qlabel)) %>%
  gather(key="stat", value="val", pct.joint.maj, pct.mgn.maj) %>%
  mutate(stat = case_when(
    stat == "pct.joint.maj" ~ "Hold position\nand all other\npositions above it",
    stat == "pct.mgn.maj" ~ "Hold position",
  ))

unique(ces_20_majv_zoomers_casc_clean$qlabel.clean)

ces_20_majv_zoomers_casc_clean %>%
  ggplot(aes(y=qlabel, x=val, fill=val)) +
  facet_grid(~ stat) +
  geom_bar(stat="identity") +
  geom_vline(xintercept=0.5, lty=1, alpha=0.5) +
  geom_text(aes(label=case_when(val > 0.5 ~ sprintf("%0.0f%%", val*100),
                                val > 0.45 ~ "<50%",
                                TRUE ~ "")), 
            color="white", nudge_x=-0.2) +  
  scale_y_discrete(name = "",
                   labels = parse(text = unique(ces_20_majv_zoomers_casc_clean$qlabel.clean))) +
  scale_fill_gradient2(low="white", mid="gray", high="#C77CFF",
                       midpoint=0.5) +
  scale_x_continuous(label=scales::percent_format(1),
                     sec.axis = dup_axis(),
                     position = "top",
                     name="% of Gen Z respondents who...") +
  labs(title="A Gen Z political 'type' might not exist", 
       caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme_linedraw() +
  theme(plot.title = element_text(face="bold",
                                  size=26,
                                  hjust=1),
        panel.spacing = unit(1, "lines"),
        axis.title.x.top = element_text(size=14),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.text.y = element_text(vjust=0.8, size=11),
        strip.text = element_text(face="bold",
                                  size=12),
        legend.position = "none")
ggsave("figures/ces_20_pid_casc_Z.jpg", height=10, width=10)
system("open figures/ces_20_pid_casc_Z.jpg")

## Concentric circles -----------------------------------------------------

### Zoomers ----

ces_20_majv_zoomers_casc %>% 
  ggplot(aes(x=0, y=0, fill=pct.joint.maj, color=pct.joint.maj, size=200*pct.joint.maj)) +
  geom_point(shape=21, alpha=1) +
  scale_size_identity() +
  scale_x_continuous(limits=c(-1,0.6)) +
  scale_y_continuous(limits=c(-0.7,1.1)) +
  scale_color_gradient2(low="white", mid="gray", high="black") +
  scale_fill_gradient2(low="white", mid="gray", high="#C77CFF",
                       midpoint=0.5) +
  geom_text_repel(data = ces_20_majv_zoomers_casc_clean %>%
                    filter(grepl("and all other", stat)) %>%
                    slice(c(1)) %>%
                    mutate(qlabel.clean = "Zoomers holding ALL\nZoomer-majority policy positions"),
                  aes(label=qlabel.clean, y=0, x=val), 
                  min.segment.length = 4,
                  #parse = TRUE,
                  color = "black",
                  size = 5,
                  hjust = 0.5,
                  nudge_y = 1.1,
                  inherit.aes = F) +
  geom_text_repel(data = ces_20_majv_zoomers_casc_clean %>%
                    filter(grepl("and all other", stat)) %>%
                    slice(c(n())) %>%
                    mutate(qlabel.clean = "Zoomers holding MOST popular\nZoomer policy position\n(officer body cams)"),
                  aes(label=qlabel.clean, y=0.25, x=-val/2-0.08), 
                  min.segment.length = 4,
                  color = "black",
                  size = 5,
                  nudge_x = -0.29,
                  nudge_y = 0.6,
                  inherit.aes = F) +
  labs(caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme_void() +
  theme(legend.position="none",
        plot.title = element_text(face="bold", size=22))
ggsave("figures/ces_20_gen_conc_Z.jpg", height=8, width=8)
system("open figures/ces_20_gen_conc_Z.jpg")
