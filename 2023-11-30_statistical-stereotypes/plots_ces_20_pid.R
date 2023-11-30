# Enumerate and visualise statistical stereotypes by partisanship
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

ces_20_qx <- colnames(ces_20) %>%
  sapply(function(.) attr(ces_20[[.]], "label")) %>%
  stack() %>%
  select(col=ind, qlabel=values)

ces_20_items <- list(
  ## attitudes
  "racial.sexual.agree"="440",
  "racial.resent"="441",
  # "approval"="320",
  # "media.use"="300b",
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

# Code PID splits on "stance" items ---------------------------------------

ces_20_pid3_items <- ces_20 %>%
  ## only D/R/I
  filter(pid3 %in% 1:3) %>%
  ## turn numerics into labels
  labelled::unlabelled() %>%
  ## pivot
  select(commonweight, pid3, matches(ces_20_items_rgx)) %>%
  gather(key=col, value=v, -commonweight, -pid3) %>%
  group_by(pid3, col, v) %>%
  summarise(count = sum(commonweight, na.rm=T), .groups = "drop") %>%
  group_by(pid3, col) %>%
  mutate(prop = 100*(count/sum(count))) %>%
  left_join(ces_20_qx, by = c("col")) %>%
  filter(!grepl(ces_20_items_rgx_excl, qlabel, ignore.case=T)) %>%
  filter(!grepl(ces_20_items_rgx_excl, col, ignore.case=T)) %>%
  arrange(qlabel, v, pid3)
View(ces_20_pid3_items)

# Code PID majority position on items -------------------------------------

ces_20_pid3_items_maj <- ces_20_pid3_items %>%
  group_by(col, qlabel, pid3) %>%
  ## for simplicity, remove these types of questions
  filter(!grepl("None of the above", qlabel)) %>%
  filter(!is.na(v)) %>%
  arrange(desc(prop)) %>% {
    if (EXCLUDE_LESS_THAN_50_PCT)
      filter(., row_number()==1 & prop >= 51)
    else
      filter(., row_number()==1)
  } %>%
  arrange(col, qlabel, pid3)

## check abortion question
ces_20_pid3_items_maj %>% 
  filter(col == "CC20_332f")

# Code majority/minority position for each respondent ---------------------

ces_20_maj.0 <- ces_20 %>%
  select(caseid, pid3, commonweight,
         matches(unique(ces_20_pid3_items_maj$col))) %>%
  filter(pid3 %in% 1:3) %>%
  labelled::unlabelled() %>%
  gather(key=col, value=v, -caseid, -pid3, -commonweight) %>%
  left_join(ces_20_pid3_items_maj %>%
              select(pid3, col, v) %>%
              mutate(maj=1),
            by = join_by(pid3, col, v)) %>%
  mutate(maj=ifelse(is.na(maj), 0, 1)) 

## check abortion question
ces_20_maj.0 %>%
  filter(col == "CC20_332f") %>% 
  group_by(pid3, v) %>% 
  count(maj)

ces_20_maj <- ces_20_maj.0 %>%
  group_by(pid3, commonweight, caseid) %>%
  summarise(pct.maj = mean(maj), 
            n.items = n(),
            .groups = "drop")

# Plot --------------------------------------------------------------------

## Histogram --------------------------------------------------------------

ces_20_maj %>% 
  group_by(pid3) %>% 
  summarise(not.maj = mean(pct.maj < 0.5))
# pid3        not.maj
# <fct>         <dbl>
# 1 Democrat     0.0801
# 2 Republican   0.167 
# 3 Independent  0.259 

ces_20_maj %>%
  ggplot(aes(x=pct.maj, fill=pid3)) +
  geom_density(col="white", alpha=0.75) +
  geom_vline(xintercept=0.5, lty=3, alpha=0.5) + 
  scale_fill_manual(values=c("blue","red","darkgrey"), 
                    labels=c("Dem.","Rep.","Ind."),
                    name="Party:") +
  scale_x_continuous(label=scales::percent_format(1),
                     limits=c(0,1),
                     name="% of responses to policy and racial attitudes questions aligned with party majority") +
  annotate("text", x=0.93, y=3.1, size=5, label="Democrats", color="blue") +
  annotate("text", x=0.77, y=3.2, size=5, label="Republicans", color="red") +
  annotate("text", x=0.39, y=2.26, size=5, label="Independents", color="darkgrey") +
  theme_classic() +
  labs(title="Many voters, especially Independents,\nhave views that don't align with group majority", 
       caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme(legend.position = "none",
        plot.title = element_text(size = 22, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("figures/ces_20_pid_hist.jpg", height=6, width=8)
system("open figures/ces_20_pid_hist.jpg")

## Power distribution -----------------------------------------------------

ces_20_pid3_maj <- seq(0, 1, by = 0.05) %>%
  map(~ces_20_maj %>%
        group_by(pid3) %>%
        summarise(q = .x,
                  p = mean(pct.maj >= .x),
                  p.wt = sum((commonweight) * (pct.maj >= .x))/sum(commonweight),
                  .groups = "drop")) %>%
  bind_rows()

ces_20_pid3_maj %>%
  ggplot(aes(x=q, y=p, color=pid3)) +
  geom_line(size=1) +
  scale_color_manual(values=c("blue","red","darkgrey"), 
                     labels=c("Dem.","Rep.","Ind."),
                     name="Party of respondent:") +
  scale_x_continuous(label=scales::percent_format(1),
                     name="...with % (or more) of positions aligned with party majority") +
  scale_y_continuous(label=scales::percent_format(1),
                     name="% of respondents...") +
  geom_vline(xintercept=0.5, lty=3, alpha=0.5) + 
  # geom_vline(xintercept=0) +
  # geom_hline(yintercept=0) +
  ## annotations
  geom_point(data = ces_20_pid3_maj %>% 
               filter(q == 0.5, pid3 != "Independent"),
             size = 3) +
  geom_text_repel(data = ces_20_pid3_maj %>% 
                    filter(q == 0.5, pid3 != "Independent"),
                  aes(label = sprintf("atop(bold('%0.0f%%')~'of %ss\\'','positions'~italic('mostly')~'align with party')", 
                                      p*100, pid3)), 
                  parse = TRUE,
                  box.padding = 1, nudge_y = -0.2, nudge_x = -0.5, min.segment.length = 0.1) +
  ## annotations
  geom_point(data = ces_20_pid3_maj %>% 
               filter(q == 0.75, pid3 != "Independent"),
             size = 3) +
  geom_text_repel(data = ces_20_pid3_maj %>% 
                    filter(q == 0.75, pid3 != "Independent"),
                  aes(label = sprintf("atop(bold('%0.0f%%')~'of %ss\\'','positions'~italic('strongly')~'align* with party')", p*100, pid3)), 
                  parse = TRUE,
                  box.padding = 1, nudge_y = -0.1, nudge_x = -0.5, min.segment.length = 0.1) +
  theme_minimal() +
  labs(title="A minority of Democrats and Republicans\nstrongly align with their party peers", 
       caption = "*Here, strongly align means hold the majority opinion of co-partisan respondents on 75% or more policy/attitude items\nSource: CCES (2020)\nAuthor: Soubhik Barari") +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 22, face = "bold"))
ggsave("figures/ces_20_pid_power.jpg", height=7, width=8)
system("open figures/ces_20_pid_power.jpg")


## Cascading bar plot -----------------------------------------------------
## Show how joint distribution of being 'in majority' changes as we add more
## questions

### Republicans ----

ces_20_majv <- ces_20 %>%
  select(caseid, pid3, commonweight,
         matches(unique(ces_20_pid3_items_maj$col))) %>%
  ## just Republicans for demonstration
  filter(pid3 %in% 2) %>%
  labelled::unlabelled() %>%
  gather(key=col, value=v, -caseid, -pid3, -commonweight) %>%
  left_join(ces_20_pid3_items_maj %>%
              select(pid3, col, v, maj.prop=prop) %>%
              mutate(maj=1),
            by = join_by(pid3, col, v)) %>%
  mutate(maj=ifelse(is.na(maj), 0, 1)) %>%
  filter(!grepl(ces_20_items_rgx_excl, qlabel, ignore.case=T)) %>%
  filter(!grepl(ces_20_items_rgx_excl, col, ignore.case=T)) %>%   
  arrange(desc(maj.prop))

ces_20_majv_items <- ces_20_pid3_items_maj %>% 
  filter(pid3 == "Republican") %>% 
  arrange(desc(prop)) %>%
  distinct(col) %>% 
  pull()
ces_20_majv_casc <- 1:length(ces_20_majv_items) %>%
  map(function(.x) {
    print(.x)
    ces_20_majv %>%
      filter(col %in% ces_20_majv_items[1:.x]) %>%
      group_by(caseid) %>%
      summarise(all.maj = all(maj==1),
                commonweight = first(commonweight),
                pct.mgn.maj = last(na.omit(maj.prop))/100,
                .groups = "drop") %>%
      summarise(pct.joint.maj = sum(all.maj*commonweight)/sum(commonweight),
                # pct.joint.maj = mean(all.maj),
                pct.mgn.maj = first(pct.mgn.maj),
                col = ces_20_majv_items[.x],
                .groups = "drop")
  }) %>%
  bind_rows() %>%
  left_join(ces_20_pid3_items_maj %>% 
              filter(pid3 == "Republican"))

## since public-facing, create pretty labels
if (!file.exists(here::here("data","cces","ces_20_majv_casc_labels.csv"))) {
  write_csv(distinct(ces_20_majv_casc, col, qlabel),
            file = here::here("data","cces","ces_20_majv_casc_labels.csv"))
  system(sprintf("open '%s'", here::here("data","cces","ces_20_majv_casc_labels.csv")))
  readline("Enter clean `qlabel` for questions")
}
ces_20_majv_casc_labels <- read_csv(
  here::here("data","cces","ces_20_majv_casc_labels.csv")
)

ces_20_majv_casc %>%
  anti_join(ces_20_majv_casc_labels, by = "col")

if (any(ces_20_majv_casc %>% anti_join(ces_20_majv_casc_labels, by = "col"))) {
  print(ces_20_majv_casc %>% anti_join(ces_20_majv_casc_labels, by = "col") %>% distinct(col, qlabel))
  write_csv(ces_20_majv_casc %>% anti_join(ces_20_majv_casc_labels, by = "col") %>% distinct(col, qlabel),
            file = here::here("data","cces","ces_20_majv_casc_labels.csv"), append = TRUE)
  readline("Enter clean `qlabel` for missed questions")
}

ces_20_majv_casc <- ces_20_majv_casc %>%
  select(-qlabel) %>%
  left_join(ces_20_majv_casc_labels,
            by = "col")

ces_20_majv_casc_clean <- ces_20_majv_casc %>%
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

unique(ces_20_majv_casc_clean$qlabel.clean)

ces_20_majv_casc_clean %>%
  ggplot(aes(y=qlabel, x=val, fill=val)) +
  facet_grid(~ stat) +
  geom_bar(stat="identity") +
  geom_vline(xintercept=0.5, lty=1, alpha=0.5) +
  geom_text(aes(label=case_when(val > 0.5 ~ sprintf("%0.0f%%", val*100),
                                val > 0.45 ~ "<50%",
                                TRUE ~ "")), 
            color="white", nudge_x=-0.2, fontface="bold") +  
  scale_y_discrete(name = "",
                   labels = parse(text = unique(ces_20_majv_casc_clean$qlabel.clean))) +
  scale_fill_gradient2(low="white", mid="darkgrey", high="red", midpoint=0.5) +
  scale_x_continuous(label = scales::percent_format(1),
                     limits = c(0, 1), 
                     sec.axis = dup_axis(),
                     position = "top",
                     name="% of Republican respondents who...") +
  labs(title="Most Republicans don't actually hold most (even popular)\nRepublican policy positions", 
       caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme_linedraw() +
  theme(plot.title = element_text(face="bold",
                                  size=25,
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
ggsave("figures/ces_20_pid_casc_R.jpg", height=10, width=10)
system("open figures/ces_20_pid_casc_R.jpg")

### Democrats ----

ces_20_majv_d <- ces_20 %>%
  select(caseid, pid3, commonweight,
         matches(unique(ces_20_pid3_items_maj$col))) %>%
  filter(pid3 %in% 1) %>%
  labelled::unlabelled() %>%
  gather(key=col, value=v, -caseid, -pid3, -commonweight) %>%
  left_join(ces_20_pid3_items_maj %>%
              select(pid3, col, v, maj.prop=prop) %>%
              mutate(maj=1),
            by = join_by(pid3, col, v)) %>%
  mutate(maj=ifelse(is.na(maj), 0, 1)) %>%
  filter(!grepl(ces_20_items_rgx_excl, qlabel, ignore.case=T)) %>%
  filter(!grepl(ces_20_items_rgx_excl, col, ignore.case=T)) %>%
  arrange(desc(maj.prop))

ces_20_majv_items_d <- ces_20_pid3_items_maj %>% 
  filter(pid3 == "Democrat") %>% 
  arrange(desc(prop)) %>%
  distinct(col) %>% 
  pull()
ces_20_majv_casc_d <- 1:length(ces_20_majv_items_d) %>%
  map(function(.x) {
    cat("\n [",.x,"/",length(ces_20_majv_items_d),"]")
    ces_20_majv_d %>%
        filter(col %in% ces_20_majv_items_d[1:.x]) %>%
        group_by(caseid) %>%
        summarise(all.maj = all(maj==1),
                  pct.mgn.maj = last(na.omit(maj.prop))/100,
                  .groups = "drop") %>%
        summarise(pct.joint.maj = sum(all.maj*commonweight)/sum(commonweight),
                  # pct.joint.maj = mean(all.maj),
                  pct.mgn.maj = first(pct.mgn.maj),
                  col = ces_20_majv_items_d[.x],
                  .groups = "drop")
  }) %>%
  bind_rows() %>%
  left_join(ces_20_pid3_items_maj %>% 
              filter(pid3 == "Democrat"))

## since (possibly) public-facing, create pretty labels
if (!file.exists(here::here("data","cces","ces_20_majv_casc_d_labels.csv"))) {
  write_csv(distinct(ces_20_majv_casc_d, col, qlabel),
            file = here::here("data","cces","ces_20_majv_casc_d_labels.csv"))
  system(sprintf("open '%s'", here::here("data","cces","ces_20_majv_casc_d_labels.csv")))
  readline("Enter clean `qlabel` for questions")
} 
ces_20_majv_casc_d_labels <- read_csv(
  here::here("data","cces","ces_20_majv_casc_d_labels.csv")
)

ces_20_majv_casc_d <- ces_20_majv_casc_d %>%
  select(-qlabel) %>%
  left_join(ces_20_majv_casc_d_labels,
            by = "col")

ces_20_majv_casc_d_clean <- ces_20_majv_casc_d %>%
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

unique(ces_20_majv_casc_d_clean$qlabel.clean)

ces_20_majv_casc_d_clean %>%
  ggplot(aes(y=qlabel, x=val, fill=val)) +
  facet_grid(~ stat) +
  geom_bar(stat="identity") +
  geom_vline(xintercept=0.5, alpha=0.5) +
  geom_text(aes(label=case_when(val > 0.5 ~ sprintf("%0.0f%%", val*100),
                                val > 0.48 ~ "<50%",
                                TRUE ~ "")), 
            color="white",
            nudge_x=-0.2) +
  scale_y_discrete(name = "",
                   labels = parse(text = unique(ces_20_majv_casc_d_clean$qlabel.clean))) +
  scale_fill_gradient2(low="white", mid="darkgray", high="blue",
                       midpoint=0.5) +
  scale_x_continuous(label = scales::percent_format(1),
                     limits = c(0, .99),
                     sec.axis = dup_axis(),
                     position = "top",
                     name="% of Democrat respondents who...") +
  labs(title="", 
       caption = "Source: CCES (2020)\nAuthor: Soubhik Barari") +
  theme_linedraw() +
  theme(plot.title = element_text(face="bold"),
        panel.spacing = unit(1, "lines"),
        axis.title.x.top = element_text(size=12),
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.text.y = element_text(vjust=0.8),
        strip.text = element_text(face="bold"),
        legend.position = "none")

## Concentric circles -----------------------------------------------------

### Republicans ----

ces_20_majv_casc %>% 
  ggplot(aes(x=0, y=0, fill=pct.joint.maj, color=pct.joint.maj, size=200*pct.joint.maj)) +
  geom_point(shape=21, alpha=1) +
  scale_size_identity() +
  scale_x_continuous(limits=c(-1,0.6)) +
  scale_y_continuous(limits=c(-0.7,1.1)) +
  scale_color_gradient2(low="white", mid="gray", high="black") +
  scale_fill_gradient2(low="white", mid="gray", high="red",
                        midpoint=0.5) +
  geom_text_repel(data = ces_20_majv_casc_clean %>%
                    filter(grepl("and all other", stat)) %>%
                    slice(c(1)) %>%
                    mutate(qlabel.clean = "Republicans holding ALL\nRepublican-majority policy positions"),
                  aes(label=qlabel.clean, y=0, x=val), 
                  min.segment.length = 4,
                  #parse = TRUE,
                  color = "black",
                  size = 5,
                  hjust = 0.5,
                  nudge_y = 1.1,
                  inherit.aes = F) +
  geom_text_repel(data = ces_20_majv_casc_clean %>%
                    filter(grepl("and all other", stat)) %>%
                    slice(c(n())) %>%
                    mutate(qlabel.clean = "Republicans holding MOST popular\nRepublican policy position\n(increase border security)"),
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
ggsave("figures/ces_20_pid_conc_R.jpg", height=8, width=8)
system("open figures/ces_20_pid_conc_R.jpg")
