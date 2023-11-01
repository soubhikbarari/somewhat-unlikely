# Preamble ----------------------------------------------------------------

library(cli)
library(haven)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(stringdist)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data --------------------------------------------------------------------

if (!file.exists("data/kousser_gamm_21.rds")) {
  kousser_gamm_21 <- dataverse::get_dataframe_by_name("PoliticsProsperityJune2021.tab",
                                                      dataset="10.7910/DVN/1HOKCH",
                                                      .f=readr::read_tsv)
  saveRDS(kousser_gamm_21, file="kousser_gamm_21.rds")
} else {
  kousser_gamm_21 <- readRDS("data/kousser_gamm_21.rds")
}

dwnom <- read_csv("../data/congress/DWnominate.csv")
congress_terms <- read_csv("../data/congress/congress_terms.csv")

house_elections <- read_csv("../data/medsl/1976-2022-house.csv")
senate_elections <- read_csv("../data/medsl/1976-2020-senate.csv")

house_legis_eff <- read_dta("data/legis_eff/CELHouse93to117ReducedClassic.dta")
senate_legis_eff <- read_dta("data/legis_eff/CELSenate93to117ReducedClassic.dta")

## get senators' last names
dwnom$bioname_last <- dwnom$bioname %>%
  str_extract("[A-Z]*\\,") %>%
  utf8::utf8_encode() %>%
  trimws(whitespace=",")

senate_elections$candidate_last <- senate_elections$candidate %>%
  str_extract(" .*$") %>%
  utf8::utf8_encode() %>%
  gsub("([A-Z]\\.|JR\\.|SR\\.|II|III|IV|\\,)", "", .) %>%
  trimws()

## make margins
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

senate_margins <- senate_elections %>%
  filter(stage == "gen", special == FALSE) %>%
  mutate(st=state.abb[match(tolower(state), tolower(state.name))]) %>%
  select(year, st, party=party_simplified, candidate, candidate_last, candidatevotes, totalvotes) %>%
  group_by(year, st) %>% 
  filter(grepl("DEM", party) | grepl("REP", party)) %>%
  filter(n() == 2) %>%
  mutate(candidatevotes=candidatevotes/totalvotes) %>%
  arrange(desc(candidatevotes)) %>%
  summarise(winner=first(candidate),
            winner_last=first(candidate_last),
            win_margin=max(candidatevotes) - min(candidatevotes),
            dem_margin=candidatevotes[grepl("DEM", party)] - candidatevotes[grepl("REP", party)],
            .groups="drop")

# Electoral competition -> polarization -----------------------------------

house_margins_x_dw <- house_margins %>%
  mutate(label=sprintf("%s-%02d ('%s)", st, district, substr(year, 3, 4))) %>%
  inner_join(congress_terms %>%
               select(congress, year=begin.date.1) %>%
               mutate(year=as.numeric(substr(year, 1, 4))-1)) %>%
  inner_join(dwnom %>% 
               filter(chamber == "House") %>%
               select(congress, cand_last=bioname_last, st=state_abbrev, district=district_code, nominate_dim1)) %>%
  filter(year > 2010) %>%
  mutate(party=ifelse(dem_margin > 0, "Dem", "Rep"))

house_margins_x_dw %>%
  mutate(label=sprintf("%s (%s-%02d '%s)", str_to_title(cand_last), st, district, substr(year, 3, 4))) %>%
  ggplot(aes(x=win_margin, y=nominate_dim1, fill=party, group=paste(party, year))) + 
  scale_y_continuous(name="DW Nominate Score of Representative") +
  scale_x_continuous(labels=scales::percent_format(1), name="Representative's Margin of Victory") +
  geom_point(color="white", shape=21, size=3, alpha=0.6) +
  # geom_vline(xintercept=0, lty=2) +
  geom_smooth(aes(color=party), method="lm", formula=y ~ poly(x, 2), se=FALSE) +  
  geom_label_repel(aes(label=ifelse(win_margin < 0.01, label, NA)),
                   box.padding=0.9, max.overlaps=100, color="white", 
                   segment.size=0.75,
                   segment.color="black",
                   color="white") +
  scale_fill_manual(values=c("blue", "red"), name="Party of Rep:") +
  scale_color_manual(values=c("blue", "red"), name="Party of Rep:") +
  theme_bw() +
  theme(legend.position="bottom")

senate_margins_x_dw <- senate_margins %>%
  rename(cand_last=winner_last) %>%
  inner_join(congress_terms %>%
               select(congress, year=begin.date.1) %>%
               mutate(year=as.numeric(substr(year, 1, 4))-1)) %>%
  inner_join(dwnom %>% 
               filter(chamber == "Senate") %>%
               select(congress, cand_last=bioname_last, st=state_abbrev, nominate_dim1),
             by=c("congress", "st")) %>%
  mutate(cand_last.dist=stringdist(cand_last.x, cand_last.y, method="cosine")) %>%
  group_by(congress, st) %>%
  arrange(cand_last.dist) %>%
  filter(cand_last.dist == first(cand_last.dist)) %>%
  mutate(party=ifelse(dem_margin > 0, "Dem", "Rep")) %>%
  filter(year > 2010)

senate_margins_x_dw %>%
  mutate(label=sprintf("%s (%s '%s)", str_to_title(cand_last.y), st, substr(year, 3, 4))) %>%
  ggplot(aes(x=win_margin, y=nominate_dim1, fill=party, group=paste(party, year))) + 
  scale_y_continuous(name="DW Nominate Score of Senator") +
  scale_x_continuous(labels=scales::percent_format(1), name="Senator's Margin of Victory") +
  geom_point(color="white", shape=21, size=3) +
  # geom_vline(xintercept=0, lty=2) +
  geom_smooth(aes(color=party), method="lm", formula=y ~ poly(x, 2), se=FALSE) +
  geom_label_repel(aes(label=ifelse(win_margin < 0.01, label, NA)),
                   box.padding=0.9, max.overlaps=100, color="white", 
                   segment.size=0.75,
                   segment.color="black",
                   color="white") +  
  scale_fill_manual(values=c("blue", "red"), name="Party of Senator:") +
  scale_color_manual(values=c("blue", "red"), name="Party of Senator:") +
  theme_bw() +
  theme(legend.position="bottom")


## Shor et al. (2017) ---------------------------------------------------

congress<-read_dta(paste("data/nrstw_17_heter_dists/congress_ideology_collapsed.dta", sep=""))
congress_ideology<-read_csv(paste("data/nrstw_17_heter_dists/cd_mrp_estimates.csv", sep=""))
load(file=paste("data/nrstw_17_heter_dists/cd_uncertainty_140415.RData", sep=""))

cd_uncertainty2<-merge(cd_uncertainty, congress, by.x="cd",by.y="fips", all.x=T)
cd_uncertainty2<-merge(cd_uncertainty2, congress_ideology, by.x="cd",by.y="fips", all.x=T)
cd_uncertainty2$republican<-NA
cd_uncertainty2$republican[cd_uncertainty2$pid3==1]<-0
cd_uncertainty2$republican[cd_uncertainty2$pid3==3]<-1
cd_uncertainty2$presdem_2008<-as.numeric(as.vector(cd_uncertainty2$presdem_2008))
cd_uncertainty2[which(cd_uncertainty2==Inf)]=NA

cuts <- 3
cols=c("blue4","red4")
cd_uncertainty2$cat.het.cit<-as.integer(cut_number(cd_uncertainty2$heterogeneity_citizens, cuts))
cd_uncertainty2$party<-NA
cd_uncertainty2$party[cd_uncertainty2$republican==0]<-"D"
cd_uncertainty2$party[cd_uncertainty2$republican==1]<-"R"

### Replication ----
p <- ggplot(subset(cd_uncertainty2,!is.na(cat.het.cit)), 
          aes(
            x=mrp_estimate,
            #x=presdem_2008,
            y=dwnom1, color=party, shape=party)) +
  stat_smooth(method="lm", formula=y ~ poly(x, 2), size=1) +
  geom_point(alpha=.25, size=2) +
  scale_color_manual(values=cols) +
  facet_grid(~ cat.het.cit) +
  labs(list(y="Legislator Ideology",x="District Opinion",title="Heterogeneity",fill="")) +
  theme_bw() +
  theme(legend.position="none")

### Median -> heterogeneity ----
cd_uncertainty2 %>%
  filter(!is.na(cat.het.cit)) %>% 
  mutate(cd=as.numeric(substr(stringr::str_pad(cd, width=4, side="left", pad="0"),
                                3, 4))) %>%
  left_join(house_margins_x_dw %>%
              mutate(label=sprintf("%s-%02d", st, district)) %>%
              distinct(abb=st, cd=district, label)) %>%
  distinct(heterogeneity_citizens, median_citizens, .keep_all=TRUE) %>%
  filter(!is.na(party)) %>%
  ggplot(aes(y=heterogeneity_citizens,
             x=median_citizens)) +
  stat_smooth(method="lm", formula=y ~ poly(x, 2), 
              size=1, color="black") +
  geom_vline(xintercept=0, lty=2) + 
  geom_point(aes(fill=median_citizens), 
             color="white", size=5, shape=21) +
  # geom_label_repel(aes(label=ifelse(between(median_citizens, -0.2, 0.2), label, NA),
  #                      fill=party,
  #                      color=party),
  #                  box.padding=0.9, max.overlaps=30, color="white", 
  #                  segment.size=0.75,
  #                  segment.color="black",
  #                  color="white") +   
  scale_color_manual(values=c("blue","red"), name="Party of Congressperson:") +
  #scale_fill_manual(values=c("blue","red"), name="Party of Congressperson:") +
  scale_x_continuous(expression(bold("Median")~"political ideology of district voters"),
                     limits=c(-1.8, 1.4)) +
  scale_y_continuous(expression(bold("Variability")~"in political ideology of district voters")) +
  annotate("text", size=5, x=-1.8, y=1.04, label=expression(" " %<-% "Less mixed"), angle=90) +
  annotate("text", size=5, x=-1.8, y=1.47, label=expression("More mixed" %->% " "), angle=90) +
  annotate("text", size=5, x=-1.55, y=0.9, label=expression(" " %<-% "More liberal"), angle=0) +
  annotate("text", size=5, x=0.9, y=0.9, label=expression("More conservative" %->% " "), angle=0) +    
  theme_bw() +
  scale_fill_gradient2(low="blue", mid="plum2", high="red",
                       midpoint=0) +
  ggtitle("Voters in 'purple' Congressional districts are \nsplit partisans, not unified moderates") +
  labs(caption="Source: Voter MRP estimates (2000-2012) from McCarty, Rodden, Shor et al. (2019)\nAuthor: Soubhik Barari") +  
  theme(legend.position="none",
        plot.title=element_text(face="bold", size=28),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_text(size=22))
ggsave("figures/cong_x=citmed_y=citmix.jpg", height=9, width=10)
system("open figures/cong_x=citmed_y=citmix.jpg")

### District opinion -> ideology ----
cd_uncertainty2 %>%
  filter(!is.na(cat.het.cit)) %>% 
  mutate(cd=as.numeric(substr(stringr::str_pad(cd, width=4, side="left", pad="0"),
                     3, 4)),
         party=ifelse(party=="D", "Democrat", "Republican")) %>%
  left_join(house_margins_x_dw %>%
              mutate(label=sprintf("%s-%02d", st, district)) %>%
              distinct(abb=st, cd=district, label)) %>%
  filter(!is.na(party)) %>%
  ggplot(aes(
    x=heterogeneity_citizens,
    #x=median_citizens,
    y=abs(dwnom1)
  )) +
  stat_smooth(method="lm", #formula=y ~ poly(x, 2), 
              color="black",
              se=TRUE,
              size=1) +
  geom_point(aes(color=party, fill=party), color="white", alpha=.5, 
             size=5, shape=21) +
  geom_label_repel(aes(label=label, color=party, fill=party),
                   box.padding=0.9, 
                   max.overlaps=30, 
                   size=5,
                   color="white", 
                   segment.size=0.75,
                   segment.color="black",
                   color="white") +   
  scale_x_continuous(expression("Variability in political ideology of district voters")) +
  scale_y_continuous("Ideology of representative in Congress") +
  annotate("text", size=5, x=0.9, y=0, label=expression(" " %<-% "More moderate rep"), angle=90) +
  annotate("text", size=5, x=0.9, y=0.95, label=expression("More partisan rep" %->% " "), angle=90) +
  annotate("text", size=5, x=1, y=-0.3, label=expression(" " %<-% "Less mixed"), angle=0) +
  annotate("text", size=5, x=1.42, y=-0.3, label=expression("More mixed" %->% " "), angle=0) +  
  scale_color_manual(values=c("blue","red"), name="Party of Congressperson:") +
  scale_fill_manual(values=c("blue","red"), name="Party of Congressperson:") +
  theme_bw() +
  ggtitle("'Purple' Congressional districts don't necessarily\nhave more moderate representatives") +
  labs(caption="Source: McCarty, Rodden, Shor et al. (2019)\nAuthor: Soubhik Barari") +
  theme(legend.position="top",
        plot.title=element_text(face="bold", size=28),
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_text(size=22))
ggsave("figures/cong_x=cithet_y=dw.jpg", height=10, width=10)
system("open figures/cong_x=cithet_y=dw.jpg")

# Electoral competition -> legislative productivity -----------------------
# * https://journals.sagepub.com/doi/10.1177/1532673X18760527
# * https://www.legbranch.org/2018-4-3-do-competitive-elections-influence-legislators-productivity/

## overall view
house_legis_eff %>%
  # select(congress, year, st=st_name, cd) %>%
  inner_join(house_margins %>%
              select(year, st_name=st, cd=district, win_margin) %>%
              mutate(year=year + 1),
             by=c("year", "st_name", "cd")) %>%
  mutate(dem=ifelse(dem == 1, "Dem.", "Rep.")) %>%
  filter(year > 2010) %>%
  select(win_margin, votepct, dem, sbill1:sslaw1, benchratio1) %>%
  gather(key="type", value="val", -win_margin, -dem, -votepct) %>%
  ggplot(aes(x=win_margin, y=val, fill=dem, group=dem)) +
  geom_point(aes(fill=dem, color=dem), shape=21, alpha=0.2) +
  geom_smooth(aes(color=dem)) +
  scale_color_manual(values=c("blue", "red")) + 
  scale_fill_manual(values=c("blue", "red")) +
  scale_y_continuous(trans="log10") +
  scale_x_continuous(labels=scales::percent_format(1)) +
  facet_wrap( ~ type, scales="free") +
  theme_classic() +
  theme(legend.position="top")

senate_legis_eff %>%
  # select(congress, year, st=st_name, cd) %>%
  inner_join(senate_margins %>%
               select(year, state=st, win_margin, last=winner_last),
             by=c("year", "state")) %>%
  mutate(dem=ifelse(dem == 1, "Dem.", "Rep.")) %>%
  mutate(last.dist=stringdist(last.x, last.y, method="cosine")) %>%
  group_by(year, state) %>%
  arrange(last.dist) %>%
  filter(last.dist == first(last.dist)) %>%
  ungroup() %>%
  filter(year > 2010) %>%
  select(win_margin, votepct, dem, sbill1:sslaw1, benchratio1) %>%
  gather(key="type", value="val", -win_margin, -dem, -votepct) %>%
  ggplot(aes(x=win_margin, y=val, fill=dem, group=dem)) +
  geom_point(aes(fill=dem, color=dem), shape=21, alpha=0.2) +
  geom_smooth(aes(color=dem)) +
  scale_color_manual(values=c("blue", "red")) + 
  scale_fill_manual(values=c("blue", "red")) +
  scale_y_continuous(trans="log10") +
  scale_x_continuous(labels=scales::percent_format(1)) +
  facet_wrap( ~ type, scales="free") +
  theme_classic() +
  theme(legend.position="top")

## selected plots
intersect(colnames(house_legis_eff), colnames(senate_legis_eff))

cat("Columns in", col_br_blue("`house_legis_eff`"),"\n")
for (col in colnames(house_legis_eff)) {
  cat("\n",col_br_magenta(col),":", attr(house_legis_eff[[col]],"label"))
}

cat("Columns in", col_br_blue("`senate_legis_eff`"),"\n")
for (col in colnames(senate_legis_eff)) {
  cat("\n",col_br_magenta(col),":", attr(senate_legis_eff[[col]],"label"))
}

house_legis_eff$lagles <- house_legis_eff$leslag
senate_legis_eff$lagles <- senate_legis_eff$lagles

OUTCOME="lagles"
#OUTCOME="sspass1"
YEAR_START=2010
attr(house_legis_eff[[OUTCOME]], "label")
attr(senate_legis_eff[[OUTCOME]], "label")

outcome_label <- attr(house_legis_eff[[OUTCOME]], "label")
outcome_label <- "Combined legislative effectiveness score"

bind_rows(
  house_legis_eff %>%
    mutate(bioname_last=stringr::str_to_title(gsub("\\,.*","", bioname)),
           bioname_first=stringr::str_to_title(gsub(".*\\,","", bioname))) %>%
    mutate(label=sprintf("%s %s (%s-%02d %d)", bioname_first, bioname_last, st_name, cd, year)) %>%
    # select(congress, year, st=st_name, cd) %>%
    inner_join(house_margins %>%
                 select(year, st_name=st, cd=district, win_margin) %>%
                 mutate(year=year + 1),
               by=c("year", "st_name", "cd")) %>%
    mutate(dem=ifelse(dem == 1, "Dem.", "Rep.")) %>%
    filter(year > YEAR_START) %>%
    select_at(c("label", "win_margin", "dem", OUTCOME)) %>%
    rename_at(OUTCOME, ~"outcome") %>%
    mutate(chamber="House")
  ,
  senate_legis_eff %>%
    mutate(label=sprintf("%s %s (%s %d)", first, last, state, year)) %>%
    # select(congress, year, st=st_name, cd) %>%
    inner_join(senate_margins %>%
                 select(year, state=st, win_margin, last=winner_last),
               by=c("year", "state")) %>%
    mutate(dem=ifelse(dem == 1, "Dem.", "Rep.")) %>%
    mutate(last.dist=stringdist(last.x, last.y, method="cosine")) %>%
    group_by(year, state) %>%
    arrange(last.dist) %>%
    filter(last.dist == first(last.dist)) %>%
    ungroup() %>%
    filter(year > YEAR_START) %>%
    select_at(c("label", "win_margin", "dem", OUTCOME)) %>%
    rename_at(OUTCOME, ~"outcome") %>%
    mutate(chamber="Senate")
) %>%
  ggplot(aes(x=win_margin, y=outcome)) +
  geom_point(aes(fill=dem), color="white", alpha=0.5, size=4, shape=21) +
  geom_smooth(color="black", method="lm") +
  geom_label_repel(aes(
    #label=ifelse(win_margin < 0.01 & outcome > 1, label, NA),
    label=ifelse(
      outcome > 7.5 & chamber == "House" & dem == "Rep." |
        (chamber == "House" & grepl("Waters", label, ignore.case=T)) |
        (chamber == "House" & grepl("Duckworth", label, ignore.case=T)) |
        (outcome > 3.2 & chamber == "Senate"), 
      label, 
      NA
    ),
    color=dem, fill=dem
  ),
  box.padding=0.9, max.overlaps=50, color="white", 
  size=4,
  segment.size=1,
  segment.color="black",
  color="white") +   
  scale_color_manual(values=c("blue", "red"), name="Party of Congressperson:") + 
  scale_fill_manual(values=c("blue", "red"), name="Party of Congressperson:") +
  scale_y_continuous(trans="log10",
                     name=outcome_label) +
  scale_x_continuous(labels=scales::percent_format(1), 
                     name="Margin of victory in previous election") +
  facet_grid(chamber ~ ., scales="free") +
  theme_bw() +
  labs(caption="Source: Center for Effective Lawmaking\nAuthor: Soubhik Barari") +
  ggtitle("Electoral competition has little to no\neffect on legislator productivity") +
  theme(legend.position="top",
        legend.title=element_text(size=18),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=22),
        plot.title = element_text(face="bold", size=24),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(face="bold", size=20, color="white"))
ggsave("figures/cong_x=mgn_y=legeff.jpg", height=10, width=8)
system("open figures/cong_x=mgn_y=legeff.jpg")
