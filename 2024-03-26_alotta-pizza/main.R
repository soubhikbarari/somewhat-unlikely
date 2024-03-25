library(here)
library(ggplot2)
library(tidyverse)

# Americans eating pizza on any given day ---------------------------------

if (!file.exists("data/wweia_17_foodcat_FNDDS.xlsx")) {
  download.file("https://www.ars.usda.gov/ARSUserFiles/80400530/apps/WWEIA2017_March2020_foodcat_FNDDS.xlsx",
                destfile = "data/wweia_17_foodcat_FNDDS.xlsx") 
}
wweia.17.foodcat.fndds <- readxl::read_excel("data/wweia_17_foodcat_FNDDS.xlsx")

if (!file.exists("data/nhanes_17-18_DR1IFF_J.XPT")) {
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.XPT",
                destfile = "data/nhanes_17-18_DR1IFF_J.XPT")
}
nhanes.17.18.dr1iff_j <- haven::read_xpt("data/nhanes_17-18_DR1IFF_J.XPT")

if (!file.exists("data/nhanes_17-18_DR2IFF_J.XPT")) {
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR2IFF_J.XPT",
                destfile = "data/nhanes_17-18_DR2IFF_J.XPT")
}
nhanes.17.18.dr2iff_j <- haven::read_xpt("data/nhanes_17-18_DR2IFF_J.XPT")

## Day 1
nhanes.17.18.dr1iff_j %>%
  select(SEQN, DR1DAY, DR1IFDCD, WTDRD1) %>%
  left_join(wweia.17.foodcat.fndds,
            by = c("DR1IFDCD"="food_code")) %>%
  mutate(pizza = category_description == "Pizza") %>%
  group_by(SEQN, DR1DAY, WTDRD1) %>%
  summarise(pizza = any(pizza), .groups = "drop") %>%
  #group_by(DR1DAY) %>%
  summarise(pizza = weighted.mean(pizza, WTDRD1), .groups = "drop")

## Day 2
nhanes.17.18.dr2iff_j %>%
  select(SEQN, DR2DAY, DR2IFDCD, WTDR2D) %>%
  left_join(wweia.17.foodcat.fndds,
            by = c("DR2IFDCD"="food_code")) %>%
  mutate(pizza = category_description == "Pizza") %>%
  group_by(SEQN, DR2DAY, WTDR2D) %>%
  summarise(pizza = any(pizza), .groups = "drop") %>%
  #group_by(DR2DAY) %>%
  summarise(pizza = weighted.mean(pizza, WTDR2D), .groups = "drop")


# Number of pizza eaten in past month -------------------------------------

## Individual food records
if (!file.exists("data/nhanes_09-10_DTQ_F.XPT")) {
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DTQ_F.XPT",
                destfile = "data/nhanes_09-10_DTQ_F.XPT")
}
nhanes.09.10.dtq_f    <- haven::read_xpt("data/nhanes_09-10_DTQ_F.XPT")

if (!file.exists("data/nhanes_09-10_DEMO_F.XPT")) {
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT",
                destfile = "data/nhanes_09-10_DEMO_F.XPT")
}
nhanes.09.10.demo_f   <- haven::read_xpt("data/nhanes_09-10_DEMO_F.XPT")

## DTD140Q:
## During the past month, how often did {you/SP} eat pizza? 
## Include frozen pizza, fast food pizza, and homemade pizza. 
## You can tell me per day, per week or per month.
nhanes.09.10.dtq_f$pizzas.per.day <- with(
  nhanes.09.10.dtq_f,
  case_when(DTQ140U == 1 ~ DTD140Q,
            DTQ140U == 2 ~ DTD140Q/7,
            DTQ140U == 3 ~ DTD140Q/30,
            DTD140Q == 0 ~ 0,
            TRUE ~ NA)
)
nhanes.09.10.dtq_f$pizzas.unit <- with(
  nhanes.09.10.dtq_f,
  case_when(DTQ140U == 1 ~ "day",
            DTQ140U == 2 ~ "week",
            DTQ140U == 3 ~ "month",
            TRUE ~ NA)
)

## The NHANES full sample 2-Year Interview Weights (WTINT2YR) 
## should be used to analyze the 2009-2010 DTQ data for 
## respondents ages 2-11 years old. The NHANES full sample 
## 2-Year MEC Exam Weights (WTMEC2YR) should be used to 
## analyze the 2009-2010 DTQ data for respondents ages 12-69 years old.

nhanes.09.10.dtq_f %>%
  select(SEQN, pizzas.per.day, pizzas.unit) %>%
  left_join(nhanes.09.10.demo_f %>%
              mutate(wt = case_when(RIDAGEYR <= 11 ~ WTINT2YR,
                                    RIDAGEYR > 11 ~ WTMEC2YR)) %>%
              select(SEQN, wt),
            by = "SEQN") %>%
  summarise(avg.pizzas.per.day = # rate of pizzas per day
              weighted.mean(pizzas.per.day, wt, na.rm=T),
            atl1.pizza.per.day.all = # % who explicitly said atl. 1 pizza per day 
              weighted.mean(pizzas.per.day >= 1 & pizzas.unit=="day", wt, na.rm=T))
