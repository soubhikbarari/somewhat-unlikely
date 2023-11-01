library(tidyverse)
library(plotly)

gss7222_r1 <- haven::read_dta("GSS_stata/gss7222_r1.dta")

table(gss7222_r1$year)

# Column names ------------------------------------------------------------

gss7222_r1.cols <- suppressWarnings(
  stack(sapply(colnames(gss7222_r1), 
               function(.) attributes(gss7222_r1[[.]])$label[1]))
)
View(gss7222_r1.cols)

# Missingness -------------------------------------------------------------

gss7222_r1.missing.yr <- gss7222_r1 %>%
  select(-id) %>%
  group_by(year) %>%
  summarise(across(everything(), ~mean(is.na(.x)))) 

gss7222_r1.missing.yr.long <- gss7222_r1.missing.yr %>%
  gather(key="var", val="missing", -year) %>%
  group_by(var) %>%
  mutate(missing.avg = mean(missing)) %>%
  ungroup() %>%
  arrange(missing.avg) %>%
  mutate(var = as_factor(var))

gss7222_r1.cols %>%
  left_join(gss7222_r1.missing.yr.long %>% 
              distinct(ind=var, missing.avg)) %>%
  arrange(missing.avg) %>%
  View()

gss7222_r1.missing.yr.long %>%
  group_by(var) %>%
  filter(cur_group_id() < 50) %>%
  ungroup() %>%
  ggplot(aes(y=var, x=year, z=missing)) +
  geom_tile(aes(fill=missing)) +
  scale_fill_viridis_c() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=0))

plot_ly(data = gss7222_r1.missing.yr,
  x = ~as.character(year),
  y = colnames(gss7222_r1.missing.yr)[2:ncol(gss7222_r1.missing.yr)],
  z = 100*t(as.matrix(gss7222_r1.missing.yr[,2:ncol(gss7222_r1.missing.yr)])),
  type = "heatmap"
)
