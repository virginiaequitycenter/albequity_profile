library(tidyverse)
library(readxl)

setwd("~/Box Sync/mpc/dataForDemocracy/albequity_profile/")

albco <- read_excel("data/albco_profile_raceovertime.xlsx")
ggplot(albco, aes(x = year, y = white_per)) + 
  geom_line(color = "blue") + 
  geom_line(aes(y = nonwhite_per), color = "red")

albco_long <- albco %>% 
  select(-c(white_per, nonwhite_per)) %>% 
  pivot_longer(-c(year, total_wcc, total_se), names_to = "poptype", values_to = "pop") %>% 
  mutate(pop_percent = round((pop/total_se)*100,1))

ggplot(albco_long, aes(x = year, y = pop_percent, color = poptype)) +
  geom_line()
