## ---------------------------
## Script name: profile_visuals.R
##
## Author:Sam Powers
## Date Created: 2021-01-04
##
## ---------------------------
## Purpose of script: To create figures for the albemarle equity profile
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:
alb_dems <- read_csv("demographic_table.csv")



# Race over time ----------------------------------------------------------

race_trends <-
alb_dems %>%
  filter(category == "RACE") %>%
  select(-category) %>%
  gather(year, pct, -final_level) 

ggplot(race_trends, aes(x = year, y = pct))



