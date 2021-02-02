## ---------------------------
## Script name: ahdi_visuals.R
##
## Author:Sam Powers
## Date Created: 2021-02-02
##
## ---------------------------
## Purpose of script: Albemarle Equity Profile Visuals for ahdi
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(readxl)
library(viridis)
library(ggforce) # for 'geom_arc_bar'
library(RColorBrewer)
library(ggnewscale)
library(tigris)
library(scales)
library(ggforce)
library("ggspatial")

select <- dplyr::select

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation



