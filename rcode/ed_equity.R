library(tidyverse)
## ---------------------------
## Script name: 
##
## Author:Sam Powers
## Date Created: 2020-12-08
##
## ---------------------------
## Purpose of script:
##   
##
## ---------------------------
## set working directory

setwd() 

## ---------------------------
## load up the packages we will need:

library(tidyverse)
options(scipen = 6, digits = 8) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:
# Albemarle Equity Table --------------------------------------------------
all <- 14318
gifted <- 1188
sped <- 1188


all  <- c(14318, 1188, 1851, 984, 721, 222, 315, 1039, 677, 677)
asian <- c(725, 88, 66, 47, 14, 0, 0, 77, 37, 45)
black <- c(1562, 42, 367, 137, 117, 47, 66, 52, 29, 104)
hispanic <- c( 2037, 52, 262, 170, 132,  27, 34, 68, 46, 96)
white <- c(9089,  932, 1041, 567, 414, 125, 178, 800, 530, 703)
races2 <- c( 905, 74, 115, 63, 44, 23, 37, 42, 35, 58)
econ_disadv <- c(4679, 121, 902,563,355, 148, 224, 163, 85, 243)
els  <- c(1466, 20, 176, 129,  88, 12, 16, 30, 17, 52)
swd <- c(1855, 24, 1855,  223, 143, 94, 146, 31, 13, 112)

eq2019 <- rbind(
  all,
  asian,
  black,
  hispanic,
  white,
  races2,
  econ_disadv,
  els,
  swd
)

dimnames(eq2019)[[2]] <- c("total", "gifted" , "swd", "absent", "absent_period", "suspended", "suspension_incidents", "ms_inhs_math", "advanced", "ontime_grad")
dat <- as_tibble(eq2019)
dat$pop <- rownames(eq2019)



dat %>%
  mutate( sus_pct = suspended/total * 100)




