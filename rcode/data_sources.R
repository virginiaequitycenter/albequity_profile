## ---------------------------
## Script name: data_sources.R
##
## Author:Sam Powers
## Date Created: 2020-12-10
##
## ---------------------------
## Purpose of script: To compile all of the relevant data sources for the albemarle equit profile 
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(readxl)
library(tidycensus)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

select <- dplyr::select
## ---------------------------
# census_api_key("", install = TRUE, overwrite = TRUE)


# What to pull from the Census --------------------------------------------
# County File & Tract File
# Tract Level Life Expectancy: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html <- use this for life expectancy for the tract disaggregated AHDI
# County Level Life Expectancy:  https://www.countyhealthrankings.org/app/virginia/2020/measure/outcomes/147/data

acs1_2019 <- load_variables(2019, "acs1/subject", cache = TRUE)
View(acs1_2019)


# Table 1: AHDI -----------------------------------------------------------
# How to Calculate HDI
# http://measureofamerica.org/Measure_of_America2013-2014MethodNote.pdf
# Use 2019 1 year estimates data.  
# income goal posts in 2019 dollars created with https://data.bls.gov/cgi-bin/cpicalc.pl

# County Level Life Expectancy
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx"
# destfile <- "health_rankings.xlsx"
# download.file(url, destfile)

life_expectancy_load <- read_excel("health_rankings.xlsx", sheet = 5, skip = 1) 

# Get Life Expectancies 
life_expectancies <-
  life_expectancy_load %>% 
  select(FIPS, State, County, contains("Life Expectancy")) %>% 
  rename_with(~tolower(str_replace_all(.x, "\\.", ""))) %>%
  rename_with(~tolower(str_replace_all(.x, " ", "_")))  %>%
  select(everything(), -contains("ci")) %>%
  gather(label, number, -fips, -state, -county) %>%
  separate(label, c("label", "demographic"), sep = "\\(") %>%
  mutate(demographic = str_replace_all(demographic, "\\)", ""),
         demographic = case_when(
           is.na(demographic) ~ "total",
           TRUE ~ demographic 
         ),
         county = case_when(
           is.na(county) ~ "total",
           TRUE ~ county 
         )
  ) %>%
  select(-label) %>%
  filter(county %in% c("total", "Albemarle", 
                       "Rockingham", "Augusta", 
                       "Harrisonburg", "Charlottesville City", 
                       "Fairfax", "Stafford"
  ))

# Census AHDI County Data -------------------------------------------------
# Pull ACS 1 for the few that you can and pull ACS 5 for charlottesville. 
# Get Fips Codes
data(fips_codes)
counties <-
  fips_codes %>%
  mutate(FIPS = paste0(state_code, county_code)) %>%
  filter(FIPS %in% unique(life_expectancies$fips))

##  - Median personal earnings of all workers with earnings ages 16 and older -- S2001_C01_002
##  - Percent high school graduate or higher -- S1501_C02_014
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent graduate degree or higher -- S1501_C02_013

tab1_labs <-
  tibble(
    variable = c("S2001_C01_002",
                 "S1501_C02_014",
                 "S1501_C02_015",
                 "S1501_C02_013"
    ),
    label = c("pers_earn",  "hs_grad", "bac_deg", "grad_deg")
  )

table1_dat <- 
  
# County Data Not Cville
  get_acs(
    year = 2019,
    geography = "county",
    state = "VA",
    county = counties$county_code,
    variables =  tab1_labs$variable,
    survey = "acs1"
  )  %>%
  select(GEOID, NAME, variable, estimate) %>%
  left_join(tab1_labs) %>%
  
  bind_rows(
# County Data Cville
    get_acs(
      year = 2019,
      geography = "county",
      state = "VA",
      county = "540",
      variables =  tab1_labs$variable,
      survey = "acs5"
    )  %>%
      select(GEOID, NAME, variable, estimate) %>%
      left_join(tab1_labs)
  
  ) %>%
  
  bind_rows(
    
# State Data
  get_acs(
    year = 2019,
    geography = "state",
    state = "VA",
    variables =  tab1_labs$variable,
    survey = "acs1"
  )  %>%
  select(GEOID, NAME, variable, estimate) %>%
  left_join(tab1_labs) %>%
    mutate(GEOID = "51000")
)

# Education Enrollment Data 
# tract_schl: 6 groups (3-4, 5-9, 10-14, 15-17, 18-19, 20-24) must be summed
#             population and enrolled, and divided
county_enroll <- get_acs(geography = "county", 
                         table = "S1401", 
                         state = "VA", 
                         county = counties$county_code, 
                         survey = "acs1", 
                         year = 2019, 
                         cache_table = TRUE)

cville_enroll <- get_acs(geography = "county", 
                         table = "S1401", 
                         state = "VA", 
                         county = "540", 
                         survey = "acs5", 
                         year = 2019, 
                         cache_table = TRUE)

state_enroll <- get_acs(geography = "state", 
                         table = "S1401", 
                         state = "VA", 
                    #     county = counties$county_code, 
                         survey = "acs1", 
                         year = 2019, 
                    cache_table = TRUE)

# County Level School Enrollment Data
county_schl_num <- county_enroll %>% 
  bind_rows(cville_enroll) %>%
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

county_schl_den <- county_enroll %>% 
  bind_rows(cville_enroll) %>%
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

county_schl_ratio <- left_join(county_schl_num, county_schl_den)

county_schl <- county_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(fips = GEOID, school_enroll = schlE)

# State Level School Enrollment Data 
state_schl_num <- state_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

state_schl_den <- state_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

state_schl_ratio <- left_join(state_schl_num, state_schl_den)

state_schl <- state_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(fips = GEOID, school_enroll = schlE) %>%
  mutate(fips = "51000")

# Enrollment totals
enrollment <-
county_schl %>%
  bind_rows(state_schl)

# Finalize Table 1 Data
table1_final <-
table1_dat %>%
  select(-variable) %>%
  spread(label, estimate) %>%
  rename(fips = GEOID) %>%
  select(-NAME) %>%
  left_join(life_expectancies %>%
              filter(demographic == "total") %>%
              rename(life_exp = number) %>% select(-demographic)) %>%
  left_join(enrollment) %>%
  mutate(
    ahdi_health = (life_exp - 66)/ (90-66)*10,
    ahdi_ed_attainment = ((hs_grad/100 + bac_deg/100 + grad_deg/100) - .5)/(2 - .5)*10,
    ahdi_ed_enroll = (school_enroll - 60)/(95 - 60) *10,
    ahdi_ed = (2*ahdi_ed_attainment/3) + (ahdi_ed_enroll/3),
    ahdi_income  = (log10(pers_earn)  -  log10(17234.09))   /(log10(72914) - log10(17234.09)) * 10
  ) %>%
  mutate(
         ahdi = (ahdi_health + ahdi_ed + ahdi_income)/3   ) %>%
  select(fips, county, everything(), -state)



# Racially Disaggregated AHDI in Albemarle --------------------------------

# Life Expectancy
le_alb <-
life_expectancies %>%
  filter(county == "Albemarle")


# Personal Earnings, HS graduate, bachelors degree, grad degree

tibble(
  variable = c("S2001_C01_002",
               "S1501_C02_014",
               "S1501_C02_015",
               "S1501_C02_013"
  ),
  label = c("pers_earn",  "hs_grad", "bac_deg", "grad_deg")
)


school_enroll_race <- 
  v19 %>%
  filter(
    label %in% c("Estimate!!Total:!!Enrolled in school:")     
  ) %>%
  filter(grepl("B14", name))  %>%
  pull(name)




# Tract Level AHDI Within Albemarle ---------------------------------------

# Tract Level Life Expectancym
# url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/XLSX/VA_A.XLSX"
# destfile <- "tract_expectancy.xlsx"
# download.file(url, destfile)

tract_expectancy_load <- read_excel("tract_expectancy.xlsx")

tract_expectancy <- 
  tract_expectancy_load %>%
  rename_with(
    ~tolower(
      str_replace_all(.x, 
                      " ", "_")
    )    
  ) %>%
  rename(GEOID = tract_id, state_fips = state2kx, county_fips = cnty2kx, tract_fips = tract2kx, life_expectancy = `e(0)`, se = `se(e(0))` ) %>%
  select(-abridged_life_table_flag) %>%
  mutate(fips = paste0(state_fips, county_fips)) %>%
  filter(county_fips == "003")

# Personal Earnings, HS Grad, Bac Degree, Grad Degree
tract_facts <- get_acs(geography = "tract",
                        variables = tab1_labs$variable,
                        state = "VA", 
                        county = "003", 
                        survey = "acs5",
                        year = 2019) %>%
                    select(GEOID, NAME, variable, estimate) %>%
                    left_join(tab1_labs)


# School enrollment
tract_enroll <- get_acs(geography = "tract", 
                         table = "S1401", 
                         state = "VA", 
                         county = "003", 
                         survey = "acs5", 
                         year = 2019, 
                         cache_table = TRUE)


# Tract Level School Enrollment Data
tract_schl_num <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

tract_schl_den <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

tract_schl_ratio <- left_join(tract_schl_num, tract_schl_den)

tract_schl <- tract_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100, 1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1)) %>%
  select(GEOID, school_enroll = schlE)


# Put it all together
tract_ahdi <-
tract_facts %>%
select(-variable) %>%
  spread(label, estimate) %>%
  select(-NAME) %>%
  left_join(tract_expectancy %>%
              select(GEOID, life_exp = life_expectancy)
              ) %>% 
  left_join(tract_schl) %>%
  mutate(
    ahdi_health = (life_exp - 66)/ (90-66)*10,
    ahdi_ed_attainment = ((hs_grad/100 + bac_deg/100 + grad_deg/100) - .5)/(2 - .5)*10,
    ahdi_ed_enroll = (school_enroll - 60)/(95 - 60) *10,
    ahdi_ed = (2*ahdi_ed_attainment/3) + (ahdi_ed_enroll/3),
    ahdi_income  = (log10(pers_earn)  -  log10(17234.09))   /(log10(72914) - log10(17234.09)) * 10
  ) %>%
  mutate(
    ahdi = (ahdi_health + ahdi_ed + ahdi_income)/3  ) 
  

# Pull racial breakdowns 2000 - 2019
# Pull Ethnicity breakdowsn 2000 - 2019

# Nativity in 2019
# Specific Nativity in 2019 [New Albemarlians. Albemarlites??]



# Median Household Income -------------------------------------------------
# https://www.census.gov/data-tools/demo/saipe/#/?map_geoSelector=mhi_s&map_yearSelector=2018&s_year=2018,2009&s_state=51&s_county=51003&s_measures=mhi_snc


# Look into the Gender Pay Gap <- lowest priority of anything. 


# Cost of Living ALICE Estimates. We do have that.  -----------------------

# Family size by % of AMI. 






