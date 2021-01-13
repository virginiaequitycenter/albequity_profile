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

acs1 <- load_variables(2019, "acs1", cache = TRUE)
acs5 <- load_variables(2019, "acs5", cache = TRUE)
acs1_2019 <- load_variables(2019, "acs1/subject", cache = TRUE)
View(acs1_2019)
acs1_2019_prof <- load_variables(2019, "acs1/profile", cache = TRUE)
View(acs1_2019_prof)



# Full Census Demographic Breakdown of Albemarle --------------------------

## Try to pull the 2010 census estimates 
## 

demographic_tables <-
  map_df(2019:2010,
         ~ get_acs(
           year = .x,
           geography = "county",
           state = "VA",
           county = "003",
           # variables =  race_vars$variable,
           table = "DP05",
           survey = "acs1", 
           cache = TRUE
         ) %>%
           mutate(year = .x)
  )

View(demographic_tables)

acs1_dp_labels <- 
  map_df(2019:2010,
        ~ load_variables(.x, 
                         "acs1/profile", 
                         cache = TRUE) %>%
          mutate(year = .x)
         ) %>%
rename(variable = name)


alb_demographic_profile_2010_2019 <-
demographic_tables %>%
  left_join(acs1_dp_labels) %>%
  separate(label,
           c("stat", "category", "group", "level", "restriction", "etc"),
           sep = "!!") %>%  
  filter(stat %in% c("Percent", "Percent Estimate"))  %>%
  filter(!is.na(estimate)) %>%
  mutate(
    final_level =
      case_when(
        category == "SEX AND AGE" &
            group == "Total population" & !is.na(level) ~ level,
        category == "SEX AND AGE"  ~ group,
        category == "RACE" & group == "Total population" & level == "One race" & !is.na(restriction) & is.na(etc) ~ restriction,
        category == "RACE" & group == "One race" & !is.na(level) & is.na(restriction) ~ level,
        category == "RACE" & group == "Total population" & level == "Two or more races" & is.na(restriction)  ~ level,
        category == "RACE" & group == "Two or more races" & is.na(level)  ~ group,
        category == "HISPANIC OR LATINO AND RACE" & group == "Total population" & !is.na(level) & is.na(restriction) ~ level,
        category == "HISPANIC OR LATINO AND RACE" & !is.na(group)  & is.na(level) ~ group,
        
        TRUE ~ NA_character_
        
      )
  ) %>%
  filter(
    !final_level %in% c(
      "Under 18 years",
      "16 years and over",
      "18 years and over",
      "21 years and over",
      "62 years and over",
      "65 years and over"
    ),
    !is.na(final_level),
    !variable %in% c(
      "DP05_0023P",
      "DP05_0024P",
      "DP05_0026P",
      "DP05_0027P"
    )
  ) %>% 
  select(variable, estimate, moe, year, category, final_level ) 

View(alb_demographic_profile_2010_2019)

alb_demographic_table <-
alb_demographic_profile_2010_2019 %>%
  select(-moe, - variable) %>%
  distinct() %>%
  spread(year, estimate) #%>%
 # select(-category) %>%
 # distinct()

alb_demographic_table %>% View()

write_csv(alb_demographic_table, path  = "demographic_table.csv")


# 2019 Age By Sex ---------------------------------------------------------


sex_age_pull <-
         get_acs(
           year = 2019,
           geography = "county",
           state = "VA",
           county = "003",
           # variables =  race_vars$variable,
           table = "B01001",
           survey = "acs1", 
           cache = TRUE
         )

sex_age <-
sex_age_pull %>%
  left_join(acs1 %>%
              rename(variable = name)) %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label,
           c(NA, "Total", "sex", "level"),
           sep = "!!")  %>%
  filter(!is.na(sex), !is.na(level)) %>%
  select(sex, level, estimate) %>%
  mutate(
    level = str_replace_all(level, " ", ""),
    level = str_replace_all(level, "years", ""),
    level = str_replace_all(level, "Under", "0to"),
    level = str_replace_all(level, "over", "500")
    ) %>% 
  separate(level, c("start", "end"), sep = "to|and") %>% 
  mutate(
    end = case_when(
      is.na(end) ~ start,
      TRUE ~ end
    ),
    start = as.numeric(start),
    end = as.numeric(end),
    age_group = 
      case_when(
        end < 6 ~ "Under 5 years",
        start > 4 & end < 10 ~ "5 to 9 years",
        start > 9 & end < 15 ~ "10 to 14 years",
        start > 14 & end < 20 ~ "15 to 19 years",
        start > 19 & end < 25 ~ "20 to 24 years",
        start > 24 & end < 35 ~ "25 to 34 years",
        start > 34 & end < 45 ~ "35 to 44 years",
        start > 44 & end < 55 ~ "45 to 54 years",
        start > 54 & end < 60 ~ "55 to 59 years",
        start > 59 & end < 65 ~ "60 to 64 years",
        start > 64 & end < 75 ~ "65 to 74 years",
        start > 74 & end < 85 ~ "75 to 84 years",
        start > 84 ~ "85 years and over"
      )
  ) %>% 
  group_by(sex, age_group) %>%
  summarize(estimate = sum(estimate))


write_csv(sex_age, path  = "sex_age.csv")


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

table1_final


write_csv(table1_final, path = "ahdi_table.csv")


# Racially Disaggregated AHDI in Albemarle --------------------------------

# Life Expectancy
# County Rankings source. 
le_alb <-
life_expectancies %>%
  filter(county == "Albemarle")

le_alb

# We might only get differential life expectancy tbh

# Racially Disaggregated Educational Achievement 
# Need to use the 5 year estimates for this one. 

# "hs_grad", "bac_deg", "grad_deg"

# These do not have most races in them. Just Black & White
race_disag_ed_1B <-
  map_df(c("B15002A","B15002B", "B15002C", "B15002D", "B15002E", "B15002F", "B15002G", "B15002H", "B15002I"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs1",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name))
  )

cleaned_white_black_ahdi_ed <-
race_disag_ed_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!") %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  ) %>%
  separate(concept, c(NA, "Race"), sep = "\\(") %>%
  mutate(Race = str_replace_all(Race, "\\)", ""))  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
     Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  group_by(Race, Sex) %>%
  mutate(Total = sum(Total))  %>% 
  group_by(Race, Sex, Level) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), Total = min(Total, na.rm = TRUE)) %>% 
  filter(estimate > 0) %>%
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
 # mutate(pct = paste0( round(estimate/Total * 100, 2), "%")) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct) %>%
  mutate(hs_grad = `Graduate or professional degree` + `Bachelor's degree` + `Associate's degree` + `Some college, no degree` + `GED or alternative credential` + `Regular high school diploma`,
         bac_deg = `Graduate or professional degree` + `Bachelor's degree`,
         grad_deg = `Graduate or professional degree`) %>%
  select(Race, hs_grad, bac_deg, grad_deg)

cleaned_white_black_ahdi_ed

# Just need black & white enrollment

## Getting the right age-grouped numerators is not a super viable option for race -- uggh. 
alb_race_enroll <- get_acs(geography = "county", 
                         table = "C14007A", 
                         state = "VA", 
                         county = "003", 
                         survey = "acs1", 
                         year = 2019, 
                         cache_table = TRUE) 




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


tract_ahdi %>% View()

summary(tract_ahdi$ahdi)

write_csv(tract_ahdi, path = "tract_ahdi.csv")
# Education Section -------------------------------------------------------

# These do not have most races in them. Just Black & White
race_disag_ed_1B <-
  map_df(c("B15002A","B15002B", "B15002C", "B15002D", "B15002E", "B15002F", "B15002G", "B15002H", "B15002I"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs1",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name))
  )

# These do not have the Grad degrees in them Still missing some data 
race_disag_ed_1C <-
  map_df(c("C15002A","C15002B", "C15002C", "C15002D", "C15002E", "C15002F", "C15002G", "C15002H", "C15002I"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs1",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name))
  )

View(race_disag_ed_1C)

# "K201501" # Supplemental table 1 - tidycensus will not pull
race_disag_ed_1K <-
  map_df(c( "K201501B"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs1",
                   year = 2019)  #%>%
         #  left_join(acs1 %>% rename(variable = name))
  )


# These do not exist???
race_disag_ed_5B <-
  map_df(c("B15002A","B15002B", "B15002C", "B15002D", "B15002E", "B15002F", "B15002G", "B15002H", "B15002I"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs5",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name))
  )


# No Grad, but All races. 
race_disag_ed_5C <-
  map_df(c("C15002A","C15002B", "C15002C", "C15002D", "C15002E", "C15002F", "C15002G", "C15002H", "C15002I"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs5",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name))
  )


disag_ed_1C_total <-
  map_df(c("C15002"),
         ~ get_acs(geography = "county",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs1",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name))
  )

## Add Sex into this one. 

## This does not have graduate level and above in it. We need something better from somewhere
ed_table_race_total <-

race_disag_ed_5C %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!") %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  ) %>%
  separate(concept, c(NA, "Race"), sep = "\\(") %>%
  mutate(Race = str_replace_all(Race, "\\)", ""))  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
      Sex == "Both" & Level == "All" ~ estimate,
      TRUE ~ 0
                    )  
  ) %>%
  group_by(Race) %>%
  mutate(Total = sum(Total))  %>% 
  group_by(Race, Level) %>%
  summarize(estimate = sum(estimate), Total = min(Total)) %>% 
 filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct) %>%
  mutate(Sex = "All")

ed_table_race_total


ed_table_sex_race_disag <-
  race_disag_ed_5C %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!") %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  ) %>%
  separate(concept, c(NA, "Race"), sep = "\\(") %>%
  mutate(Race = str_replace_all(Race, "\\)", ""))  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
    Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  group_by(Race, Sex) %>%
  mutate(Total = sum(Total))  %>% 
  group_by(Race, Sex, Level) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), Total = min(Total, na.rm = TRUE)) %>% 
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct)

ed_table_sex_race_disag
names(ed_table_sex_race_disag)


ed_table_sex_total <- 
disag_ed_1C_total %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!")  %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                  TRUE ~ Sex),
  
    Level = case_when(is.na(Level)  ~ "All",
                    TRUE ~ Level)
  )  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
     Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
  group_by(Sex) %>%
  mutate(Total = sum(Total)) %>%
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total, -concept, -variable, -moe) %>%
  spread(Level, pct) %>%
  transmute(
    Race = "All",
    Sex = Sex,
    `Less than high school diploma` = `Less than 9th grade` + `9th to 12th grade, no diploma`,
    `High school graduate (includes equivalency)` = `High school graduate (includes equivalency)`,
    `Some college or associate's degree` = `Associate's degree` + `Some college, no degree`,
    `Bachelor's degree or higher` = `Bachelor's degree` + `Graduate or professional degree`,
  )

ed_table_sex_total
names(ed_table_sex_total)



ed_table_total <-
  disag_ed_1C_total %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!")  %>%
  mutate(
    Sex = case_when(is.na(Sex) ~ "Both",
                    TRUE ~ Sex),
    
    Level = case_when(is.na(Level)  ~ "All",
                      TRUE ~ Level)
  )  %>%
  select(-Estimate, -Total) %>%
  mutate(
    Total = case_when(
      Sex == "Both" & Level == "All" ~ estimate,
      TRUE ~ 0
    )  
  ) %>%
 # group_by(Sex) %>%
  mutate(Total = sum(Total)) %>%
  group_by( Level) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), Total = min(Total, na.rm = TRUE))  %>% 
  filter(!Level %in% c("All")) %>%
  mutate(pct = estimate/Total * 100) %>%
  select(-estimate, -Total) %>%
  spread(Level, pct) %>%
  transmute(
    Race = "All",
    Sex = "All",
    `Less than high school diploma` = `Less than 9th grade` + `9th to 12th grade, no diploma`,
    `High school graduate (includes equivalency)` = `High school graduate (includes equivalency)`,
    `Some college or associate's degree` = `Associate's degree` + `Some college, no degree`,
    `Bachelor's degree or higher` = `Bachelor's degree` + `Graduate or professional degree`,
  )

ed_table_total
names(ed_table_total)

final_ed_table <-
bind_rows(
ed_table_race_total,
ed_table_sex_race_disag,
ed_table_sex_total,
ed_table_total 
)


View(final_ed_table)
names(final_ed_table)
final_ed_table %>%
  rename(`High school graduate` = `High school graduate (includes equivalency)`) %>%
  gather(degree, percent, -c(Race, Sex)) %>%
  write_csv(., path = "education_distrbution.csv")

View(final_ed_table)

# Tract Level Education ---------------------------------------------------
# collapse this as finishing vs not finishing bachelors degrees 
disag_ed_5B_tract <-
  map_df(c("B15002"),
         ~ get_acs(geography = "tract",
                   table = .x,
                   state = "VA", 
                   county = "003", 
                   survey = "acs5",
                   year = 2019)  %>%
           left_join(acs5 %>% rename(variable = name))
  )

geo_ed <-
disag_ed_5B_tract %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Sex", "Level"), sep = "!!")  %>%
  mutate(Sex = case_when(
    is.na(Sex) ~ "All",
    TRUE ~ Sex
    ),
  Level = case_when(
    is.na(Level) & Sex == "All" ~ "Total",
    TRUE ~ Level
    )
  ) %>%
  select(GEOID, estimate, Sex, Level) %>%
  group_by(GEOID, Level) %>%
  summarize(estimate = sum(estimate)) %>%
  filter(!is.na(Level)) %>%
  mutate(
    bac = case_when(
      Level %in% c("Bachelor's degree", "Doctorate degree", "Master's degree", "Professional school degree") ~ "bac",
      Level %in% c("Total") ~ "tot",
      TRUE ~ "less"
    )
  ) %>%
  group_by(GEOID, bac) %>% 
  summarize(est = sum(estimate)) %>%
  spread(bac, est) %>%
  mutate(perc_bac = bac/tot*100)
  
write_csv(geo_ed, path = "geographic_education.csv")

# Nativity ----------------------------------------------------------------

# Nativity in 2019

nativity_1B <-
         get_acs(geography = "county",
                   table = "B05012",
                   state = "VA", 
                   county = "003", 
                   survey = "acs1",
                   year = 2019)  %>%
           left_join(acs1 %>% rename(variable = name)
                     )

nativity <-
nativity_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Nativity"), sep = "!!") %>%
  mutate(denom = case_when(is.na(Nativity) ~ estimate,
                           TRUE ~ 0)) %>%
  group_by(NAME) %>%
  mutate(denom = sum(denom),
         pct = estimate / denom * 100) %>%
  ungroup() %>%
  select(Nativity, pct) %>%
  filter(!is.na(Nativity)) 

nativity

citizenship_1B <-
  get_acs(geography = "county",
          table = "B05001",
          state = "VA", 
          county = "003", 
          survey = "acs1",
          year = 2019)  %>%
  left_join(acs1 %>% rename(variable = name)
  )

citizenship <-
citizenship_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  separate(label, c("Estimate", "Total", "Citizenship"), sep = "!!") %>%
  mutate(denom = case_when(is.na(Citizenship) ~ estimate,
                           TRUE ~ 0)) %>%
  group_by(NAME) %>%
  mutate(denom = sum(denom),
         pct = estimate / denom * 100) %>%
  ungroup() %>%
  select(Citizenship, pct) %>%
  filter(!is.na(Citizenship)) 

citizenship

# Origin of foreign born
# B05006

origin_1B <-
  get_acs(geography = "county",
          table = "B05006",
          state = "VA", 
          county = "003", 
          survey = "acs5",
          year = 2019)  %>%
  left_join(acs5 %>% rename(variable = name)
  )


origin <-
origin_1B %>%
  mutate(label = str_replace_all(label, ":", "")) %>%
  
  separate(label, c("Estimate", "Total", "Continent", "Country"), sep = "!!")  %>%
  filter(!is.na(Continent), is.na(Country)) %>%
  select(Continent, estimate)
  

save(origin, citizenship, nativity, file = "origins.Rda")

# Specific Nativity in 2019 [New Albemarlians. Albemarlites??]
# Median Household Income -------------------------------------------------
# https://www.census.gov/data-tools/demo/saipe/#/?map_geoSelector=mhi_s&map_yearSelector=2018&s_year=2018,2009&s_state=51&s_county=51003&s_measures=mhi_snc



# Look into the Gender Pay Gap <- lowest priority of anything. 



# Cost of Living ALICE Estimates. We do have that.  -----------------------
# https://www.unitedforalice.org/virginia
# url <- "https://www.unitedforalice.org/Attachments/StateDataSheet/DataSheet_VA.xlsx"
# destfile <- "alice_va.xlsx"
# download.file(url, destfile)


# Stacked Area Chart of Poverty, Alice, + Alice
sheets <- excel_sheets("alice_va.xlsx")

alice_va <- read_excel("alice_va.xlsx", sheet = sheets[2]) %>%
  rename_with(~ tolower(str_replace_all(.x, ":|-| ", "_") )
              ) %>%
  filter(geo.id2 == "51003")

alice_alb <-
alice_va %>%
  select(year, household, poverty_household, alice_household, above_alice_household) %>%
  gather(level, number, -year, -household) %>%
  mutate(pct = number/household*100 )
  

write_csv(alice_alb, path = "alice_alb_hhs.csv")

# Alice Level & Median HHInc by Race
# S1901 
# B19013

#  Household incomes by race. 
# B190001A-I

table_list <- c(
"B19013",
"B19013A", 
"B19013B", 
"B19013C", 
"B19013D", 
"B19013E", 
"B19013F", 
"B19013G", 
"B19013H",
"B19013I"  
)

year_list <- seq(2010,2018, 2)
year_tables <- expand.grid(table_list, year_list)

med_hhinc <- 
  map2_df(year_tables$Var1, year_tables$Var2,
 ~ get_acs(geography = "county", 
                               table = .x, 
                               state = "VA", 
                               county = "003", 
                               survey = "acs5", 
                               year = .y, 
                               cache_table = TRUE)  %>%
   mutate(year = .y)

)


# Alice Threshold vs. Median income together. 

alice_hhinc_thresh <-
med_hhinc %>%
  left_join(acs5 %>% rename(variable = name)) %>%
 separate(concept, c(NA, NA, "race"), sep = "\\(") %>%
  mutate(
    race = case_when(
    is.na(race) ~ "Overall",
    TRUE ~ str_trim(str_replace_all(proper(race), "\\)|Householder|Alone", ""))
    )
  ) %>%
  select(year, race, `Median Household Income` = estimate) %>%
left_join(
alice_va %>%
  select(year, `ALICE Threshold` = alice_threshold___hh_under_65) 
) #%>%
#  gather(estimate, stat, -year, -race)

write_csv(alice_hhinc_thresh, path = "alice_thresh.csv")


# Gini Index to match Alice at county level ACS1. 




# Cost Burdened Renters ---------------------------------------------------

# Cost Burdened Renters
# B25074 or
# B25070

county_housing_cost <- get_acs(geography = "county", 
                               table = "B25070", 
                               state = "VA", 
                               county = "003", 
                               survey = "acs1", 
                               year = 2019, 
                               cache_table = TRUE)


tract_housing_cost <- get_acs(geography = "tract", 
                               table = "B25070", 
                               state = "VA", 
                               county = "003", 
                               survey = "acs5", 
                               year = 2019, 
                               cache_table = TRUE)

# County level stats. 
county_housing <-
county_housing_cost %>%
  left_join(acs1 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
    ),
    "_percent",
    "")
  ) %>%
  
  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
         
  ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`) %>%
  mutate(across(c(Burdened, `Severely Burdened`, `Not Burdened`), ~.x/denom)) %>%
  mutate(geoid = "total", county_type = "County")


# Tract level stats. 
albemarle_housing_costs <- 
tract_housing_cost %>%
  left_join(acs1 %>% rename(variable = name)) %>%
  separate(label, c(NA, "Total", "level"), sep = "!!")  %>%
  mutate(level = case_when(is.na(level) ~ "Total",
                           TRUE ~ level)) %>%
  select(estimate, level, GEOID)  %>%
  spread(level, estimate) %>%
  rename_with( ~ str_replace_all(
    tolower(
      str_replace_all(.x, " ", "_")
      ),
           "_percent",
            "")
    ) %>%

  mutate(denom = total - not_computed,
         Burdened = `30.0_to_34.9` + `35.0_to_39.9` + `40.0_to_49.9`,
         `Severely Burdened` =  `50.0_or_more`,
         `Not Burdened` = less_than_10.0 + `10.0_to_14.9` + `15.0_to_19.9` + `20.0_to_24.9` +`25.0_to_29.9`
           
           ) %>%
  select(geoid, denom, Burdened, `Severely Burdened`, `Not Burdened`) %>%
  mutate(across(c(Burdened, `Severely Burdened`, `Not Burdened`), ~.x/denom)) %>%
  mutate(county_type = "Census Tracts") %>%
  bind_rows(county_housing) 

write_csv(albemarle_housing_costs, path = "housing_costs.csv")
  



