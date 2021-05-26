# Collect data for online webstory
# May 26, 2021 mpc

library(tidyverse)
library(readxl)
library(tigris)


# Figure 1 (1 in report) ----
race_dec <- read_excel("data/albco_profile_raceovertime.xlsx")


# Figure 2 (2 in report) ----
alb_dems <- read_csv("data/demographic_table.csv") %>%
  mutate(
    category =
      case_when(
        final_level == "Male" ~ "SEX",
        final_level == "Female" ~ "SEX",
        grepl("years", final_level) ~ "AGE",
        final_level == "Hispanic or Latino (of any race)" ~ "Ethnicity",
        final_level == "Not Hispanic or Latino" ~ "Ethnicity",
        TRUE ~ category
      )
  ) %>%
  filter(!final_level == "Total population")


# Table 1 (1 in report) ----
ahdi_table <- read_csv("data/ahdi_table.csv") %>%
  mutate(
    county = case_when(county == "total" ~ "Virginia",
                       TRUE ~ county
    ))


# Figure 3 (5a in report) ----
tract_names <- read_csv("data/tract_names.csv") %>%
  select(-contains("X"))

tract_ahdi <- read_csv("data/tract_ahdi.csv") %>%
  rename_with(~tolower(.x))  %>%
  left_join(tract_names)

alb_tract <- tracts(state = "VA", county = "003" )


# Figure 4 (6 in report) ----
# data already loaded


# Figure 5 (8 in report) ----
life_expectancy_load <- read_excel("data/health_rankings.xlsx", sheet = 5, skip = 1) 


unique(life_expectancies$ci)

life_expectancies <-
  life_expectancy_load %>% 
  select(FIPS, State, County, contains("Life Expectancy")) %>% 
  rename_with(~tolower(str_replace_all(.x, "\\.", ""))) %>%
  rename_with(~tolower(str_replace_all(.x, " ", "_")))  %>%
  gather(label, number, -fips, -state, -county) %>%
  separate(label, c("label", "demographic"), sep = "\\(") %>%
  separate(demographic, c("demographic", "ci"), sep = "\\)") %>%
  
  mutate(demographic = str_replace_all(demographic, "\\)", ""),
         demographic = case_when(
           is.na(demographic) ~ "total",
           TRUE ~ demographic 
         ),
         
         ci = str_replace_all(ci, "_95%_ci_-_", ""),
         ci = case_when(
           ci %in% c("low", "high") ~ ci,
           TRUE ~ "mean"
         )
         
  ) %>%
  select(-label) %>%
  filter(county %in% c("Albemarle")) %>%
  spread(ci, number) %>%
  mutate(
    demo = case_when(demographic == "total" ~ "Overall",
                     TRUE ~ "Race/Ethnicity"
    ),
    
    demographic = case_when(
      demographic == "total" ~ "Albemarle County",
      TRUE ~ str_to_sentence(demographic)
    )
    
  ) %>%
  filter(!is.na(mean))

## this figure isn't in github code...


# Figure 6 (9a in report) ----
life_exp <- read_csv("data/tract_ahdi.csv") %>%
  rename_with(~ tolower(.x))  %>%
  left_join(tract_names) %>%
  select(geoid, keypoints, life_exp) %>%
  mutate(geo = "Census Tract")


# Figure 7 (13 in report) ----
## couldn't find "proper", so found it online and added it here
proper <- function(x, just_first_word = TRUE) {
  if (just_first_word) {
    paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  } else {
    gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl = TRUE)
  }
}

ed_dist <-read_csv("data/education_distrbution.csv") %>%
  mutate(Race = proper(Race, FALSE)) %>%
  mutate(
    degree = factor(degree,
                    levels = rev(c(
                      "Less than high school diploma",
                      "High school graduate",
                      "Some college or associate's degree",
                      "Bachelor's degree or higher"
                    ))
    ),
    Race = factor(Race,
      levels = rev(c("All",
                     "White Alone",
                     "Black Or African American Alone",
                     "Asian Alone",
                     "Native Hawaiian And Other Pacific Islander Alone",
                     "American Indian And Alaska Native Alone",
                     "Two Or More Races",
                     "Some Other Race Alone",
                     "Hispanic Or Latino"
                     #  "White Alone, Not Hispanic Or Latino"
      ))
    )
  ) %>%
  filter(!is.na(Race))


# Figure 8 (14b in report) ----
ed_tract <- read_csv("data/geographic_education.csv")

alb_tract2 <- alb_tract %>%
  mutate(GEOID = as.numeric((GEOID)))

ed_geo_tract <- alb_tract2 %>%
  left_join(ed_tract) %>%
  mutate(perc_bac = perc_bac/100)


# Figure 9 (18 in report) ----
med_hh_inc <- read_csv("data/med_inc_tract.csv") 

med_hhinc_tract_map <-
  alb_tract2 %>% left_join(med_hh_inc 
                          %>% select(-NAME) )


# Figure 10 (19 in report) ----
## don't see this code in github
sheets <- excel_sheets("data/alice_va.xlsx")

alice_alb <- read_excel("data/alice_va.xlsx", sheet = sheets[5]) %>%
  rename_with(~ tolower(str_replace_all(.x, ":|-| ", "_") )
  ) %>%
  filter(str_detect(geo.id2, "51003"))

alice_alb <-
  alice_alb %>%
  select(year, name, households, poverty_household, alice__household, above_alice_household) %>%
  gather(level, number, -year, -name, -households) %>%
  mutate(pct = number/households*100 )


# Figure 11 (21 in report) ----
alice_thresh <- read_csv("data/alice_thresh.csv")


# SAVE ----
rm(sheets, proper)

save.image("webstory/webstory_data.RData")
# load("webstory/webstory_data.RData")