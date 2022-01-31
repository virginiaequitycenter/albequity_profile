## ...........................
## Script name: living_standards_visuals.R
##
## Author: Michele Claibourn
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Generate tract characteristics for appendix 
##
## ...........................
## set working directory

setwd("data")


## ...........................
## load packages ----
library(tidyverse)
library(tidycensus)
library(kableExtra)


## ...........................
## Prep data ----

## Pull data
dp05 <-
  get_acs(year = 2019,
           geography = "tract",
           state = "VA",
           county = "003",
           table = "DP05",
           survey = "acs5", 
           cache = TRUE) 

## Generate labels
dp05_labels <- 
  load_variables(2019,
                 "acs5/profile", 
                 cache = TRUE) %>%
  rename(variable = name)

## Add labels and parse, keep percents, remove unneeded variables
dp05_table <- dp05 %>%
  left_join(dp05_labels) %>%
  separate(label,
           c("stat", "category", "group", "level", "restriction", "etc"),
           sep = "!!") %>%  
  filter(stat %in% c("Percent")) %>%
  filter(!is.na(estimate)) %>%
  mutate(
    final_level =
      case_when(
        category == "SEX AND AGE" & 
          group == "Total population" & 
          !is.na(level) ~ level,
        category == "SEX AND AGE" ~ group,
        category == "RACE" & 
          group == "Total population" & 
          level == "One race" & 
          !is.na(restriction) & 
          is.na(etc) ~ restriction,
        category == "RACE" & 
          group == "One race" & 
          !is.na(level) & is.na(restriction) ~ level,
        category == "RACE" & 
          group == "Total population" & 
          level == "Two or more races" & 
          is.na(restriction)  ~ level,
        category == "RACE" & 
          group == "Two or more races" & 
          is.na(level)  ~ group,
        category == "HISPANIC OR LATINO AND RACE" & 
          group == "Total population" & !is.na(level) & 
          is.na(restriction) ~ level,
        category == "HISPANIC OR LATINO AND RACE" & 
          !is.na(group)  & is.na(level) ~ group,
        TRUE ~ NA_character_ )) %>%
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
      "DP05_0027P",
      "DP05_0058P"
    )
  ) %>% 
  select(GEOID, NAME, variable, estimate, moe, category, final_level ) 

## reshape for tract table format
dp05_table <- dp05_table %>%
  select(-moe, -variable, -category) %>%
  distinct() %>% 
  pivot_wider(names_from = final_level, values_from = estimate) 

## reduce tract name, create age groups, combine race groups, select and order vars
dp05_table <- dp05_table %>% 
  mutate(NAME = str_replace(NAME, ", Albemarle County, Virginia", ""),
         NAME = str_replace(NAME, "Census Tract ", "")) %>% 
  mutate(`Remaining Races` = 
           `Native Hawaiian and Other Pacific Islander` + `Some other race`) %>% 
  mutate(`19 and under` = 
           `Under 5 years` + `5 to 9 years` + `10 to 14 years` + `15 to 19 years`,
         `20 to 44` = 
           `20 to 24 years` + `25 to 34 years` + `35 to 44 years`,
         `45 to 64` = 
           `45 to 54 years` + `55 to 59 years` + `60 to 64 years`,
         `65 and over` = 
           `65 to 74 years` + `75 to 84 years` + `85 years and over`) %>% 
  select(GEOID, Tract = NAME, `Total population`:`Female`, 
         `19 and under`:`65 and over`, White, `Black or African American`, 
         `Asian`, `American Indian and Alaska Native`, `Remaining Races`,
         `Two or more races`, `Hispanic or Latino` = `Hispanic or Latino (of any race)`)
  

## ...........................
## Make pretty table

## add tract descriptors
tract_names <- read_csv("tract_names.csv") %>%
  select(geoid, keypoints) %>% 
  mutate(GEOID = as.character(geoid)) %>% 
  select(-geoid)

dp05_table_final <- dp05_table %>% 
  left_join(tract_names) %>% 
  select(`Tract Number` = Tract, Description = keypoints, everything(), -GEOID)

## kableExtra
kbl(dp05_table_final, 
    align = c("l", "l", rep("c", 14)),
    caption = "Albemarle County Tract Demographics") %>%
  kable_classic_2("striped") %>% 
  add_header_above(
    c(" " = 3, "Gender" = 2, "Age" = 4, "Race and Ethnicity" = 7),
    italic = TRUE, color = "white", 
    background = c("#FFFFFF", "#D3D3D3", "#C0C0C0", "#808080")
    ) %>% 
  row_spec(0, bold = T, background = "#C0C0C0", align = "center") %>%
  column_spec(2, width = "30em") %>% 
  column_spec(3:16, width = "10em") %>% 
  footnote(
    general = "Estimates are from the 5-year American Community Survey, 2019, Table DP05.", 
    footnote_as_chunk = TRUE) %>%
  as_image(file = "../final_graphs/supplemental_tract_demo.pdf")
