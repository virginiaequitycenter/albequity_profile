## ---------------------------
## Script name: demographic_visuals.R
##
## Author:Sam Powers
## Date Created: 2021-02-02
##
## ---------------------------
## Purpose of script: Albemarle Equity Profile Visuals for Demographics and Overall AHDI 
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


## ---------------------------
## read in demographic data:
alb_dems <- read_csv("demographic_table.csv") %>%
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

pal2 <- c("#f4b255", "#41bf9b", "#3ca36f", "#6a6384", "#c4b8ca")

alb_pal <- c("#707d34", "#d27d29", "#f2c431", "#39657c", "#e4dbd2", "#6787ae", "#9b4c28")



# Historical race over time ----------------------------------------------
race_dec <- read_excel("albco_profile_raceovertime.xlsx")

race_dec_long <- race_dec %>% 
  select(-c(white_per, nonwhite_per)) %>% 
  pivot_longer(-c(year, total_wcc, total_se), names_to = "poptype", values_to = "pop") %>% 
  mutate(pop_percent = round((pop/total_se)*100,1),
         poptype = fct_recode(poptype, 
                              "White" = "white_se",
                              "Not White" = "nonwhite_se"))

dec_pal <- brewer.pal(5, "BuPu")[c(2,5)]

p <- ggplot(race_dec_long, aes(x = year, y = pop_percent, color = poptype)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(breaks = seq(1790,2010,10)) +
  scale_color_manual(values = dec_pal) +
  theme_classic() +
  labs(x="Year", y="Population %", title = "", color = "") +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    legend.position = "top",
    panel.grid.major.y = element_line(linetype = "dashed", size = .4),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

jpeg(filename = "../graphs/race_1790_2010.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

p

dev.off()




### Race --------------------------------------------------------------------
# Race Without White ------------------------------------------------------
# Or look at the over time trend excluding White?
race_trends_poc <-
  alb_dems %>%
  filter(category == "RACE" & final_level != "White")

race_trends_poc$final_level <- factor(race_trends_poc$final_level, levels = rev(c("Black or African American",
                                                                                  "Asian",
                                                                                  "American Indian and Alaska Native",
                                                                                  "Native Hawaiian and Other Pacific Islander",
                                                                                  "Two or more races",
                                                                                  "Some other race"))
)

race_trends_poc <-
  race_trends_poc %>%
  select(-category) %>%
  gather(year, pct, -final_level) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(final_level)) %>%
  mutate(pct2 = round(100*(pct/sum(pct)),1)) %>%
  mutate(height = cumsum(pct2) - pct2/2) %>%
  mutate(display = ifelse(pct2 > 2, paste0(round(pct2),"%"), ""))

midyear = ceiling((max(race_trends_poc$year) - min(race_trends_poc$year) + 1)/2)

poc_pal <- sample(brewer.pal(7, "BuPu")[-1], 6, replace = FALSE)

race_trends2 <- ggplot(race_trends_poc, aes(x = year, y = pct2, fill = final_level, group = final_level)) +
  geom_area(alpha=0.6 , size=.5, colour = "white") +
  
  # 2019 label
  geom_text(data = race_trends_poc %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by(final_level) %>%
              arrange(desc(year)) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1.2, hjust = 1, size = 2) +
  
  # 2011 Label
  geom_text(data = race_trends_poc %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by(final_level) %>%
              arrange(year) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 2) +
  
  # middle label
  geom_text(data = race_trends_poc %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by( final_level) %>%
              arrange(year) %>%
              slice(midyear),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 2) +
  
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0), limits = c(0, 101), breaks=seq(0,100, 25)) +
  scale_x_continuous( breaks=seq(2010,2019, 1), limits = c(2010, 2019))  +
  scale_fill_manual(values =c(rev(poc_pal))) +
  coord_cartesian(clip = 'off') +
  labs(x="Year", y="Population %", fill = "Demographic")  +
  theme_bw()+
  theme(
    plot.title = element_text( face="bold", hjust = 0, size = 12),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_text(face = "bold", vjust=-4.5, size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x=element_text(vjust=-5),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line =  element_line(color  = "white"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="bold", size = 10),
    panel.border = element_blank(),
    plot.margin=unit(c(t = .25, r = 1, b = 1.5, l = .1),"cm")
  )

jpeg(filename = "../graphs/race_poc_2000_2019.jpg", height = 30*72, width = 30*72, units = 'px', res = 300)

race_trends2

dev.off()

