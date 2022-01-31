## ...........................
## Script name: demographic_visuals.R
##
## Authors: Michele Claibourn, Sam Powers
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Analysis and visuals for demographic section 
##
## ...........................
## set working directory

# setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")
setwd("data")

## ...........................
## load packages ----

library(tidyverse)
library(readxl)
library(viridis)
library(ggforce) # for 'geom_arc_bar'
library(RColorBrewer)
library(ggnewscale)
library(scales)
library("ggspatial")

options(scipen = 6, digits = 4) # to view outputs in non-scientific notation
select <- dplyr::select # avoid function name conflicts


## ...........................
## read in data ----

## ACS data
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

## Historical race data
race_dec <- read_excel("albco_profile_raceovertime.xlsx")


## ...........................
## Race visuals ----

## Historical race figure
race_dec_long <- race_dec %>% 
  #filter(year < 2019) %>% 
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
  scale_x_continuous(breaks = seq(1790,2020,10)) +
  scale_color_manual(values = dec_pal) +
  theme_classic() +
  labs(x="Year", y="Population %", title = "", color = "") +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    legend.position = "top",
    panel.grid.major.y = element_line(linetype = "dashed", size = .4),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

jpeg(filename = "../final_graphs/demographics/race_1790_2010.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)
#jpeg(filename = "../final_graphs/race_1790_2020.jpg", height = 20*72, width = 20*72, units = "px", res = 300)

p

dev.off()


## ...........................
## Change in Race Composition ----

race_trends_poc <-
  alb_dems %>%
  filter(category == "RACE" & final_level != "White")

race_trends_poc$final_level <- 
  factor(race_trends_poc$final_level, 
         levels = rev(c("Black or African American",
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

jpeg(filename = "../final_graphs/demographics/race_poc_2000_2019.jpg", height = 30*72, width = 30*72, units = 'px', res = 300)

race_trends2

dev.off()


## ...........................
## 2010 & 2019 Age ----

life_table_nums <-
  alb_dems %>%
  filter(category == "AGE") %>%
  mutate(
    final_level = factor(final_level, levels = c(
      "Under 5 years",
      "5 to 9 years",
      "10 to 14 years",
      "15 to 19 years",
      "20 to 24 years",
      "25 to 34 years",
      "35 to 44 years",
      "45 to 54 years",
      "55 to 59 years",
      "60 to 64 years",
      "65 to 74 years",
      "75 to 84 years",
      "85 years and over"
    )
    )
  ) %>%
  
  arrange(final_level) %>%
  gather(year, stat, - category, -final_level) %>%
  filter(year %in% c(2010, 2019)) %>%
  mutate(
    stat_display = case_when(
      year == 2010 ~ stat * -1,
      TRUE ~ stat
    ),
    
    label_pos = stat + 2,
    
    label_pos = case_when(
      year == 2010 ~ label_pos * -1,
      TRUE ~ label_pos
    ),
  )

life_table_nums

age_pal <- brewer.pal(3, "BuPu")[c(2, 3)]

year_age <-
  ggplot(life_table_nums, aes(y = final_level, x = stat_display, fill = year)) +
  geom_col(alpha = .9) +
  scale_fill_manual(values = age_pal)  +
  geom_text( aes(y = final_level, x = label_pos, label = paste0(stat, "%")), size = 3, inherit.aes = FALSE
  ) +
  scale_x_continuous(labels = function(x){paste0(abs(x), "%")}, limits = c(-20, 25), breaks = c(seq(-20, 20, 5), 0)) +
  
  annotate("text", y = 13.5, x = -15, label = "2010", fontface = "bold" ) +
  annotate("text", y = 13.5, x = 15, label = "2019", fontface = "bold"  ) +
  coord_cartesian(clip = 'off') +
  
  geom_vline(xintercept  = 0) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line()
  )


jpeg(filename = "../final_graphs/demographics/year_age.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

year_age

dev.off()


## ...........................
# Age by Sex ----

sex_age <- read_csv("sex_age.csv") %>%
  mutate(
    age_group = factor(age_group, levels = c(
      "Under 5 years",
      "5 to 9 years",
      "10 to 14 years",
      "15 to 19 years",
      "20 to 24 years",
      "25 to 34 years",
      "35 to 44 years",
      "45 to 54 years",
      "55 to 59 years",
      "60 to 64 years",
      "65 to 74 years",
      "75 to 84 years",
      "85 years and over"
    )
    )
  ) %>%
  group_by(sex) %>%
  mutate(denom = sum(estimate),
         pct = estimate/denom *100,
         stat_display =
           case_when(
             sex == "Male" ~ pct * -1,
             TRUE ~ pct
           ),
         label_pos = pct + 2,
         label_pos = case_when(
           sex == "Male" ~ label_pos * -1,
           TRUE ~ label_pos
         ),
  )

sex_age

sex_age_graph <-
  ggplot(sex_age, aes(y = age_group, x = stat_display, fill = sex)) +
  geom_col(alpha = .9) +
  scale_fill_manual(values = age_pal)  +
  geom_text( aes(y = age_group, x = label_pos, label = paste0(round(pct, 1), "%")), size = 3, inherit.aes = FALSE
  ) +
  scale_x_continuous(labels = function(x){paste0(abs(x), "%")}, limits = c(-20, 25), breaks = c(seq(-20, 20, 5), 0)) +
  
  annotate("text", y = 13.5, x = -15, label = "Male", fontface = "bold" ) +
  annotate("text", y = 13.5, x = 15, label = "Female", fontface = "bold" ) +
  coord_cartesian(clip = 'off') +
  
  geom_vline(xintercept  = 0) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line()
  )

jpeg(filename = "../final_graphs/demographics/sex_age.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

sex_age_graph

dev.off()


## ...........................
# Nativity ----

load("origins.Rda")

# Citizenship and place of birth
# Compute the cumulative percentages (top of each rectangle)
citizenship <- citizenship %>%
  mutate(fraction = pct/100,
         ymax = cumsum(fraction), # cum %/top of rect
         ymin = c(0,head(ymax, n=-1)), # bottom of rect
         status = fct_recode(Citizenship, # shorter labels
                             "US citizen, born in US" = "U.S. citizen, born in the United States",
                             "US citizen, born in\nunincorporated territories" = "U.S. citizen, born in Puerto Rico or U.S. Island Areas",
                             "US citizen, born abroad" = "U.S. citizen, born abroad of American parent(s)",
                             "US citizen, naturalized" = "U.S. citizen by naturalization",
                             "Not US citizen" = "Not a U.S. citizen"))

donut_pal <- sample(brewer.pal(7, "BuPu")[-1], 6, replace = FALSE)

# Plot
p <- ggplot(citizenship, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=status)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  scale_fill_manual(values = donut_pal) +
  geom_text(aes(label=paste0(round(pct,1),"%"),x=c(3.75, 3.5, 3.75, 3.75, 3.75),y=(ymin+ymax)/2), color = "white") +
  theme_void() +
  theme(legend.position=c(.525, .5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8))

jpeg(filename = "../final_graphs/demographics/citizen.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

p

dev.off()


## ...........................
## Origin of New Residents ----

origin <- origin %>%
  mutate(fraction = estimate/sum(estimate),
         pct = round(fraction*100,1),
         ymax = cumsum(fraction),
         ymin = c(0,head(ymax, n=-1)))


# Plot
p <- ggplot(origin, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Continent)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  scale_fill_manual(values = donut_pal) +
  geom_text(aes(label=paste0(round(pct,1),"%"),x=3.5,y=(ymin+ymax)/2), color = "white") +
  theme_void() +
  theme(legend.position=c(.5, .5)) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 8))

jpeg(filename = "../final_graphs/demographics/origin.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

p

dev.off()
