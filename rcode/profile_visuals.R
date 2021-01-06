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
library(viridis)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------
## read in data:
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


# Race over time ----------------------------------------------------------

race_trends <-
  alb_dems %>%
  filter(category == "RACE")
  
race_trends$final_level <- factor(race_trends$final_level, levels = rev(c("White", 
                                                                        "Black or African American", 
                                                                        "Asian", 
                                                                        "American Indian and Alaska Native",
                                                                        "Native Hawaiian and Other Pacific Islander",
                                                                        "Two or more races",
                                                                      "Some other race"))
  )

race_trends <- 
  race_trends %>%
  select(-category) %>%
  gather(year, pct, -final_level) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  group_by(year) %>%
  arrange(year, desc(final_level)) %>%
  mutate(height = cumsum(pct) - pct/2) %>%
  mutate(display = ifelse(pct > 5, paste0(round(pct),"%"), "")) 
  
midyear = ceiling((max(race_trends$year) - min(race_trends$year) + 1)/2)

alb_pal <- sample(alb_pal, length(alb_pal), replace = FALSE)

ggplot(race_trends, aes(x = year, y = pct, fill = final_level, group = final_level)) +
  geom_area(alpha=0.6 , size=.5, colour = "white") +
  
  # 2019 labale
  geom_text(data = race_trends %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by( final_level) %>%
              arrange(desc(year)) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1.2, hjust = 1, size = 2) +
  
  # 2011 Label
  geom_text(data = race_trends %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by( final_level) %>%
              arrange(year) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 2) +
  
  # middle label
  geom_text(data = race_trends %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by( final_level) %>%
              arrange(year) %>%
              slice(midyear),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 2) +
  
  
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(0, 100), breaks=seq(0,100, 25)) +

  scale_x_continuous( breaks=seq(2011,2019, 1), limits = c( 2011, 2019) )  +
  
  scale_fill_manual(values =c(rev(alb_pal))) +
  
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
  

# Try to do them all at once ----------------------------------------------
### errrr, maybe not - its pretty messy. 


unique(alb_dems$final_level)

alb_dems_use <- alb_dems

alb_dems_use$final_level <-
  factor(
    alb_dems$final_level,
    levels =
     c( 
    rev(
      c(
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
      ),
      
    rev(
      c("Not Hispanic or Latino",
        "Hispanic or Latino (of any race)"
        )
      ),
    
    rev(
      c(
        "White",
        "Black or African American",
        "Asian",
        "American Indian and Alaska Native",
        "Native Hawaiian and Other Pacific Islander",
        "Two or more races",
        "Some other race"
      )
    ),
    
    rev(
      c(
        "Male",
        "Female"
      )
    ),
    "Total population"
    
     )
  )
    
graph_dems <-
  alb_dems_use %>%
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
filter(!final_level == "Total population") %>%
  gather(year, pct, -final_level, -category) %>%
  mutate(year = as.numeric(year)) %>%
  ungroup() %>%
  group_by(year, category) %>%
  arrange(year, category, desc(final_level)) %>%
  mutate(height = cumsum(pct) - pct/2) %>%
  mutate(display = ifelse(pct > 5, paste0(round(pct),"%"), "")) 


ggplot(graph_dems, aes(x = year, y = pct, fill = final_level, group = final_level)) +
  geom_area(alpha=0.6 , size=.5, colour = "white") +
  
  # 2019 labale
  geom_text(data = graph_dems %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by(category, final_level) %>%
              arrange(desc(year)) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1.2, hjust = 1, size = 2) +
  
  # 2011 Label
  geom_text(data = graph_dems %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by(category, final_level) %>%
              arrange(year) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 2) +
  
  # middle label
  geom_text(data = graph_dems %>%
              # filter(AccesStatus == "Covered without PA") %>%
              group_by(category, final_level) %>%
              arrange(year) %>%
              slice(midyear),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 2) +
  
  
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(0, 100), breaks=seq(0,100, 25)) +
  
  scale_x_continuous( breaks=seq(2011,2019, 1), limits = c( 2011, 2019) )  +
  
  scale_fill_manual(values =c(rev(alb_pal), alb_pal[1:6], alb_pal[1:2], alb_pal, alb_pal[1:2]) )+
  
  coord_cartesian(clip = 'off') +
  
  labs(x="Year", y="Population %", fill = "Demographic")  +
  
  facet_grid(.~category) +
  
  theme_bw() +
  
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







# Just Age ----------------------------------------------------------------

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


ggplot(life_table_nums, aes(y = final_level, x = stat_display, fill = year)) +
  geom_col(alpha = .9) +
  scale_fill_manual(values = alb_pal)  +
  geom_text( aes(y = final_level, x = label_pos, label = paste0(stat, "%")), size = 3, inherit.aes = FALSE
            ) +
  scale_x_continuous(labels = function(x){paste0(abs(x), "%")}, limits = c(-20, 25), breaks = c(seq(-20, 20, 5), 0)) +
  
  annotate("text", y = 13.5, x = -15, label = "2010" ) +
  annotate("text", y = 13.5, x = 15, label = "2019" ) +
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


  
  
   
  









