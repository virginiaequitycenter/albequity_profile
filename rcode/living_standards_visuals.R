## ...........................
## Script name: living_standards_visuals.R
##
## Authors: Sam Powers, Michele Claibourn
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Analysis and visuals for living standards section 
##
## ...........................
## set working directory

# setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")
setwd("data")

## ...........................
## load packages ----

library(tidyverse)
library(ggforce) # for 'geom_arc_bar'
library(RColorBrewer)
library(ggnewscale)
library(scales)
library(tigris)
library("ggspatial")

options(scipen = 6, digits = 4) # to view outputs in non-scientific notation
select <- dplyr::select # avoid function name conflicts


## ...........................
## set palette ----

## Pal 1
# inc_colors <- c("#b1c5be", "#106449")

## Pal 2
# inc_colors <- c("#dfdcce", "#106348")

## Pal 3
inc_colors <- c("#b1c5be", "#106449")

inc_pal <- function(x) rgb(colorRamp(inc_colors)(x), maxColorValue = 255) 


## ...........................
## housing costs ----

tract_names <- read_csv("tract_names.csv") %>%
  select(-contains("X"))

house_cost <- read_csv("housing_costs.csv")

house_cost_burden <-
  house_cost %>%
  # select(-denom) %>%
  select(-total_renting, - total_households, - Renting) %>%
  gather(burden, pct,-geoid, -county_type, -denom) %>%
  mutate(burden = factor(burden, levels = c(
    "Not Burdened",
    "Burdened",
    "Severely Burdened"
  ))) %>%
  ungroup() %>%
  group_by(geoid) %>%
  arrange(geoid, burden) %>%
  mutate(
    end_pct = cumsum(pct),
    start_pct = cumsum(pct) - pct,
    burden_pct = case_when(
      burden == "Not Burdened" ~ end_pct,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    burden_pct = sum(burden_pct),
    start_line = start_pct - burden_pct,
    end_line = start_line + pct,
    height = (start_line + end_line) / 2,
    display =
      case_when(
        pct > .05 ~ paste0(round(pct*100), "%"),
        TRUE ~ ""
      )
  )  %>%
  left_join(tract_names %>%
              mutate(geoid = as.character(geoid)) %>%
              bind_rows(
                tibble(geoid = "total", keypoints = "Albemarle County")
              )
  ) %>%
  filter(!grepl("UVA", keypoints)) %>%
  mutate(county_type = factor(county_type, levels = c("County", "Census Tracts"))
  ) %>%
  group_by(geoid) %>%
  mutate(end_line_order = max(end_line)) %>%
  ungroup()

house_cost_burden

house_pal <- inc_pal( seq(0, 1, length = 3))

p <-
  ggplot(house_cost_burden, aes(y = reorder(keypoints, end_line_order )  ))  +
  geom_segment(aes(x = start_line, xend = end_line, color = burden, yend = keypoints ),
               size = 10, alpha= .8) +
  scale_color_manual(values = house_pal,
                     name = element_blank(),
                     guide = guide_legend(reverse = FALSE, nrow = 1)) +
  new_scale_color() +
  geom_text(
    aes(x = height, label = display, y = keypoints, color = burden),  alpha = 1, hjust =   .5, size = 2.75
  ) +
  scale_color_manual(values = c("Black",  "Black", "White"),
                     guide = "none") +
  geom_text(data = house_cost_burden %>%
              filter(end_pct == 1),
            aes(x = end_line + .02,
                #  x = -1.05,
                y = keypoints,
                label = paste0("Among ", denom,"\nRenting Households")),
            hjust = 0,
            size = 2.5,
            
            inherit.aes = FALSE
  ) +
  geom_segment(aes(x = start_line - .002, xend = start_line, yend = keypoints ), color = "white",
               size = 11, alpha= 1)  +
  geom_vline(xintercept = 0) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(
    labels = function(x)
      paste0(abs(round(x*100)), "%"),
    limits = c(-1, .75),
    breaks = seq(-1, .8, .25)
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)
  ) +
  facet_grid(county_type ~ . ,  switch = "y", scales = "free",  space = "free_y") +
  labs(x = "Percentage of Renting Households", y = "", title = "Rent Burdened Population by Census Tract") +
  # guides(color = guide_legend(label.position = "bottom")) +
  theme_classic() +
  theme(panel.spacing = unit(.5, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        #   axis.text.x = element_blank(),
        #  axis.ticks.x = element_blank(),
        #  axis.text.y=element_text(face= c("plain", "plain", "plain", "plain", "plain", "plain", "bold")),
        panel.grid.major.y = element_line(linetype = "dashed", size = .2),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5, face = "bold"),
        plot.margin = margin(l = .5, r = 1, t = 1, b =1, "cm")
  )

jpeg(filename = "../final_graphs/living_standards/housing_costs.jpg", height = 41*72, width = 46*72, units = 'px', res = 300)

p

dev.off()


## ...........................
## ALICE Metrics ----

alice_hhs <- read_csv("alice_alb_hhs.csv")

alice_hhs

alice_hhs_graph  <-
  alice_hhs %>%
  mutate(level = case_when(
    level == "poverty_household" ~ "Poverty",
    level == "alice_household" ~ "ALICE",
    level == "above_alice_household" ~ "Above ALICE"
  )
  ) %>%
  group_by(year) %>%
  arrange(year, desc(level)) %>%
  mutate(height = cumsum(pct) - pct/2) %>%
  mutate(display = ifelse(pct > 2, paste0(round(pct),"%" ), ""))

alice_pal <- inc_pal( seq(0, 1, length = 3))

alice_hhs_gg<-
  ggplot(alice_hhs_graph, aes(x = year, y = pct, fill = level, group = level)) +
  geom_area(alpha=0.6 , size=.5, colour = "white") +
  # end label
  geom_text(data = alice_hhs_graph %>%
              group_by( level) %>%
              arrange(desc(year)) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1.2, hjust = 1, size = 4) +
  # front Label
  geom_text(data = alice_hhs_graph %>%
              group_by( level) %>%
              arrange(year) %>%
              slice(1),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = -.2, size = 4) +
  # middle label
  geom_text(data = alice_hhs_graph %>%
              group_by( level) %>%
              arrange(year) %>%
              slice(3),
            aes(x = year, label = display, y = height ), color = "Black", alpha = 1, hjust = .5, size = 4) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(0, 100), breaks=seq(0,100, 25)) +
  scale_x_continuous( breaks=seq(2010,2018, 2), limits = c( 2010, 2018) )  +
  scale_fill_manual(values = alice_pal) +
  guides(fill = guide_legend(nrow = 1, label.position = "top", reverse = TRUE)) +
  coord_cartesian(clip = 'off') +
  labs(x="Year", y="Population %", fill = "Income Status", title = "Asset Limited, Income Constrained, Employed Population in Albemarle")  +
  theme_bw()+
  theme(
    plot.title = element_text( face="bold", hjust = .5, size = 12),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(face = "bold", vjust=-2.5, size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x=element_text(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line =  element_line(color  = "white"),
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face="bold", size = 10),
    panel.border = element_blank(),
    plot.margin=unit(c(t = .25, r = 1, b = 1.5, l = .1),"cm")
  )

jpeg(filename = "../final_graphs/living_standards/alice_hhs.jpg", height = 30*72, width = 30*72, units = 'px', res = 300)

alice_hhs_gg

dev.off()


## ...........................
## Alice Thres Race ----

alice_thresh <- read_csv("alice_thresh.csv")

alice_thresh_graph1 <-
  alice_thresh %>%
  filter(!race %in% c("White , Not Hispanic Or Latino")) %>%
  mutate(
    race = factor(race, levels = c(
      "Overall",
      "White",
      "Black Or African American",
      "Asian",
      "American Indian And Alaska Native",
      "Two Or More Races",
      "Some Other Race",
      "Hispanic Or Latino")
    )
  ) %>%
  filter(!is.na(race)) %>%
  filter(!race == "Overall")

alice_pal <- inc_pal( seq(0, 1, length = 3))

alice_thresh_race <-
  ggplot(alice_thresh_graph1, aes(x = year, y = `ALICE Threshold`)) +
  geom_area(data = alice_thresh_graph1,
            aes(x = year, y = `ALICE Threshold`),
            color = "black", size = 0, alpha = .1,
            fill = alice_pal[3] ) +
  geom_line(data = alice_thresh_graph1 %>%
              gather(type, value, -year, -race),
            aes(x = year, y = value, color = type),
            inherit.aes = FALSE, size = 1 ) +
  scale_color_manual(values = c("black", "blue")) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1)+
  labs(color = "", y = "", x = "", title = "Cost of Living Outpaces Median Income") +
  scale_y_continuous(limits = c(0, 100000), labels = function(x) paste0("$",formatC(x, format = "d", big.mark = ","))    )+
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    #  legend.position = c(.85, .15),
    legend.position = "top",
    axis.line.y = element_blank(),
    strip.background = element_rect(fill = "light grey"),
    panel.spacing = unit(1, "lines"),
    panel.grid.major.y = element_line(linetype = "dashed", size = .4),
    axis.line.x = element_blank(),
  ) +
  facet_wrap(~race, scales = "free_x")

jpeg(filename = "../final_graphs/living_standards/alice_thresh.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

alice_thresh_race

dev.off()


## ...........................
## ALICE Thresh Main ----

alice_thresh_graph2 <-
  alice_thresh %>%
  filter(!is.na(race)) %>%
  filter(race == "Overall")

alice_thresh_main <-
  ggplot(alice_thresh_graph2, aes(x = year, y = `ALICE Threshold`)) +
  geom_area(data = alice_thresh_graph2,
            aes(x = year, y = `ALICE Threshold`),
            color = "black", size = 0, alpha = .1,
            fill = alice_pal[3] ) +
  geom_line(data = alice_thresh_graph2 %>%
              gather(type, value, -year, -race),
            aes(x = year, y = value, color = type),
            inherit.aes = FALSE, size = 1 ) +
  scale_color_manual(values = c("black", "blue")) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 1)+
  labs(color = "", y = "", x = "", title = "Cost of Living Outpaces Median Income") +
  scale_y_continuous(limits = c(0, 100000), labels = function(x) paste0("$",formatC(x, format = "d", big.mark = ","))    )+
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    #  legend.position = c(.85, .15),
    legend.position = "top",
    axis.line.y = element_blank(),
    strip.background = element_rect(fill = "light grey"),
    panel.spacing = unit(1, "lines"),
    panel.grid.major.y = element_line(linetype = "dashed", size = .4),
    axis.line.x = element_blank(),
  ) 

jpeg(filename = "../final_graphs/living_standards/alice_thresh_overall.jpg", height = 30*72, width = 35*72, units = 'px', res = 300)

alice_thresh_main

dev.off()


## ...........................
## Income Map ----

med_hh_inc <- read_csv("med_inc_tract.csv") 

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

med_hhinc_tract_map <-
  alb_tract %>% left_join(med_hh_inc 
                          #%>% mutate(GEOID = as.character(GEOID)) 
                          %>% select(-NAME) )

med_hhinc_map  <-
  ggplot(med_hhinc_tract_map) +
  geom_sf( aes(fill = estimate), color = "black", alpha = .9) +
  scale_fill_steps(
    low = inc_colors[1],
    high = inc_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 8,
    labels = dollar_format(prefix = "$", suffix = "", 
                           big.mark = ",", 
    )
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Median Household Income") +
  annotation_scale(location = "br", width_hint = 0.25) +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         pad_x = unit(0.0, "in"),
                         pad_y = unit(0.5, "in"),
                         style = north_arrow_minimal(
                           line_width = 1,
                           line_col = "black",
                           fill = "black",
                           text_col = "black",
                           text_family = "",
                           text_face = NULL,
                           text_size = 0
                         )) +
  theme(
    legend.position = "top",
    legend.text = element_text(angle = -45, hjust = 0),
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

jpeg(filename = "../final_graphs/living_standards/hhinc_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

med_hhinc_map

dev.off()


## ...........................
## Gini Index ----

gini_index <- read_csv("gini_index.csv")

gini_plot <-
  ggplot(gini_index, aes(x = year, y = estimate)) +
  geom_line(size = 1, color = inc_colors[2]) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  labs(y = "", x = "", title = "Gini Index of Income Inequality") +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    panel.grid.major.y = element_line(linetype = "dashed", size = .4)
  )

jpeg(filename = "../final_graphs/living_standards/gini_index.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

gini_plot

dev.off()


gini_index_all <- read_csv("gini_index_all.csv")

gini_plot_all <- 
  filter(gini_index_all, GEOID == 51003) %>% 
  ggplot(aes(x = year, y = estimate, color = NAME)) +
  geom_line(size = 1, color = inc_colors[2]) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = 2010:2019) +
  theme_classic() +
  labs(y = "", x = "", title = "Gini Index of Income Inequality") +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    panel.grid.major.y = element_line(linetype = "dashed", size = .4)
  )

jpeg(filename = "../final_graphs/living_standards/gini_index_2019.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

gini_plot_all

dev.off()

