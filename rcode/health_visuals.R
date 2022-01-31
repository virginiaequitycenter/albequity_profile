## ...........................
## Script name: health_visuals.R
##
## Authors: Michele Claibourn, Sam Powers
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Analysis and visuals for health section 
##
## ...........................
## set working directory

# setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")
setwd("data")

## ...........................
## load packages ----

library(tidyverse)
library(readxl)
library(ggforce) # for 'geom_arc_bar'
library(RColorBrewer)
library(ggnewscale)
library(scales)
library(tigris)
library("ggspatial")
# devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
# install.packages("emojifont")
library(emojifont)
library(ggpubr)

options(scipen = 6, digits = 4) # to view outputs in non-scientific notation
select <- dplyr::select # avoid function name conflicts
summarize <- dplyr::summarize


## ...........................
## set palettes ----

## Pal 1
# hlth_colors <- c("#f0dbe2", "#b02c58")

## Pal 2
# hlth_colors <- c("#dfdcce", "#c23617")

## Pal 3
hlth_colors <- c("#f0dbe2", "#b02c58")

hlth_pal <- function(x) rgb(colorRamp(hlth_colors)(x), maxColorValue = 255) 


## ...........................
## Life Expectancy ----

tract_names <- read_csv("tract_names.csv") %>%
  select(-contains("X"))

tract_ahdi <- read_csv("tract_ahdi.csv")

life_exp <- tract_ahdi %>%
  rename_with(~ tolower(.x))  %>%
  left_join(tract_names) %>%
  select(geoid, keypoints, life_exp) %>%
  mutate(geo = "Census Tract") %>%
  bind_rows(
    read_csv("race_exp.csv") %>%
      select(
        geoid = fips,
        keypoints = demographic,
        life_exp = number
      ) %>%
      mutate(
        geo = case_when(keypoints == "total" ~ "Overall",
                        TRUE ~ "Race/Ethnicity"),
        keypoints = case_when(
          keypoints == "total" ~ "Albemarle County",
          TRUE ~ str_to_sentence(keypoints)
        )
      ) %>%
      filter(!is.na(life_exp))
  ) %>%
  mutate(geo = factor(geo,
                      levels = c("Overall", "Race/Ethnicity", "Census Tract"))
  ) %>%
  filter(!is.na(life_exp)) %>%
  mutate(plot_exp = life_exp  - 83,
         label_pos =
           case_when(
             plot_exp < 0 ~ plot_exp - 2,
             plot_exp > 0 ~ plot_exp + 2
           ),
         
         neg =      case_when(
           plot_exp < 0 ~ "neg",
           plot_exp > 0 ~ "pos"
         )
  )

life_exp

life_pal <- hlth_pal( seq(0, 1, length = 5))

life_exp_graph  <-
  ggplot(life_exp) +
  geom_segment(
    aes(xend = plot_exp, x = 0, y = reorder(keypoints, life_exp), yend = keypoints),
    size = 1
  ) +
  
  geom_point(
    aes(x = plot_exp,
        y = keypoints,
        color = neg
    ),
    size = 3
  )  +
  
  scale_color_manual(values = life_pal[c(2, 5)]) +
  
  geom_text(aes(x = label_pos , y = keypoints, label =  round(life_exp, 1) ), hjust = .5) +
  
  scale_x_continuous( limits = c(-20, 20), breaks = seq(-18, 17, 5), labels = function(x) x + 83 ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  
  geom_vline(xintercept = 0) +
  
  labs( x = "Average Life Expectancy", y = "", title = "Albemarle County Life Expectancy") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b =1, "cm"),
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(face = "bold"),
    strip.text.x = element_text(face = "bold")
    
  ) +
  
  facet_grid(geo ~ . ,  switch = "y", scales = "free",  space = "free_y")


jpeg(filename = "../final_graphs/health/life_exp.jpg", height = 45*72, width = 35*72, units = 'px', res = 300)

life_exp_graph

dev.off()


## ...........................
## Life Expectancy 2.0 ----
## Not used

life_expectancy_load <- read_excel("health_rankings.xlsx", sheet = 5, skip = 1) 

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
  
life_expectancies

life_pal <- hlth_pal( seq(0, 1, length = 5))

life_exp_graph  <-
  ggplot(life_expectancies) +
  geom_segment(
    aes(xend = low, x = high, y = reorder(demographic, mean), yend = demographic),
    size = 3,
    color = life_pal[5],
    alpha = .7
  ) +
  geom_segment(
    aes(xend = mean - .05,
        x = mean + .05,
        yend = demographic, 
        y = demographic,
    ),
    size = 4
  )  +
  geom_text(aes(x = mean , y = demographic, label =  round(mean, 1) ),
            hjust = .5, vjust = -1) +
  scale_x_continuous( limits = c(62, 103), breaks = seq(65, 100, 5) ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_vline(xintercept = 83) +
  labs( x = "Average Life Expectancy", y = "", title = "Albemarle County Life Expectancy") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b =1, "cm"),
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(face = "bold"),
    strip.text.x = element_text(face = "bold")
  ) +
  facet_grid(demo ~ . ,  switch = "y", scales = "free",  space = "free_y")

life_exp_graph


## ...........................
# Life Expectancy Map ----

alb_tract <- tracts(state = "VA", county = "003" )

ahdi_map <-
  alb_tract %>%
  left_join(tract_ahdi %>% mutate(GEOID = as.character(GEOID)))

life_exp_map_gg <-
  ggplot(ahdi_map) +
  geom_sf( aes(fill = life_exp), color = "black", alpha = .9) +
  scale_fill_steps(
    low = hlth_colors[1],
    high = hlth_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 8
    ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  
  labs(fill = "Life Expectancy") +
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
    #   legend.box="horizontal",
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

jpeg(filename = "../final_graphs/health/life_exp_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

life_exp_map_gg

dev.off()


## ...........................
# Food Insecurity ----
# Alternatively: https://github.com/hrbrmstr/waffle/

waffle_pal <-  hlth_pal( seq(0, 1, length = 5))[c(5,2)]

# Values transcribed from https://map.feedingamerica.org/county/2018/overall/virginia

food <- read_csv("food_insecurity.csv") 
foodsim_all <- data.frame(pop = rep("Overall", 100), 
                          food_insecure = rep(c("Food Insecure", "Not Food Insecure"), times = c(8, 92))) 

waffle_data_total <- waffle_iron(foodsim_all, aes_d(group = food_insecure),
                                 rows = 10, sample_size = 1) %>% 
  mutate(label = fontawesome('fa-male'))

ptotal <- ggplot(waffle_data_total, aes(x, y, fill = group)) + 
  geom_waffle() +
  coord_equal() + 
  scale_fill_manual(values = waffle_pal) + 
  theme_waffle() + 
  labs(x = "", y = "", fill = "", title = "Overall") +
  annotate(geom="text", x=5.5, y=5.5, label="8.4%",
           color=waffle_pal[1], size = 12, fontface = "bold")

ptotal2 <- ggplot(waffle_data_total, aes(x, y, color = group)) + 
  geom_text(aes(label=label), family='fontawesome-webfont', size=8) +
  coord_equal() + 
  scale_color_manual(values = waffle_pal) + 
  theme_waffle() + 
  labs(x = "", y = "", color = "", title = "Overall") +
  annotate(geom="text", x=5.5, y=5.5, label="8.4%",
           color=waffle_pal[1], size = 12, fontface = "bold")

foodsim_child <- data.frame(pop = rep("Child", 100), 
                            food_insecure = rep(c("Food Insecure", "Not Food Insecure"), times = c(10, 90))) 

waffle_data_child <- waffle_iron(foodsim_child, aes_d(group = food_insecure),
                                 rows = 10, sample_size = 1) %>% 
  mutate(label = fontawesome('fa-child'))

pchild <- ggplot(waffle_data_child, aes(x, y, fill = group)) + 
  geom_waffle() +
  coord_equal() + 
  scale_fill_manual(values = waffle_pal) + 
  theme_waffle() + 
  labs(x = "", y = "", fill = "", title = "Child")  +
  annotate(geom="text", x=5.5, y=5.5, label="9.6%",
           color=waffle_pal[1], size = 12, fontface = "bold")

pchild2 <- ggplot(waffle_data_child, aes(x, y, color = group)) + 
  geom_text(aes(label=label), family='fontawesome-webfont', size=8) +
  coord_equal() + 
  scale_color_manual(values = waffle_pal) + 
  theme_waffle() + 
  labs(x = "", y = "", color = "", title = "Child") +
  annotate(geom="text", x=5.5, y=5.5, label="9.6%",
           color=waffle_pal[1], size = 12, fontface = "bold")

psquare <- ggarrange(ptotal, pchild,  common.legend = TRUE, legend = "bottom")
picon <- ggarrange(ptotal2, pchild2, common.legend = TRUE, legend = "bottom")

jpeg(filename = "../final_graphs/health/food_insecure.jpg", height = 20*72, width = 40*72, units = 'px', res = 300)

psquare

dev.off()

jpeg(filename = "../final_graphs/health/food_insecure_icon.jpg", height = 20*72, width = 40*72, units = 'px', res = 300)

picon

dev.off()
