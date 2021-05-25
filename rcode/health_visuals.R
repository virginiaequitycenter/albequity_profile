## ---------------------------
## Script name: health_visuals.R
##
## Author:Sam Powers
## Date Created: 2021-02-02
##
## ---------------------------
## Purpose of script: Albemarle Equity Profile Visuals for ahdi
##   
##
## ---------------------------
## set working directory

setwd("/Volumes/GoogleDrive/My Drive/Equity Center/Github/albequity_profile/data")

## ---------------------------
## load up the packages we will need:

library(tidyverse)
library(ggforce) # for 'geom_arc_bar'
library(RColorBrewer)
library(ggnewscale)
library(scales)
library(tigris)
library("ggspatial")
#devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
#  install.packages("emojifont")
library(emojifont)
library(ggpubr)

select <- dplyr::select

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

# Health Color Theme ----------------------------------------------------------

# Pal 1
hlth_colors <- c("#f0dbe2", "#b02c58")

# Pal 2
hlth_colors <- c("#dfdcce", "#c23617")

# Pal 3
hlth_colors <- c("#f0dbe2", "#b02c58")

hlth_pal <- function(x) rgb(colorRamp(hlth_colors)(x), maxColorValue = 255) 


# Life Expectancy ---------------------------------------------------------

life_exp <- read_csv("tract_ahdi.csv") %>%
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










# Life Expectancy Map -----------------------------------------------------


alb_tract <- tracts(state = "VA", county = "003" )

ahdi_map <-
  alb_tract %>%
  left_join(tract_ahdi %>% mutate(GEOID = as.character(geoid)))


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



# Food Insecurity  --------------------------------------------------------------


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

