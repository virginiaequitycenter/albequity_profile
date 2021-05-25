# life expectancy by tract experiment

library(tidyverse)
library(scales)
library(tidycensus)
library(ggpubr)

setwd("data")

# From health_visuals.R
hlth_colors <- c("#f0dbe2", "#b02c58")

hlth_pal <- function(x) rgb(colorRamp(hlth_colors)(x), maxColorValue = 255)

# Load data ----
tract_names <- read_csv("tract_names.csv") %>%
  select(-contains("X"))

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


# Playing around: tracts ----
life_exp_tract <- life_exp %>% 
  filter(geoid != "51003") %>% # remove non-tract rows
  mutate(y = 1)

ggplot(life_exp_tract, aes(x = life_exp, y = jitter(y, factor = 0.5))) +
  geom_point() + 
  scale_y_continuous(limits = c(0.5,1.5)) 

# add tract pop size
tract_pop <- get_acs(geography = "tract",
                     variable = "S0101_C01_001",
                     state = "VA",
                     county = "003",
                     survey = "acs5",
                     year = 2019,
                     cache_table = TRUE)

life_exp_tract <- life_exp_tract %>% 
  left_join(tract_pop %>% 
              select(GEOID, estimate) %>% 
              mutate(geoid = as.numeric(GEOID)))

# geom_bracket seems to break it...?
# so annotation for the moment
life_pal <- hlth_pal(seq(0, 1, length = 5))

ggplot(life_exp_tract, 
       aes(x = life_exp, y = y, size = estimate)) +
  geom_point(color = hlth_colors[2]) +
  geom_text(aes(x = life_exp, y = y, 
                label = keypoints), 
            alpha = 1, angle=45, size = 2,
            hjust = 0, vjust = 0, 
            nudge_y = 0.001, nudge_x = .1) +
  geom_bracket(xmin = 75.5, xmax = 87.1,
               y.position = 1.025,
               bracket.nudge.y = -.005,
               label = "11.6 year gap") +
  scale_x_continuous(limits = c(73,90)) + 
  scale_y_continuous(limits = c(1, 1.04)) +
  labs(x = "") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
  )

jpeg(filename = "../graphs/life_exp_tract_experiment.jpg", height = 15*72, width = 40*72, units = 'px', res = 300)

last_plot()

dev.off()
