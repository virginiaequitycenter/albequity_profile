## ...........................
## Script name: education_visuals.R
##
## Authors: Sam Powers, Michele Claibourn
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Analysis and visuals for education section 
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
# educ_colors <- c("#e7dbbc", "#ffc000")

## Pal 2
# educ_colors <- c("#dfdcce", "#624906")

## Pal 3
educ_colors <- c("#e7dbbc", "#e68026")

ed_ramp <- colour_ramp(educ_colors)


## ...........................
## read ed data ----

tract_names <- read_csv("tract_names.csv") %>%
  select(-contains("X"))

ed_dist <- read_csv("education_distrbution.csv")

proper=function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)

unique(ed_dist$degree)
unique(ed_dist$Race)

ed_dist <- read_csv("education_distrbution.csv") %>%
  mutate(Race = proper(Race)) %>%
  mutate(
    degree = factor(degree,
                    levels = rev(c(
                      "Less than high school diploma",
                      "High school graduate",
                      "Some college or associate's degree",
                      "Bachelor's degree or higher"
                    ))
    ),
    Race = factor(
      Race,
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


## ...........................
## Education by Race ----

ed_graph <-
  ed_dist %>%
  ungroup() %>%
  group_by(Sex, Race) %>%
  arrange(Sex, Race, desc(degree)) %>%
  mutate(
    end_pct = cumsum(percent),
    start_pct = cumsum(percent) - percent,
    bac_pct = case_when(
      degree == "Some college or associate's degree" ~ end_pct,
      TRUE ~ 0
    ),
    bac_pct = sum(bac_pct),
    start_line = start_pct - bac_pct,
    end_line = start_line + percent,
    height = (start_line + end_line) / 2,
    display =
      case_when(
        percent > 10 ~ paste0(round(percent), "%"),
        TRUE ~ ""
      )
  ) #%>%
# filter(Sex == "All")

ed_race_pal <- rev(ed_ramp(seq(0, 1, length = 4)))

ed_race <-
  ggplot(ed_graph, aes(y = Race))  +
  geom_segment(aes(x = start_line, xend = end_line, color = degree, yend = Race ),
               size = 14, alpha= .7) +
  scale_color_manual(values = ed_race_pal,
                     name = element_blank(),
                     guide = guide_legend(reverse = TRUE, nrow = 1)) +
  new_scale_color() +
  geom_text(
    aes(x = height, label = display, y = Race, color = degree),  alpha = 1, hjust =   .5, size = 2.75
  ) +
  scale_color_manual(values = c("Black",  "Black", "Black", "Black"),
                     guide = "none") +
  
  geom_segment(aes(x = start_line - .3, xend = start_line, yend = Race ), color = "white",
               size = 14, alpha= 1)  +
  
  geom_vline(xintercept = 0) +
  
  coord_cartesian(clip = 'off') +
  
  scale_x_continuous(
    labels = function(x)
      paste0(abs(round(x)), "%")
  ) +
  
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)
  ) +
  
  facet_grid(~Sex, scales = "free") +
  
  guides(
    #  color = guide_legend(label.position  = "top")
  ) +
  
  labs(x = "Percentage", y = "", title = "Educational Distributions by Race and Ethnicity") +
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
        legend.position = "bottom",
        plot.title = element_text(hjust = .5, face = "bold")
  )

jpeg(filename = "../final_graphs/education/ed_race.jpg", height = 30*72, width = 55*72, units = 'px', res = 300)

ed_race

dev.off()


## ...........................
## Bachelor's Tract Map ----

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

ed_tract <- read_csv("geographic_education.csv")

ed_geo_tract <- alb_tract %>%
  left_join(ed_tract) %>%
  mutate(perc_bac = perc_bac/100)

ed_map <-
  ggplot(ed_geo_tract) +
  geom_sf( aes(fill = perc_bac), color = "black") +
  scale_fill_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10,
    labels = percent

  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Percent With Bachelor's Degree or Higher") +
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
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
    
  )

jpeg(filename = "../final_graphs/education/ed_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

ed_map

dev.off()


## ...........................
## Attainment Lollipop Chart ----

ed_tract_graph  <-
  ed_tract %>%
  rename_with( ~ tolower(.x))  %>%
  left_join(tract_names) %>%
  filter(!grepl("UVA", keypoints)) %>%
  mutate(label = paste0(round(perc_bac, 1), "%"))

bac_deg_graph  <-
  ggplot(ed_tract_graph) +
  geom_segment(
    aes(xend = perc_bac, x = 0, y = reorder(keypoints ,perc_bac), yend = keypoints), size = 1
  ) +
  geom_point(
    aes(x = perc_bac,
        y = reorder(keypoints, perc_bac),     
        color = perc_bac 
    ),
    size = 3
  )  +
  scale_color_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "color",
  ) +
  geom_text(aes(x = perc_bac + 2.5, y = keypoints, label = label ), hjust = 0) +
  scale_x_continuous( limits = c(0, 90), breaks = c(seq(0, 100, 10), 0), labels = function(x) paste0(x,"%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_vline(xintercept = 60.57,
             # linetype = "dashed",
             color = "black",
             size = .2) +
  annotate("text", x = 60.57, y = 19.5,
           label = "Albemarle County %",
           vjust = -.7,
           hjust = 0.5,
           color = "black",
           size = 3.5 ) +
  labs( x = "Percent With Bachelor's degree", y = "", title = "Percent with Bachelor's Degrees by Tract") +
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
    plot.margin = margin(l = .5, r = 1, t = 2, b =1, "cm")
  )


jpeg(filename = "../final_graphs/education/bach_deg_graph.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

bac_deg_graph

dev.off()


## ...........................
## School Enrollment Map ----

tract_enroll <- read_csv("tract_enroll.csv")

enroll_geo_tract <- alb_tract %>%
  left_join(tract_enroll %>% rename(GEOID = fips)) %>%
  mutate(school_enroll = school_enroll/100)

enroll_map <-
  ggplot(enroll_geo_tract) +
  geom_sf( aes(fill = school_enroll), color = "black") +
  scale_fill_steps(
    low = educ_colors[1],
    high = educ_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10,
    labels = percent
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Percent Enrolled in School [Ages 3 - 24]") +
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
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
    
  )

jpeg(filename = "../final_graphs/education/enroll_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

enroll_map

dev.off()


## ...........................
## Within Schools Data ----

### Student Discipline ----

student_data <- read_csv("student_data.csv") %>%
  mutate(pop  = factor(pop, levels = rev(c(
    "All",
    "White",
    "Black",
    "Asian",
    "Two or more races",
    "Hispanic",
    "English language learner",
    "Students with disabilities",
    "Economically disadvantaged"
  ))
  )
  )

suspended  <-
  ggplot(student_data[!student_data$pop == "All" ,]) +
  geom_segment(
    aes(xend = Suspended, x = 0, y = pop, yend = pop),
    size = 1
  ) +
  geom_point(
    aes(x = Suspended,
        y = pop
    ),
    color = educ_colors[2],
    size = 3
  )  +
  geom_vline(xintercept = student_data[student_data$pop == "All" , "Suspended"][[1]],
             linetype = "dashed",
             color = "black",
             size = .2) +
  annotate("text",
           x = student_data[student_data$pop == "All" , "Suspended"][[1]],
           y = 9,
           label = "All Students",
           vjust = -.7,
           hjust = 0.5,
           color = "black",
           size = 3.5 ) +
  geom_text(aes(x = Suspended + .2, y = pop, label = paste0(round(Suspended), "%") ), hjust = 0) +
  scale_x_continuous( limits = c(0, 10), breaks = c(seq(0, 10, 1), 0), labels = function(x) paste0(x,"%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = guide_legend(label.position  = "top")
  ) +
  labs( x = "Percent Suspended from School", y = "", title = "Percent Students Suspended from School") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b =1, "cm")
  )


jpeg(filename = "../final_graphs/education/suspended.jpg", height = 25*72, width = 25*72, units = 'px', res = 300)

suspended

dev.off()


### Absences ----

absent  <-
  ggplot(student_data[!student_data$pop == "All" ,]) +
  geom_segment(
    aes(xend = `Chronically Absent`, x = 0, y = pop, yend = pop),
    size = 1
  ) +
  geom_point(
    aes(x = `Chronically Absent`,
        y = pop
    ),
    color = educ_colors[2],
    size = 3
  )  +
  geom_vline(xintercept = student_data[student_data$pop == "All" , "Chronically Absent"][[1]],
             linetype = "dashed",
             color = "black",
             size = .2) +
  annotate("text",
           x = student_data[student_data$pop == "All" , "Chronically Absent"][[1]],
           y = 9,
           label = "All Students",
           vjust = -.7,
           hjust = 0.5,
           color = "black",
           size = 3.5 ) +
  geom_text(aes(x = `Chronically Absent` + .4, y = pop, label = paste0(round(`Chronically Absent`), "%") ), hjust = 0) +
  scale_x_continuous( limits = c(0, 15), breaks = c(seq(0, 15, 2.5), 0), labels = function(x) paste0(x,"%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = guide_legend(label.position  = "top")
  ) +
  labs( x = "Percent Chronically Absent from School", y = "", title = "Percent Students Chronicaly Absent from School") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b =1, "cm")
  )

jpeg(filename = "../final_graphs/education/absent.jpg", height = 25*72, width = 25*72, units = 'px', res = 300)

absent

dev.off()


### AP Testing  ----

ap  <-
  ggplot(student_data[!student_data$pop == "All" ,]) +
  geom_segment(
    aes(xend = `Enrolled in AP Courses`, x = 0, y = pop, yend = pop),
    size = 1
  ) +
  geom_point(
    aes(x = `Enrolled in AP Courses`,
        y = pop
    ),
    color = educ_colors[2],
    size = 3  )  +
  geom_vline(xintercept = student_data[student_data$pop == "All" , "Enrolled in AP Courses"][[1]],
             linetype = "dashed",
             color = "black",
             size = .2) +
  annotate("text",
           x = student_data[student_data$pop == "All" , "Enrolled in AP Courses"][[1]],
           y = 9,
           label = "All Students",
           vjust = -.7,
           hjust = 0.5,
           color = "black",
           size = 3.5 ) +
  geom_text(aes(x = `Enrolled in AP Courses` + 2, y = pop, label = paste0(round(`Enrolled in AP Courses`), "%") ), hjust = 0) +
  scale_x_continuous( limits = c(0, 60), breaks = c(seq(0, 60, 10), 0), labels = function(x) paste0(x,"%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = guide_legend(label.position  = "top")
  ) +
  labs( x = "Percent HS Students Enrolled in AP Classes", y = "", title = "Percent Students Enrolled in AP Classes") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5, vjust = 6, face = "bold"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l = .5, r = 1, t = 2, b =1, "cm")
  )

jpeg(filename = "../final_graphs/education/ap.jpg", height = 25*72, width = 25*72, units = 'px', res = 300)

ap

dev.off()


