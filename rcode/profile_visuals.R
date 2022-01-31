## ...........................
## Script name: profile_visuals.R
##
## Authors: Michele Claibourn, Sam Powers
## Date Created: 2021-01-04
## Updated: 2022-01-28 mpc
## Purpose: Combine analysis and visuals code for the albemarle equity profile
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
library(tigris)
library(scales)
library(ggforce)
library("ggspatial")

options(scipen = 6, digits = 4) # to view outputs in non-scientific notation
select <- dplyr::select # avoid function name conflicts


## ...........................
## Racial composition ----

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


## Race over time ----

race_trends <-
  alb_dems %>%
  filter(category == "RACE")

race_trends$final_level <- 
  factor(race_trends$final_level, 
         levels = rev(c("White", 
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
  mutate(display = ifelse(pct > 2, paste0(round(pct),"%"), ""))

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
  scale_x_continuous( breaks=seq(2010,2019, 1), limits = c( 2010, 2019) )  +
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

## It has been so stable over the past 10 years.
## Maybe we just pie chart it or something.

race_trends_pie <-
  race_trends %>%
  filter(year == 2019) %>%
  mutate(end = 2 * pi * cumsum(pct)/sum(pct),
         start = lag(end, default = 0),
         middle = 0.5 * (start + end),
         hjust = ifelse(middle > pi, 1, 0),
         vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

pie_pal <- sample(brewer.pal(8, "BuPu")[-1], 7, replace = FALSE)

race_pie <-
ggplot(race_trends_pie) +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                   start = start, end = end, fill = final_level)) +
  geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = display,
                hjust = hjust, vjust = vjust)) +
  coord_fixed(clip = "off") +
  scale_x_continuous(limits = c(-1.5, 1.4),  # Adjust so labels are not cut off
                     name = "", breaks = NULL, labels = NULL) +
  # scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
  #                    name = "", breaks = NULL, labels = NULL) +
  scale_fill_manual(values =c(pie_pal)) +
   guides(fill = guide_legend(ncol = 1, label.position = "right", reverse = TRUE)) +
  theme_void() +
  labs(fill = "") +
  theme(
    legend.position = "right"
  )

race_pie

## Or look at the over time trend excluding White?
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

jpeg(filename = "../final_graphs/complete/race_poc_2000_2019.jpg", height = 30*72, width = 30*72, units = 'px', res = 300)

race_trends2

dev.off()


## ...........................
## Historical race over time ----

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

jpeg(filename = "../final_graphs/complete/race_1790_2010.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

p

dev.off()


## ...........................
## Try to do them all at once ----
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


## ...........................
## Just Age ----

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

jpeg(filename = "../final_graphs/complete/year_age.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

year_age

dev.off()


## ...........................
## Age by Sex ----

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

jpeg(filename = "../final_graphs/complete/sex_age.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

sex_age_graph

dev.off()


## ...........................
# AHDI  ----

## Do we want to graph this?
## Still need AHDI of the USA
ahdi_table <- read_csv("ahdi_table.csv") %>%
  mutate(
  county = case_when(county == "total" ~ "Virginia",
            TRUE ~ county
            )
  )

ahdi_table
View(ahdi_table)

tract_names <- read_csv("tract_names.csv") %>%
  select(-contains("X"))

## Tract level ahdi
tract_ahdi <- read_csv("tract_ahdi.csv") %>%
  rename_with(~tolower(.x))  %>%
  left_join(tract_names)

tract_ahdi_graph_prep <-
  tract_ahdi %>%
  select(geoid, ahdi_health, ahdi_ed, ahdi_income, ahdi, keypoints) %>%
  gather(component, metric, -c(geoid, keypoints)) %>%
  separate(component, c(NA, "category")) %>%
  mutate(
    category =
      case_when(
        is.na(category) ~ "Composite",
        category == "ed" ~ "Education",
        TRUE ~ str_to_sentence(category)
      ),
    category =
      factor(category,
             levels = c(
               "Health",
               "Education",
               "Income",
               "Composite"
             )
      )
  ) %>%
    filter(!is.na(metric)) %>%
  filter(!grepl("UVA", keypoints))

tract_ahdi_graph <-
  tract_ahdi_graph_prep %>%
  mutate(alpha_indicator =
         case_when(
           category == "Composite" ~ 1,
           TRUE ~ 0
         )
)

ahdi_pal <- brewer.pal(9, "BuPu")[c(4,6,8,9)]
adhi_alb <- ahdi_table[ahdi_table$county == "Albemarle", c("ahdi_ed", "ahdi_income", "ahdi_health", "ahdi")]

names(adhi_alb) <- c("Education", "Income", "Health", "Composite")

adhi_alb[1,"Education"]

## Dot plot option
ahdi_dots_function <- function(input){
  plot_out <-
    tract_ahdi_graph_prep %>%
    mutate(alpha_indicator =
             case_when(
               category == input ~ 1,
               TRUE ~ 0
             )
    ) %>%
ggplot() +
  geom_point(
    aes(x = metric,
        y = reorder(keypoints, metric),
        color = category,
        alpha = alpha_indicator,
        size = alpha_indicator
    ),
                )  +
  scale_color_manual(values = ahdi_pal) +
  scale_alpha_continuous(range = c(.8, .7)) +
 scale_size_continuous(range = c(2,4)) +
  scale_x_continuous( limits = c(3, 10.5), breaks = c(seq(3, 10, 1), 0)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  #  scale_color_manual(values = alb_pal) +
  geom_vline(xintercept = adhi_alb[1,input][[1]],
            # linetype = "dashed",
             color = "black",
             size = .2) +
  annotate("text", x = adhi_alb[1,input][[1]], y = 19.5,
           label = paste0( "Albemarle County ", input, " AHDI" ),
           vjust = -.7,
           hjust = 0.5,
           color = "black",
           size = 3.5 ) +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = guide_legend(label.position  = "top")
  ) +
  labs(color = "", x = "AHDI", y = "", title = "Tract-Level American Human Development Index") +
    coord_cartesian(clip = "off") +
theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
   # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
   plot.margin = margin(l =  .1, r = 1, t = 1, b =1, "cm")
  )
print(plot_out)
}

component_vec <- c("Health", "Income", "Education", "Composite")

for (component in component_vec) {

jpeg(filename = paste0("../final_graphs/complete/ahdi_dots_", component,".jpg"),
     height = 30*72, width = 30*72,
     units = 'px', res = 300)

ahdi_dots_function(component)

dev.off()

}


## ...........................
## Overall AHDI Metric alone ----

plot_out <-
  tract_ahdi_graph_prep %>%
  mutate(alpha_indicator =
           case_when(
             category == "Composite" ~ 1,
             TRUE ~ 0
           )
  ) %>%
 # filter(category == "Composite") %>%
  ggplot() +
  geom_point(
    aes(x = metric,
        y = reorder(keypoints, metric),
        color = category,
        alpha = alpha_indicator,
        size = alpha_indicator
    ),
  )  +
  scale_color_manual(values = ahdi_pal) +
  scale_alpha_continuous(range = c(0, .7)) +
  scale_size_continuous(range = c(2,4)) +
  scale_x_continuous( limits = c(3, 10.5), breaks = c(seq(3, 10, 1), 0)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  #  scale_color_manual(values = alb_pal) +
  geom_vline(xintercept = adhi_alb[1, "Composite"][[1]],
             # linetype = "dashed",
             color = "black",
             size = .2) +
  annotate("text", x = adhi_alb[1, "Composite"][[1]], y = 19.5,
           label = paste0( "Albemarle County Composite AHDI" ),
           vjust = -.7,
           hjust = 0.5,
           color = "black",
           size = 3.5 ) +
  guides(
    alpha = FALSE,
    size = FALSE,
    color = guide_legend(label.position  = "top")
  ) +
  labs(color = "", x = "AHDI", y = "", title = "Tract-Level American Human Development Index") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    #   axis.text.x = element_blank(),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(),
    plot.margin = margin(l =  .1, r = 1, t = 1, b =1, "cm")
  )

jpeg(filename = paste0("../final_graphs/complete/ahdi_dots_just_composite.jpg"),
     height = 30*72, width = 30*72,
     units = 'px', res = 300)

plot_out

dev.off()


## ...........................
## Parallel Coordinates ----

tract_ahdi_graph %>%
  mutate(category =
           factor(category,
               levels = c(
                          "Health",
                          "Education",
                          "Income",
                          "Composite"
              )
              )
           ) %>%
ggplot() +
  geom_point( aes(x = category, y = metric, color = category) ) +
  geom_line(aes(group = keypoints, x = category, y = metric), size = .1) +
  geom_text(data = tract_ahdi_graph %>% filter(category == "composite"),
            aes(label = keypoints, x = "composite" , y = metric,
                hjust = -.1))


## ...........................
## AHDI Tract Map ----

alb_tract <- tracts(state = "VA", county = "003")
ahdi_map <-
  alb_tract %>%
  left_join(tract_ahdi %>% mutate(GEOID = as.character(geoid)))

ahdi_map_gg <-
  ggplot(ahdi_map) +
  geom_sf( aes(fill = ahdi), color = "black", alpha = .9) +
  scale_fill_fermenter(limits = c(3,10), palette = "BuPu", direction = 1,   type = "seq", n.breaks = 8) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "American Human Development Index") +
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

jpeg(filename = "../final_graphs/complete/ahdi_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

ahdi_map_gg

dev.off()


## ...........................
## Education ----

ed_dist <- read_csv("education_distrbution.csv")

proper=function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)

unique(ed_dist$degree)
unique(ed_dist$Race)

ed_dist <-read_csv("education_distrbution.csv") %>%
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

ed_cols <- c("#e7dbbc", "#ffc000")
ramp <- colour_ramp(c("#e7dbbc", "#ffc000"))
ed_race_pal <- rev(ramp(seq(0, 1, length = 4)))

#ed_race_pal <- rev(brewer.pal(5, "BuPu"))[-5]

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

jpeg(filename = "../final_graphs/complete/ed_race.jpg", height = 30*72, width = 55*72, units = 'px', res = 300)

ed_race

dev.off()


## ...........................
## Education by tract ----

alb_tract <- tracts(state = "VA", county = "003") %>%
  mutate(GEOID = as.numeric((GEOID)))

ed_tract <- read_csv("geographic_education.csv")

ed_geo_tract <- alb_tract %>%
  left_join(ed_tract) %>%
  mutate(perc_bac = perc_bac/100)

#map_pal <- brewer.pal(9, "BuPu")
map_pal <- ramp(seq(0, 1, length = 9 ))

ed_map <-
  ggplot(ed_geo_tract) +
    geom_sf( aes(fill = perc_bac), color = "black") +
    # scale_fill_steps(
    #   low = map_pal[1],
    #   high = map_pal[9],
    #   space = "Lab",
    #   na.value = "grey50",
    #   guide = "coloursteps",
    #   aesthetics = "fill",
    #   n.breaks = 10,
    #   labels = percent
    #
    # ) +
#?scale_fill_fermenter()
 # scale_fill_fermenter(palette = "BuPu", direction = 1,   type = "seq", labels = percent) +
  #scale_fill_binned(map_pal )
#scale_fill_continuous(values = ed_cols)+

scale_fill_gradientn(colors = ed_cols, 
                     labels = scales::percent,
                   #  limits = c(, 1),
                     guide = guide_colorbar(labesl = scales::percent,
                                            direction = "horizontal",
                                            title.position = "top", nbin = 10, 
                                            barwidth = 20)) +
    theme_void() +
    # guides(fill =
    #          guide_colourbar(title.position="top", title.hjust = 0.5,
    #                          barwidth = 20)
    # ) +
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
      #   legend.box="horizontal",
      legend.title = element_text(),
      panel.border = element_rect(color  = "black",
                                  fill = NA,
                                  size = 1),
      plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
    )

jpeg(filename = "../final_graphs/complete/ed_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

ed_map

dev.off()


## ...........................
## Maybe a different chart ----

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
        y = reorder(keypoints, perc_bac)
    ),
    color = map_pal[8],size = 3
  )  +
  geom_text(aes(x = perc_bac + 2.5, y = keypoints, label = label ), hjust = 0) +
  scale_x_continuous( limits = c(0, 90), breaks = c(seq(0, 100, 10), 0), labels = function(x) paste0(x,"%")) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  #  scale_color_manual(values = alb_pal) +
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
  guides(
    alpha = FALSE,
    size = FALSE,
    color = guide_legend(label.position  = "top")
  ) +
  labs( x = "Percent With Bachelor's degree", y = "", title = "Percent with Bachelor's Degrees by Tract") +
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

jpeg(filename = "../final_graphs/complete/bach_deg_graph.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

bac_deg_graph

dev.off()


## ...........................
## Within Schools ----

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

## Put the "All" bar going down.
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
    color = map_pal[8],
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

jpeg(filename = "../final_graphs/completed/suspended.jpg", height = 25*72, width = 25*72, units = 'px', res = 300)

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
    color = map_pal[8],
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

jpeg(filename = "../final_graphs/complete/absent.jpg", height = 25*72, width = 25*72, units = 'px', res = 300)

absent

dev.off()


### ap class ----

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
    color = map_pal[8],
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

jpeg(filename = "../final_graphs/complete/ap.jpg", height = 25*72, width = 25*72, units = 'px', res = 300)

ap

dev.off()


## ...........................
## Nativity ----

load("origins.Rda")

## Citizenship and place of birth
## Compute the cumulative percentages (top of each rectangle)
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

## Plot
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

jpeg(filename = "../final_graphs/complete/citizen.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

p

dev.off()

## wanted to add a marker to identify foreign-born; this didn't work
  # geom_bracket(
  #   xmin = 0.5, xmax = 1, y.position = 1,
  #   label = "Foreign-Born"
  # )

## Huh, ggpubr now has a donut function....
# ggdonutchart(
#   citizenship,
#   x = "fraction",
#   label = "status",
#   lab.pos = "out",
#   fill = "status",
#   ggtheme = theme_pubr())


## Origin of new residents
origin <- origin %>%
  mutate(fraction = estimate/sum(estimate),
         pct = round(fraction*100,1),
         ymax = cumsum(fraction),
         ymin = c(0,head(ymax, n=-1)))

## Plot
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

jpeg(filename = "../final_graphs/complete/origin.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

p

dev.off()


## ...........................
## housing costs ----

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

house_pal <- brewer.pal(4, "BuPu")[-1]

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

jpeg(filename = "../final_graphs/complete/housing_costs.jpg", height = 41*72, width = 46*72, units = 'px', res = 300)

p

dev.off()

## ...........................
## Side bar pie charts

house_cost_totals <-
  house_cost %>%
  select(total_renting,  total_households, Renting, geoid, county_type) %>%
  left_join(tract_names %>%
              mutate(geoid = as.character(geoid)) %>%
              bind_rows(
                tibble(geoid = "total", keypoints = "Albemarle County")
              )
  ) %>%
  mutate( `Not Renting` = 1 - Renting)  %>%
  filter(!grepl("UVA", keypoints)) %>%
  mutate(county_type = factor(county_type, levels = c("County", "Census Tracts"))
  ) 

house_bars <-
  house_cost_totals %>%
  select(Renting, `Not Renting`, geoid, keypoints) %>%
  gather(rent_status, pct, -geoid, -keypoints) %>%
  group_by(
    geoid
  ) %>%
  arrange(geoid, rent_status) %>%
  mutate(start = lag(pct),
         start = case_when(
           is.na(start) ~ 0,
           TRUE ~ start
         ),
         end = pct + start,
         start = start * 2*pi,
         end = end*2*pi
  ) #%>%

house_bars

ggplot(house_bars, aes(
    y = as.numeric(as.factor(keypoints))
  )) +
  geom_arc_bar( aes(x0 = 1, 
                    y0 = as.numeric(as.factor(keypoints)), 
                    start = start, 
                    end = end, 
                    r0 = 0, 
                    r= .25, 
                    fill = rent_status),
                inherit.aes = FALSE
                ) +
    coord_equal()
  

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

alice_pal <- brewer.pal(4, "BuPu")[-1]

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

jpeg(filename = "../final_graphs/complete/alice_hhs.jpg", height = 30*72, width = 30*72, units = 'px', res = 300)

alice_hhs_gg

dev.off()


## ...........................
## ALICE Threshold ----

alice_thresh <- read_csv("alice_thresh.csv")

alice_thresh_graph <-
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
      "Hispanic Or Latino"
    )
 )
) %>%
  filter(!is.na(race)) %>%
  filter(!race == "Overall")

alice_pal <- brewer.pal(4, "BuPu")[-1]

alice_thresh_race <-
ggplot(alice_thresh_graph, aes(x = year, y = `ALICE Threshold`)) +
  geom_area(data = alice_thresh_graph,
            aes(x = year, y = `ALICE Threshold`),
            color = "black", size = 0, alpha = .1,
            fill = alice_pal[3] ) +
  geom_line(data = alice_thresh_graph %>%
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

jpeg(filename = "../graphs/alice_thresh.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

alice_thresh_race

dev.off()


## ...........................
## ALICE Thresh Main ----

alice_thresh <- read_csv("alice_thresh.csv")

alice_thresh_graph <-
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
      "Hispanic Or Latino"
    )
    )
  ) %>%
  filter(!is.na(race)) %>%
  filter(race == "Overall")

alice_pal <- brewer.pal(4, "BuPu")[-1]

alice_thresh_main <-
  ggplot(alice_thresh_graph, aes(x = year, y = `ALICE Threshold`)) +
  geom_area(data = alice_thresh_graph,
            aes(x = year, y = `ALICE Threshold`),
            color = "black", size = 0, alpha = .1,
            fill = alice_pal[3] ) +
  geom_line(data = alice_thresh_graph %>%
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

jpeg(filename = "../graphs/alice_thresh_overall.jpg", height = 30*72, width = 35*72, units = 'px', res = 300)

alice_thresh_main

dev.off()


## ...........................
# Income Map ----

med_hh_inc <- read_csv("med_inc_tract.csv") 

med_hhinc_tract_map <-
  alb_tract %>% 
  left_join(med_hh_inc %>% mutate(GEOID = as.character(GEOID)) %>% select(-NAME) )

med_hhinc_map  <-
  ggplot(med_hhinc_tract_map) +
  geom_sf( aes(fill = estimate), color = "black", alpha = .9) +
  scale_fill_fermenter(limits = c(50000,140000), palette = "BuPu", direction = 1,   type = "seq", n.breaks = 8, 
                       label = dollar_format(prefix = "$", suffix = "", 
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
    #   legend.box="horizontal",
    legend.text = element_text(angle = -45, hjust = 0),
    legend.title = element_text(),
    panel.border = element_rect(color  = "black",
                                fill = NA,
                                size = 1),
    plot.margin = margin(l =  .1, r = .1, t = 1, b =1, "cm")
  )

jpeg(filename = "../final_graphs/complete/hhinc_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

med_hhinc_map

dev.off()


## ...........................
## Gini Index ----

gini_index <- read_csv("gini_index.csv")

gini_plot <-
ggplot(gini_index, aes(x = year, y = estimate)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_classic() +
  labs(y = "", x = "", title = "Gini Index of Income Inequality") +
  theme(
    plot.title = element_text(face = "bold", hjust = .5),
    panel.grid.major.y = element_line(linetype = "dashed", size = .4)
  )

jpeg(filename = "../final_graphs/complete/gini_index.jpg", height = 20*72, width = 20*72, units = 'px', res = 300)

gini_plot

dev.off()


## ...........................
## Life Expectancy ----

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
 scale_color_manual(values = alice_pal[c(1, 3)]) +
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


jpeg(filename = "../final_graphs/complete/life_exp.jpg", height = 45*72, width = 35*72, units = 'px', res = 300)

life_exp_graph

dev.off()


## ...........................
## Life Expectancy Map ----

life_exp_map_gg <-
  ggplot(ahdi_map) +
  geom_sf( aes(fill = life_exp), color = "black", alpha = .9) +
  scale_fill_fermenter(limits = c(70,90), palette = "BuPu", direction = 1,   type = "seq", n.breaks = 8) +
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

jpeg(filename = "../final_graphs/complete/life_exp_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

life_exp_map_gg

dev.off()


## ...........................
## Food Insecurity  ----

# devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
# install.packages("emojifont")
library(emojifont)
library(ggpubr)

## Alternatively: https://github.com/hrbrmstr/waffle/

waffle_pal <- brewer.pal(5, "BuPu")[c(5,2)]

## Values transcribed from https://map.feedingamerica.org/county/2018/overall/virginia
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

jpeg(filename = "../final_graphs/complete/food_insecure.jpg", height = 20*72, width = 40*72, units = 'px', res = 300)

psquare

dev.off()

jpeg(filename = "../final_graphs/complete/food_insecure_icon.jpg", height = 20*72, width = 40*72, units = 'px', res = 300)

picon

dev.off()


## ...........................
## AHDI Table  ----

library(reactable)

ahdi_table <- read_csv("ahdi_table.csv") %>%
  mutate(
    county = case_when(county == "total" ~ "Virginia",
                       TRUE ~ county
    ))

hlth_cols <- "life_exp"
ed_cols <- c("hs_grad", "bac_deg", "grad_deg", "school_enroll")
inc_cols <- "pers_earn"
ahdi_sub <- ahdi_table[, c("county", "ahdi", hlth_cols, ed_cols, inc_cols)]

ahdi_sub <- ahdi_sub %>% mutate(county = recode(county, 
                                                "Charlottesville City" = "Charlottesville"))

bupu <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7")
ahdi_pal <- function(x) rgb(colorRamp(bupu)(x), maxColorValue = 255) 
## "#e5f5e0", "#238b45" - colorbrewer Greens 2/7
hlth_pal <- function(x) rgb(colorRamp(bupu)(x), maxColorValue = 255) 
## "#fff5f0", "#fc9272" - colorbrewer Reds 1/4
educ_pal <- function(x) rgb(colorRamp(bupu)(x), maxColorValue = 255) 
## "#deebf7", "#2171b5" - colorbrewer Blues 2/7
earn_pal <- function(x) rgb(colorRamp(bupu)(x), maxColorValue = 255) 
## "#efedf5", "#6a51a3" - colorbrewer Blues 2/7

reactable(ahdi_sub, 
          columns = list(
            county = colDef(name = ""),
            ahdi = colDef(name = "American HDI", align = "center",
                          style = function(value) {
                            normalized <- (value - min(ahdi_sub$ahdi)) / (max(ahdi_sub$ahdi) - min(ahdi_sub$ahdi))
                            color <- ahdi_pal(normalized)
                            list(background = color)
                            },
                          format = colFormat(digits = 2)),
            life_exp = colDef(name = "Life Expectancy at Birth", align = "center",
                              style = function(value) {
                                normalized <- (value - min(ahdi_sub$life_exp)) / (max(ahdi_sub$life_exp) - min(ahdi_sub$life_exp))
                                color <- hlth_pal(normalized)
                                list(background = color)
                              },
                              format = colFormat(digits = 1)),
            hs_grad = colDef(name = "HS Degree or more (Adults 25+)", align = "center",
                             style = function(value) {
                               normalized <- (value - min(ahdi_sub$hs_grad)) / (max(ahdi_sub$hs_grad) - min(ahdi_sub$hs_grad))
                               color <- educ_pal(normalized)
                               list(background = color)
                             },
                             format = colFormat(digits = 1, suffix = "%")),
            bac_deg = colDef(name = "Bachelors Degree or more (Adults 25+)", align = "center",
                             style = function(value) {
                               normalized <- (value - min(ahdi_sub$bac_deg)) / (max(ahdi_sub$bac_deg) - min(ahdi_sub$bac_deg))
                               color <- educ_pal(normalized)
                               list(background = color)
                             },
                             format = colFormat(digits = 1, suffix = "%")),
            grad_deg = colDef(name = "Grad/ Professional Degree (Adults 25+)", align = "center",
                              style = function(value) {
                                normalized <- (value - min(ahdi_sub$grad_deg)) / (max(ahdi_sub$grad_deg) - min(ahdi_sub$grad_deg))
                                color <- educ_pal(normalized)
                                list(background = color)
                              },
                              format = colFormat(digits = 1, suffix = "%")),
            school_enroll = colDef(name = "School Enrollment (Ages 3-24)", align = "center",
                                   style = function(value) {
                                     normalized <- (value - min(ahdi_sub$school_enroll)) / (max(ahdi_sub$school_enroll) - min(ahdi_sub$school_enroll))
                                     color <- educ_pal(normalized)
                                     list(background = color)
                                   },
                                   format = colFormat(digits = 1, suffix = "%")),
            pers_earn = colDef(name = "Median Personal Earnings (Ages 16+)", align = "center",
                               style = function(value) {
                                 normalized <- (value - min(ahdi_sub$pers_earn)) / (max(ahdi_sub$pers_earn) - min(ahdi_sub$pers_earn))
                                 color <- earn_pal(normalized)
                                 list(background = color)
                               },
                               format = colFormat(digits = 0, separators = TRUE, prefix = "$"))
            ),
          columnGroups = list(
            colGroup(name = "Health", columns = hlth_cols),
            colGroup(name = "Access to Knowledge", columns = ed_cols),
            colGroup(name = "Living Standards", columns = inc_cols)
            )
          )

## look for better way to save
## SO says saveWidget from htmlwidgets and webshot from webshot
## but this isn't working for me...
