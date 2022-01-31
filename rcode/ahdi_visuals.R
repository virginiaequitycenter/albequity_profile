## ...........................
## Script name: ahdi_visuals.R
##
## Authors: Michele Claibourn, Sam Powers
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Analysis and visuals for AHDI section 
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
library(reactable)

options(scipen = 6, digits = 4) # to view outputs in non-scientific notation
select <- dplyr::select # avoid function name conflicts


## ...........................
## set palettes ----
bupu <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7") # bupu (1-4/4)
hlth_colors <- c("#fff5f0", "#fc9272") # colorbrewer Reds 1,4/9
educ_colors <- c("#fee6ce", "#f16913") # colorbrewer Oranges 2,6/9
inc_colors <- c("#e5f5e0", "#238b45") # colorbrewer Greens 2,7/9


## ...........................
# AHDI Table ----

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


ahdi_pal <- function(x) rgb(colorRamp(bupu)(x), maxColorValue = 255) 
hlth_pal <- function(x) rgb(colorRamp(hlth_colors)(x), maxColorValue = 255) 
educ_pal <- function(x) rgb(colorRamp(educ_colors)(x), maxColorValue = 255) 
earn_pal <- function(x) rgb(colorRamp(inc_colors)(x), maxColorValue = 255) 

ahdi_table_output <-
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

ahdi_table_output
# look for better way to save
# SO says saveWidget from htmlwidgets and webshot from webshot
# but this isn't working for me...

library("htmlwidgets")
library("webshot2")

html_file <- "ahdi_table.html"
img_file <- "../final_graphs/ahdi/ahdi_table.png"

saveWidget(widget = ahdi_table_output, file = html_file, selfcontained = TRUE)
webshot(url = html_file, file = img_file, delay = 0.1, vwidth = 1245)


## ...........................
## AHDI Tract Map ----

## Tract Data 
tract_names <- read_csv("tract_names.csv") %>%
  select(-contains("X"))

tract_ahdi <- read_csv("tract_ahdi.csv") %>%
  rename_with(~tolower(.x))  %>%
  left_join(tract_names)

alb_tract <- tracts(state = "VA", county = "003" )

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

jpeg(filename = "../final_graphs/ahdi/ahdi_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

ahdi_map_gg

dev.off()


## ...........................
## Tract Level AHDI Components ----

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

## Make the county line labeller
ahdi_alb <- ahdi_table[ahdi_table$county == "Albemarle", c("ahdi_ed", "ahdi_income", "ahdi_health", "ahdi")]
names(ahdi_alb) <- c("Education", "Income", "Health", "Composite")

## Make the color pallette
composite_pal <- c(
  hlth_colors[2],
  educ_colors[2],
  inc_colors[2],
  bupu[4]
)

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
   # filter(category == 1) %>%
    
    ggplot() +
    geom_point(
      aes(x = metric,
          y = reorder(keypoints, metric),
          color = category,
          alpha = alpha_indicator,
          size = alpha_indicator
      ),
    )  +
    scale_color_manual(values = composite_pal) +
    scale_alpha_continuous(range = c(0, .7)) +
    scale_size_continuous(range = c(2,4)) +
    scale_x_continuous( limits = c(3, 10.5), breaks = c(seq(3, 10, 1), 0)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
    
    geom_vline(xintercept = ahdi_alb[1,input][[1]],
               # linetype = "dashed",
               color = "black",
               size = .2) +
    annotate("text", x = ahdi_alb[1,input][[1]], y = 19.5,
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
  
  jpeg(filename = paste0("../final_graphs/ahdi/ahdi_dots_just_", component,".jpg"),
       height = 30*72, width = 30*72,
       units = 'px', res = 300)
  
  ahdi_dots_function(component)
  
  dev.off()
  
}

