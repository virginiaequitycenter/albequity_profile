## ...........................
## Script name: snap_datas.R
##
## Author: Michele Claibourn
## Date Created: 2021-02-02
## Updated: 2022-01-28 mpc
## Purpose: Analysis and visuals for health section 
##
## ...........................
## set working directory

setwd("data")

## ...........................
## load packages ----

library(tidyverse)
library(jsonlite)
library(tidycensus)
library(tigris)
library("ggspatial")
library(scales)
library(sf)
# library(ggmap)


## ...........................
## set palette ----
hlth_colors <- c("#f0dbe2", "#b02c58")

hlth_pal <- function(x) rgb(colorRamp(hlth_colors)(x), maxColorValue = 255)


## ...........................
## pull store location data ----

## API query: from https://usda-fns.hub.arcgis.com/datasets/USDA-FNS::snap-store-locations/geoservice

## County query
# full_path <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/Store_Locations/FeatureServer/0/query?where=State%20%3D%20'VA'%20AND%20County%20%3D%20'ALBEMARLE'%20OR%20County%20%3D%20'CHARLOTTESVILLE'&outFields=*&outSR=4326&f=json"

## lat/lon query
full_path <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/Store_Locations/FeatureServer/0/query?where=Longitude%20%3E%3D%20-78.83887%20AND%20Longitude%20%3C%3D%20-78.20938%20AND%20Latitude%20%3E%3D%2037.72264%20AND%20Latitude%20%3C%3D%2038.27793&outFields=*&outSR=4326&f=json"

##     xmin      ymin      xmax      ymax
## -78.83887  37.72264 -78.20938  38.27793

## Retrieve data
stores_json <- fromJSON(full_path)

## Extract data frame from list
stores <- stores_json$features$attributes
## 132 - Siri's estimate in revision is based on distance

## make it an SF object
stores_4326 <- st_as_sf(stores,
                        coords = c("Longitude", "Latitude"),
                        crs = 4326)


## ...........................
## pull snap recipient data ----

tract_snap <- get_acs(geography = "tract",
                        table = "S2201",
                        state = "VA",
                        county = "003",
                        survey = "acs5",
                        year = 2019,
                        cache_table = TRUE)

## could use percents or households
## households = "S2201_C03_001"
## percent = "S2201_C04_001"

tract_snap_per <- tract_snap %>%
  filter(variable == "S2201_C04_001")

tract_snap_house <- tract_snap %>%
  filter(variable == "S2201_C03_001")

## Albemarle tract data
alb_tract <- tracts(state = "VA", county = "003")

## add surrounding counties?
near_county <- counties(state = "VA")
near_county <- near_county %>% 
  filter(COUNTYFP %in% c("029", "065", "079", "109",
                         "125", "137", "015", "165"))

## add surrounding tracts?
near_tract <- tracts(state = "VA", 
                     county = c("029", "065", "079", "109",
                                "125", "137", "015", "165"))

snap_tract <- alb_tract %>%
  select(-NAME) %>%
  left_join(tract_snap_per %>%
              select(GEOID, perc = estimate)
  )  %>%
  left_join(tract_snap_house %>%
              select(GEOID, house = estimate)
  ) %>%
  left_join(tract_snap_house %>%
              select(GEOID, house = estimate)
            )

## ...........................
## Map snap use/food retailers ----
snap_tract_4326 <- sf::st_transform(snap_tract, 4326)
st_bbox(snap_tract_4326)

snap_map <-
  ggplot(snap_tract_4326) +
  geom_sf(aes(fill = house), color = "black") +
  #geom_sf(data = near_county, fill = "white") +
  #geom_sf(data = near_tract, fill = "white") +
  geom_sf(data = stores_4326, size = 1, shape = 21, fill = "darkblue") +
  scale_fill_steps(
    low = hlth_colors[1],
    high = hlth_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Number of Housholds Receiving SNAP Benefits") +
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

jpeg(filename = "../final_graphs/health/snap_locations_map.jpg", height = 40*72, width = 40*72, units = 'px', res = 300)

snap_map

dev.off()
