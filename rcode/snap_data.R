library(tidyverse)
library(jsonlite)
library(tidycensus)
library(tigris)
library("ggspatial")
library(scales)
library(sf)
# library(ggmap)

# API query: from https://usda-fns.hub.arcgis.com/datasets/USDA-FNS::snap-store-locations/geoservice

full_path <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/Store_Locations/FeatureServer/0/query?where=State%20%3D%20'VA'%20AND%20County%20%3D%20'ALBEMARLE'%20OR%20County%20%3D%20'CHARLOTTESVILLE'&outFields=*&outSR=4326&f=json"

# Retrieve data
stores_json <- fromJSON(full_path)

# Extract data frame from list
stores <- stores_json$features$attributes
# only 107 - Siri's estimate in revision is based on distance
# from a point, so includes things in surrounding counties
# but might miss parts of the county that fall outside
# the circle...

# make it an SF object
stores_4326 <- st_as_sf(stores, 
                        coords = c("Longitude", "Latitude"),
                        crs = 4326)

# Albemarle snap data by tract
tract_snap <- get_acs(geography = "tract", 
                        table = "S2201", 
                        state = "VA", 
                        county = "003", 
                        survey = "acs5", 
                        year = 2019, 
                        cache_table = TRUE)

# not sure if it makes more sense to use percent or households
# percent = "S2201_C04_001"
# households = "S2201_C03_001"
tract_snap_per <- tract_snap %>% 
  filter(variable == "S2201_C04_001") 

tract_snap_house <- tract_snap %>% 
  filter(variable == "S2201_C03_001") 


# Albemarle tract data
alb_tract <- tracts(state = "VA", county = "003") 

snap_tract <- alb_tract %>%
  select(-NAME) %>%
  left_join(tract_snap_per %>%
              select(GEOID, perc = estimate)
              )  %>%
  left_join(tract_snap_house %>%
              select(GEOID, house = estimate)
            )

# ESTIMATES AREN'T JOINING -- WHYYYYYYY?????
## The NAMES field was the issue - SDP

# So figure below isn't displaying color

# Map snap use by tract with retailers overlaid
snap_tract_4326 <- sf::st_transform(snap_tract, 4326)

# Pal 3
hlth_colors <- c("#f0dbe2", "#b02c58")

hlth_pal <- function(x) rgb(colorRamp(hlth_colors)(x), maxColorValue = 255) 

snap_map <-
  ggplot(snap_tract_4326) +
  geom_sf(aes(fill = house), color = "black") +
  geom_sf(data = stores_4326, size = 1, shape = 21, fill = "darkblue") +
  scale_fill_steps(
    low = hlth_colors[1],
    high = hlth_colors[2],
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill",
    n.breaks = 10,
  #  labels = percent
  ) +
  theme_void() +
  guides(fill =
           guide_colourbar(title.position="top", title.hjust = 0.5,
                           barwidth = 20)
  ) +
  labs(fill = "Number Housholds Receiving SNAP Benefits") +
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

snap_map
