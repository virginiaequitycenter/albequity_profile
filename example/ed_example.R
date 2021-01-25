
# Setup ----
# working directory
setwd("data") # wherever the education_distribution.csv file resides

# libraries
library(tidyverse)
library(RColorBrewer)

# data source
ed_dist <- read_csv("education_distrbution.csv")


# Data prep ----
# This data particular data was pulled with tidycensus
# none of this first round of data prep is entirely relevant

# Some additional cleaning
proper=function(s) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(s), perl=TRUE)

ed_dist <-read_csv("education_distrbution.csv") %>%
  mutate(Race = proper(Race)) %>%
  mutate(
    degree = factor(degree,
                    levels = c(
                      "Less than high school diploma",
                      "High school graduate",
                      "Some college or associate's degree",
                      "Bachelor's degree or higher")),
    Race = factor(Race,
                  levels = rev(c("All",
                     "White Alone",
                     "Black Or African American Alone",
                     "Asian Alone",
                     "Native Hawaiian And Other Pacific Islander Alone",
                     "American Indian And Alaska Native Alone",
                     "Two Or More Races",
                     "Some Other Race Alone",
                     "Hispanic Or Latino")))
  ) %>%
  filter(!is.na(Race), Sex == "All")

# The approach below requires that a summary data frame has been created
# (that is, we can't graph the raw data), 
# e.g., using group_by(var) and summarizing proportions/percents
# then creating an overall summary in the same form (e.g, same vars) as above
# and using bind_rows() to combine it with the grouped summary
# In short, it needs to look something like ed_dist with
# the variable for the axis (here, Race), a variable for the fill (here degree),
#   and a variable with the value to be graphed (here percent).


# Figure prep ----
# To make this work, some groups must be graphed as negative values
# so the data must be split into two. Because we're graphing 2 dfs,
# the colors and legends are trickier.

# a nice explanation: http://rnotr.com/likert/ggplot/barometer/likert-plots/

# create a palette and attach it directly to the data 
ed_pal <- brewer.pal(5, "BuPu")[-1] 

ed_dist <- ed_dist %>% arrange(degree) %>% # data must be sorted by fill var
  mutate(col = rep(ed_pal, each = 9)) # each degree is repeated 9 times

# split the data into df above and below center line
ed_graph_high <- ed_dist %>% 
  filter(degree %in% c("Bachelor's degree or higher",
                       "Some college or associate's degree")) %>% 
  # get the sum of the stacked bars for later positioning of values
  group_by(Race) %>% 
  mutate(height = (cumsum(percent) - percent/2)) %>% 
  ungroup() %>% 
  # order the levels: the first element will appear at end of stacked bar
  # here we'll be using the color as the fill
  mutate(col = factor(col, 
                         levels = c("#810F7C", # Bachelors
                                    "#8856A7")), # Some
         # add the display label to df (if above 10)
         label = if_else(percent > 10, paste0(round(percent), "%"), "")) 
         

ed_graph_low <- ed_dist %>% 
  filter(!(degree %in% c("Bachelor's degree or higher",
                         "Some college or associate's degree"))) %>%   
  # get the sum of the stacked bars for later positioning of values
  # left of center values will be negative, so the order of levels is reversed
  mutate(degree = fct_relevel(degree, "High school graduate", after = 0L)) %>% 
  group_by(Race) %>% 
  arrange(degree) %>% 
  mutate(height = (cumsum(percent) - percent/2)) %>% 
  ungroup() %>% 
  # order levels: first element is at end of stacked bar
  mutate(col = factor(col, 
                         levels = c("#B3CDE3", # less than HS
                                    "#8C96C6")), # HS
         # add the display label to df (if above 10)
         label = if_else(percent > 10, paste0(round(percent), "%"), ""))
         

# create labels to use for legend (because defaults will be messed up)
deglevels <- c("Less than high school diploma", 
               "High school graduate", 
               "Some college or associate's degree", 
               "Bachelor's degree or higher")

ggplot() +
  # add the bars to the right of center (note that the color is the fill)
  geom_bar(data = ed_graph_high,             
           aes(x = Race, y = percent, fill = col), 
           position="stack", stat="identity") +
  # add the bars to the left of center (note the - sign in y)
  geom_bar(data = ed_graph_low,
           aes(x = Race, y = -percent, fill = col), 
           position="stack", stat="identity") + 
  # manually create the legend info with deglevels and ed_pal
  scale_fill_identity("", labels = deglevels, 
                      breaks=ed_pal, guide="legend") +
  # add a white center line
  geom_hline(yintercept = 0, color = "white") + 
  # the str_wrap function just wraps the long race labels on the axis
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  # add percent labels to bars (for bars left of center, note - sign in y)
  geom_text(data = ed_graph_high, 
            aes(x = Race, y = height, label = label), color = "white", 
            alpha = 1, hjust = .5, size = 2.75) +
  geom_text(data = ed_graph_low, 
            aes(x = Race, y = -height, label = label), color = "black", 
            alpha = 1, hjust = .5, size = 2.75) +
  # some final touches
  labs(title = "Wow, that was a lot of effort!", x = "", y = "Percent") +
  theme_classic() +
  theme(legend.position = "bottom") +
  coord_flip()

ggsave("../example/stacked_recentered.jpg")
