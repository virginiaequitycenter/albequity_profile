library(tidyverse)
library(kableExtra)
library(scales)

# bupu <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7")
# hlth_colors <- c("#f0dbe2", "#b02c58")
# educ_colors <- c("#e7dbbc", "#e68026")
# inc_colors <- c("#b1c5be", "#106449")
# 
# # try using endpoints of original palettes
# pal_all <- c("#8856a7", "#b02c58", "#e68026", "#106449")
# pal_hdi <- c("#8856a7", "#f0dbe2", "#e7dbbc", "#b1c5be")
# pal_hlth <- c("#b3cde3", "#b02c58", "#e7dbbc", "#b1c5be")
# pal_educ <- c("#b3cde3", "#f0dbe2", "#e68026", "#b1c5be")
# pal_inc <- c("#b3cde3", "#f0dbe2", "#e7dbbc", "#106449")
# 
# show_col(pal_all)
# show_col(pal_hdi)
# show_col(pal_hlth)
# show_col(pal_educ)
# show_col(pal_inc)

# # would rather add transparency to non-centered component
# # install.packages("colorspace")
# library(colorspace)
# show_col(adjust_transparency("#8856a7", alpha = c(0.33, 0.5, 1)))
# show_col(adjust_transparency("#b02c58", alpha = c(0.33, 0.5, 1)))
# show_col(adjust_transparency("#e68026", alpha = c(0.33, 0.5, 1)))
# show_col(adjust_transparency("#106449", alpha = c(0.33, 0.5, 1)))
# # use alpha = .33
# 
# pal_all <- c("#8856a7FF", "#b02c58FF", "#e68026FF", "#106449FF")
# pal_hdi <- c("#8856a7FF", "#b02c5854", "#e6802654", "#10644954")
# pal_hlth <- c("#8856a754", "#b02c58FF", "#e6802654", "#10644954")
# pal_educ <- c("#8856a754", "#b02c5854", "#e68026FF", "#10644954")
# pal_inc <- c("#8856a754", "#b02c5854", "#e6802654", "#106449FF")
# 
# show_col(pal_all)
# show_col(pal_hdi)
# show_col(pal_hlth)
# show_col(pal_educ)
# show_col(pal_inc)
# # still not sure
# 
# df <- data.frame(Dimension = 
#   c("Human Development Index", 
#     "Long and Healthy Life", 
#     "Access to Knowledge", 
#     "Decent Standard of Living"))
# 
# df %>% kbl() %>% 
#   kable_paper("hover", full_width = F) %>% 
#   row_spec(1, background = pal_all[1]) %>% 
#   row_spec(2, background = pal_all[2]) %>% 
#   row_spec(3, background = pal_all[3]) %>% 
#   row_spec(4, background = pal_all[4]) %>% 
#   column_spec(1, color = "white")
# 
# df %>% kbl() %>% 
#   kable_paper("hover", full_width = F) %>% 
#   row_spec(1, background = pal_hdi[1], color = "white") %>% 
#   row_spec(2, background = pal_hdi[2]) %>% 
#   row_spec(3, background = pal_hdi[3]) %>% 
#   row_spec(4, background = pal_hdi[4]) 
# 
# df %>% kbl() %>% 
#   kable_paper("hover", full_width = F) %>% 
#   row_spec(1, background = pal_hlth[1]) %>% 
#   row_spec(2, background = pal_hlth[2], color = "white") %>% 
#   row_spec(3, background = pal_hlth[3]) %>% 
#   row_spec(4, background = pal_hlth[4]) 
# 
# df %>% kbl() %>% 
#   kable_paper("hover", full_width = F) %>% 
#   row_spec(1, background = pal_educ[1]) %>% 
#   row_spec(2, background = pal_educ[2]) %>% 
#   row_spec(3, background = pal_educ[3], color = "white") %>% 
#   row_spec(4, background = pal_educ[4]) 
# 
# df %>% kbl() %>% 
#   kable_paper("hover", full_width = F) %>% 
#   row_spec(1, background = pal_inc[1]) %>% 
#   row_spec(2, background = pal_inc[2]) %>% 
#   row_spec(3, background = pal_inc[3]) %>% 
#   row_spec(4, background = pal_inc[4], color = "white") 

# closer, but I'm not loving it yet -- I think the purple and red are probably too 
# similar for this? 

df2 <- data.frame(Ch_1 = "Demographic Profile",
                  Ch_2 = "Human Development Index",
                  Ch_3 = "Long and Healthy Life",
                  Ch_4 = "Access to Knowledge",
                  Ch_5 = "Decent Standard of Living",
                  Ch_6 = "Looking Forward")

# all bright
bright <- df2 %>% kbl() %>% 
  kable_paper(full_width = F) %>% 
  column_spec(1, background = "#f9f9f9", color = "black") %>% 
  column_spec(2, background = "#8856a7FF", color = "white") %>% 
  column_spec(3, background = "#b02c58FF", color = "white") %>% 
  column_spec(4, background = "#e68026FF", color = "white") %>% 
  column_spec(5, background = "#106449FF", color = "white") %>% 
  column_spec(6, background = "#696969", color = "white") 

# all diminshed
dim <- df2 %>% kbl() %>% 
  kable_paper(full_width = F) %>% 
  column_spec(1, background = "#f9f9f9", color = "#C0C0C0") %>% 
  column_spec(2, background = "#8856a7FF",color = "#808080", ) %>% 
  column_spec(3, background = "#b02c58FF", color = "#808080") %>% 
  column_spec(4, background = "#e68026FF", color = "#808080") %>% 
  column_spec(5, background = "#106449FF", color = "#C0C0C0") %>% 
  column_spec(6, background = "#696969", color = "#C0C0C0") 

save_kable(dim, "color_guides_dim.png")
save_kable(bright, "color_guides_bright.png")
