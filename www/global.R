# source("www/data_manipulation.R")
# 
## Read in the dataset
library(googlesheets4)
library(tidyverse)
merged_df <- read_rds("www/merged_df.rds")
wb_sheet <- "https://docs.google.com/spreadsheets/d/1Mv73sv4XwuT_rgn8LP2M_367CtX_L5I6A6ZvmE5iZeY/edit#gid=618760682"
sdg_file_codebook <- read_sheet(wb_sheet,sheet = "codebook")
goal_target_cols <- read_sheet(wb_sheet,sheet = "goal_target")

## Inputs lists
goals_list <- unique(as.character(sort(merged_df$Goal)))
target_list <- goal_target_cols$Target
topic_list <- sort(unique(merged_df$Topic))
indicator_list <- sort(unique(merged_df$`Indicator Name`))

## Poverty targets
T1 <- "Target 1. 1: By 2030, eradicate extreme poverty for all people everywhere, currently measured as people living on less than $1.25 a day"
T2 <- "Target 1. 2: By 2030, reduce at least by half the proportion of men, women and children of all ages living in poverty in all its dimensions according to national definitions"
T3 <- "Target 2.1: By 2030, end hunger and ensure access by all people, in particular the poor and people in vulnerable situations, including infants, to safe, nutritious and sufficient food all year round"

## Poverty indicators
I1 <- "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)"
I2 <- "Multidimensional poverty headcount ratio (% of total population)"
I3 <- "Multidimensional poverty headcount ratio, children (% of child population)"


## Background colors

goal_target_cols <-goal_target_cols %>% 
                      mutate(bg_color_scheme = paste0("<div color:white; style='background: ",Goal_col,";'>",Goal_Name,"</div>"))

goal_target_cols_sp <- unique(goal_target_cols$bg_color_scheme)
# selectInput("sdg", span(tags$i("Select a goal"),style="color:black;font-size:18px"),
#             choices = unique(sdg_file$Goal_Name))
#             
## 7. Map theme ----------------------------------------------------------
map_theme <- theme(axis.line = element_blank(), 
                   axis.title = element_blank(), 
                   axis.text = element_blank(),
                   plot.title = element_text(family = "serif", size = 16, face = "bold"), 
                   axis.ticks = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_rect("white", color = NA),
                   panel.background = element_rect("white", color = NA),
                   legend.position = "bottom", 
                   legend.direction = "horizontal",
                   legend.text = element_text(family = "serif", size = 12, colour = "black"),
                   legend.title = element_text(family = "serif", size = 14, colour = "black"),
                   plot.caption =  element_text(family = "serif", size = 14))

## 8. Plot themes ----------------------------------------------------------
plot_theme <- theme(axis.line = element_line(linetype = "solid"), 
                    axis.title = element_text(family = "serif", size = 18), 
                    axis.text = element_text(family = "serif", size = 14, colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(family = "serif", size = 20, face = "bold"), 
                    plot.background = element_rect("white", color = NA),
                    panel.background = element_rect("white", color = NA),
                    legend.position = "right", 
                    legend.direction = "vertical",
                    plot.caption =  element_text(family = "serif", size = 14)) 

list_countries <- unique(merged_df$`Country Name`)


low_income <- merged_df %>% filter(`Income Group` == "Low income") %>% distinct(`Country Name`) %>% pull()
lower_middle_income <- merged_df %>% filter(`Income Group` == "Lower middle income") %>% distinct(`Country Name`) %>% pull()
upper_middle_income <- merged_df %>% filter(`Income Group` == "Upper middle income") %>% distinct(`Country Name`) %>% pull()
high_income <- merged_df %>% filter(`Income Group` == "High income") %>% distinct(`Country Name`) %>% pull()

years <- as.character(c(1990:2020))