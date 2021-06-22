## Load the packages
library(googlesheets4)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(kableExtra)
library(shinycssloaders)
library(shinyalert)
library(sf)
library(tidyverse)
library(directlabels)
library(plotly)
library(leaflet)
library(googlesheets4)
library(googledrive)
library(openxlsx)

options(scipen = 999)

## Read in the datasets
merged_df <- read_rds("www/merged_df.rds")
wb_sheet <- "https://docs.google.com/spreadsheets/d/1Mv73sv4XwuT_rgn8LP2M_367CtX_L5I6A6ZvmE5iZeY/edit#gid=618760682"
# sdg_file_codebook <- read_sheet(wb_sheet,sheet = "codebook")
# goal_target_cols <- read_sheet(wb_sheet,sheet = "goal_target")

sdg_file_codebook <- read.xlsx("www/WB_SDG.xlsx", sheet = "codebook")
goal_target_cols  <- read.xlsx("www/WB_SDG.xlsx", sheet = "goal_target")

## Inputs lists
goals_list <- unique(as.character(sort(merged_df$Goal)))
target_list <- goal_target_cols$Target
topic_list <- sort(unique(merged_df$Topic))
indicator_list <- sort(unique(merged_df$`Indicator Name`))

## pickerInput function
picker_input_function <- function(id, title, choices){
  
  pickerInput(
    inputId = id,
    label = span(title, style="color:black;font-size:16px"), 
    choices = choices)
  
}

## reactive ui inputs
### sdg banners
### slider components (colors and min max years)
### countries present in each subset of data (per goal, topic and indicator) 
### 
list_countries <- unique(merged_df$`Country Name`)


low_income <- merged_df %>% filter(`Income Group` == "Low income") %>% distinct(`Country Name`) %>% pull()
lower_middle_income <- merged_df %>% filter(`Income Group` == "Lower middle income") %>% distinct(`Country Name`) %>% pull()
upper_middle_income <- merged_df %>% filter(`Income Group` == "Upper middle income") %>% distinct(`Country Name`) %>% pull()
high_income <- merged_df %>% filter(`Income Group` == "High income") %>% distinct(`Country Name`) %>% pull()

# years <- as.character(c(1990:2020))

## Map theme ----------------------------------------------------------
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

## Plot themes ----------------------------------------------------------
plot_theme <- theme(axis.line = element_line(linetype = "solid"), 
                    axis.title = element_text(family = "serif", size = 18), 
                    axis.text = element_text(family = "serif", size = 10, colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.title = element_text(family = "serif", size = 18, face = "bold"), 
                    plot.subtitle = element_text(family = "serif", size = 18), 
                    plot.background = element_rect("white", color = NA),
                    panel.background = element_rect("white", color = NA),
                    legend.position = "none", 
                    legend.direction = "horizontal",
                    legend.title = element_blank(),
                    legend.text = element_text(family = "serif", size = 14, colour = "black"),
                    legend.key=element_blank(),
                    legend.key.width = unit(4, "cm"),
                    plot.caption =  element_text(family = "serif", size = 14)) 

