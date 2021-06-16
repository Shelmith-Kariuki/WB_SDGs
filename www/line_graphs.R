library(tidyverse)
library(sf)
library(leaflet)

## Read in the data
merged_df <- read_rds("www/merged_df.rds")

## Subset the dataset

## Goal: SDG 7 : Affordable and Clean Energy
## Topic: Environment: Energy production & use


## One country
line_function2 <- function(goal, topic, indicatorname,countryname,sdg_col, title ){
  
my_df <- merged_df %>% 
          filter(!is.na(Goal)) %>%
          filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
                   `Country Name` == countryname) 

line1 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, group = 1)) +
          geom_point(size = 1, color = sdg_col) +
          geom_line(size = 1, color = sdg_col) +
          plot_theme + 
          labs(title = title, x = "", y = "")
line1  

return(line1)
}          

## Multiple countries
line_function2 <- function(goal, topic, indicatorname,countryname1,countryname2, sdg_palette, title ){
  
  my_df <- merged_df %>% 
    filter(!is.na(Goal)) %>%
    filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
             (`Country Name` == countryname1|`Country Name` %in% countryname2)) 
  
  line1 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, group = `Country Name`, color = `Country Name`)) +
    geom_point(size = 1) +
    geom_line(size = 1) +
    plot_theme + 
    scale_color_manual(values = sdg_palette) + 
    labs(title = title, x = "", y = "")
  line1  
  
  return(line1)
}  

# my_df <- merged_df %>% 
#   filter(!is.na(Goal)) %>%
#   filter(Goal == "SDG 1 : No Poverty" & Topic == "Poverty: Multidimensional poverty" & 
#            `Indicator Name` == "Multidimensional poverty headcount ratio (% of total population)") 
# 
# my_df_onecountry <- my_df %>% filter(`Country Name` == "Kenya")
# 
# line1 <- ggplot(data = my_df_onecountry, aes(x = as.factor(Year), y = value, group = 1)) +
#   geom_point(size = 1, color = "#409F4D") +
#   geom_line(size = 1, color = "#409F4D") +
#   plot_theme + 
#   labs(title = title, x = "", y = "")
# line1  


# filter(Goal == "SDG 7 : Affordable and Clean Energy" &
#          Topic == "Environment: Agricultural production" &
#          `Indicator Name` == "Cereal yield (kg per hectare)")
## Several countries