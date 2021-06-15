library(tidyverse)
library(sf)
library(leaflet)

## Read in the data
merged_df <- read_rds("www/merged_df.rds")

## Subset the dataset

## Goal: SDG 7 : Affordable and Clean Energy
## Topic: Environment: Energy production & use

line_function <- function(goal, topic, indicatorname,countryname, title ){
  
my_df <- merged_df %>% 
          filter(!is.na(Goal)) %>%
          filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
                   `Country Name` == countryname) 

# my_df_onecountry <- my_df %>% filter(`Country Name` == "Kenya")

## One country
line1 <- try(ggplot(data = my_df, aes(x = as.factor(Year), y = value, group = 1)) +
          geom_point(size = 1, color = "#409F4D") +
          geom_line(size = 1, color = "#409F4D") +
          plot_theme + 
          labs(title = title, x = "", y = ""))
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