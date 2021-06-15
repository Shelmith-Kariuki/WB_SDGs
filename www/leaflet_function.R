library(tidyverse)
library(sf)
library(leaflet)
library(rmapshaper)

## Read in the data
merged_df <- read_rds("www/merged_df.rds")

## Subset the dataset

my_df <- merged_df %>% 
            filter(Goal == "SDG 3 : Good Health and Well-being" &
                  Topic == "Health: Mortality" &
                  `Indicator Name` == "Mortality rate, under-5 (per 1,000 live births)")


## Read in the shapefiles
africa_shp <- read_sf("www/afr_g2014_2013_0/afr_g2014_2013_0.shp")
africa_shp <- rmapshaper::ms_simplify(africa_shp, keep = 0.05)
africa_shp <- africa_shp %>% 
  mutate(ISO3 = trimws(ISO3)) %>% 
  mutate(ADM0_NAME = ifelse(ADM0_NAME=="C�te d'Ivoire", "Côte d'Ivoire",
                            ifelse(ISO3 == "SSD", "South Sudan",ADM0_NAME))) %>% 
  select(ADM0_CODE, ADM0_NAME, ISO3) %>% 
  filter(!ADM0_NAME %in% c("Abyei", "Western Sahara", "Hala'ib triangle",
                           "Ma'tan al-Sarra", "Ilemi triangle") &
           !is.na(ADM0_NAME))

## Filter the data to have the records of interest
## Goal: SDG 7 : Affordable and Clean Energy
## Topic: Environment: Energy production & use

my_df <- my_df %>% 
  mutate(`Country Code` = trimws(`Country Code`))

## Merge the data with the shapefiles
merged_mapping_df <- africa_shp %>% 
  left_join(.,my_df,  by = c("ISO3" = "Country Code")) 

# leaflet_function <- function(year){
# 
# ## Filter to only get one year
# merged_mapping_df <- merged_mapping_df %>% 
#                       filter(Year == year)
#   
# 
# ### leaflet
# pal <- colorBin(palette = "YlOrRd", domain = merged_mapping_df$value)
# labels <- sprintf(
#   "<strong>%s</strong><br/><strong>%s</strong>%g<br/><strong>%s</strong>%g",
#   merged_mapping_df$ADM0_NAME,"Year: ",merged_mapping_df$Year,"Value: ", merged_mapping_df$value
# ) %>% lapply(htmltools::HTML)
# 
# 
# 
# 
# lm <- leaflet(merged_mapping_df) %>%
#   addTiles() %>% 
#   setView(lng = 20.48554, lat = 6.57549,  zoom = 3) %>% 
#   addPolygons(color = "#397E4A", weight = 1, dashArray = "3", fillColor = ~pal(value),
#               highlight = highlightOptions(
#                 weight = 4,
#                 color = "#397E4A",
#                 dashArray = "",
#                 bringToFront = TRUE),
#               label = labels,
#               labelOptions = labelOptions(
#                 style = list("font-weight" = "normal", padding = "3px 8px"),
#                 textsize = "15px",
#                 direction = "auto")) %>% 
#   addLegend(position = c("bottomright"),pal = pal, values = ~value, title = "")
# 
# }
