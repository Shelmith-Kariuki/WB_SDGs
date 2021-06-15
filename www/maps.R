library(tidyverse)
library(sf)
library(googlesheets4)
library(plotly)
library(leaflet)

## Goal: SDG 7 : Affordable and Clean Energy
## Topic: Environment: Energy production & use

## Read in the dataset
merged_df <- read_rds("www/merged_df.rds")

## Read in the shapefiles
africa_shp <- read_sf("www/afr_g2014_2013_0/afr_g2014_2013_0.shp")
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

mapping_df <- merged_df %>% 
                mutate(`Country Code` = trimws(`Country Code`))

## Merge the data with the shapefiles
merged_mapping_df <- africa_shp %>% 
                        left_join(.,mapping_df,  by = c("ISO3" = "Country Code")) 

## Reshape the data so that the years are on one column
merged_mapping_df2 <- merged_mapping_df %>% 
                        gather("Year", "value", `1990`:`2019`, na.rm = TRUE) %>% 
                        mutate(Year = as.numeric(Year)) %>% 
                        arrange(ADM0_NAME, Goal, Topic, `Indicator Code`, Year)


## Generate centroids
merged_mapping_df2 <- merged_mapping_df2 %>% 
  mutate(
    lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

write_rds(merged_mapping_df2, "www/merged_mapping_df.rds")