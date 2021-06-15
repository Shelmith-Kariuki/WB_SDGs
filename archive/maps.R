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
                filter(Goal == "SDG 7 : Affordable and Clean Energy" & 
                       Topic == "Environment: Energy production & use" &
                      `Indicator Name` == "Energy intensity level of primary energy (MJ/$2011 PPP GDP)") %>% 
                mutate(`Country Code` = trimws(`Country Code`))

## Merge the data with the shapefiles
merged_mapping_df <- africa_shp %>% 
                        left_join(.,mapping_df,  by = c("ISO3" = "Country Code")) 

## Reshape the data so that the years are on one column
merged_mapping_df2 <- merged_mapping_df %>% 
                        gather("Year", "value", `1990`:`2019`, na.rm = TRUE) %>% 
                        mutate(Year = as.numeric(Year))
## Select one year
merged_mapping_df2 <- merged_mapping_df2 %>% filter(Year == 2012)

## Generate centroids
merged_mapping_df2 <- merged_mapping_df2 %>% 
                        mutate(
                          lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
                          lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))


## Map the data
### ggplot2 + plotly
map3 <- ggplot(data = merged_mapping_df2, aes(text = paste0("Country: ",ADM0_NAME, "\n",  "Value: ", value))) +
  geom_sf(aes(geometry = geometry, fill = value)) +
  # geom_text(aes(label = ADM0_NAME, x = lon, y = lat), size = 2) +
  theme_void() +
  #map_theme +
  scale_fill_gradient(low = "white", high = "#FFC339") +
  labs(title = "" ) 
map3

# pop_py1b<- ggplotly(map3, tooltip = c("text")) %>% 
#   config(displayModeBar = F)
# pop_py1b

### leaflet
pal <- colorBin(palette = "YlOrRd", domain = merged_mapping_df2$value)
labels <- sprintf(
  "<strong>%s</strong><br/><strong>%s</strong>%g<br/><strong>%s</strong>%g",
  merged_mapping_df2$ADM0_NAME,"Year: ",merged_mapping_df2$Year,"Value: ", merged_mapping_df2$value
) %>% lapply(htmltools::HTML)




lm <- leaflet(merged_mapping_df2) %>%
          addTiles() %>% 
          setView(lng = 20.48554, lat = 6.57549,  zoom = 4) %>% 
          addPolygons(color = "#397E4A", weight = 1, dashArray = "3", fillColor = ~pal(value),
              highlight = highlightOptions(
                weight = 4,
                color = "#397E4A",
                dashArray = "",
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
        addLegend(position = c("bottomright"),pal = pal, values = ~value, title = "")

