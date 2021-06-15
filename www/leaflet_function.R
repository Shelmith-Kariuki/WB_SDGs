library(tidyverse)
library(sf)
library(leaflet)

## Read in the data
## merged_mapping_df <- read_rds("www/merged_mapping_df.rds")

## Subset the dataset

my_df <- merged_mapping_df2_14 %>% 
            filter(Goal == "SDG 1 : No Poverty" &
                  Topic == "Poverty: Income distribution" &
                  `Indicator Name` == "Proportion of people living below 50 percent of median income (%)") #&
                    #Year == 2012) 

### leaflet
pal <- colorBin(palette = "YlOrRd", domain = my_df$value)
labels <- sprintf(
  "<strong>%s</strong><br/><strong>%s</strong>%g<br/><strong>%s</strong>%g",
  my_df$ADM0_NAME,"Year: ",my_df$Year,"Value: ", my_df$value
) %>% lapply(htmltools::HTML)




lm <- leaflet(my_df) %>%
  addTiles() %>% 
  setView(lng = 20.48554, lat = 6.57549,  zoom = 3) %>% 
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
lm
