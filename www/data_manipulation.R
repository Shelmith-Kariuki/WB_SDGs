## Load the libraries required
library(googlesheets4)
library(tidyverse)

## Read in the datasets
options(scipen = 999)
wb_sheet <- "https://docs.google.com/spreadsheets/d/1Mv73sv4XwuT_rgn8LP2M_367CtX_L5I6A6ZvmE5iZeY/edit#gid=618760682"
main_data <- read_sheet(wb_sheet,sheet = "Data")
country_data <- read_sheet(wb_sheet,sheet = "Country")
sdg_file_codebook <- read_sheet(wb_sheet,sheet = "codebook")
indicators_interest <- read_sheet(wb_sheet,sheet = "Indicators_Interest")
series_df <- read_sheet(wb_sheet,sheet = "Series") 
sdgs_all <- read_csv("www/SDGS - Sheet2 (3).csv")

## Manipulate the datasets and merge them
country_data <- country_data %>% 
  select(`Country Code`, `Table Name`, Region, `Income Group`) %>% 
  filter(Region %in%c("Sub-Saharan Africa","Middle East & North Africa"))

merged_df1 <- country_data %>% 
  left_join(., main_data, by = c("Country Code", "Table Name" = "Country Name")) %>% 
  rename(`Country Name` = `Table Name`) %>% 
  filter(Region == "Sub-Saharan Africa" |
           `Country Name` %in% c("Morocco", "Algeria", "Tunisia", "Libya", "Egypt, Arab Rep.", "Djibouti"))


indicators_interest <- indicators_interest %>% 
  mutate(across(everything(), ~trimws(.)))
series_df <- series_df %>% 
  mutate(across(everything(), ~trimws(.)))

series_indicator_df <- series_df %>% 
  select(`Series Code`,	`Topic`,	`Indicator Name`) %>%
  right_join(., indicators_interest, by = c("Series Code" = "Indicator_Code")) %>% 
  select(-`Indicator Name`)

merged_df2 <- merged_df1 %>%
  right_join(., series_indicator_df, by = c("Indicator Code" = "Series Code",
                                            "Indicator Name" = "Indicator_Name"))

merged_df3 <- merged_df2 %>% 
  select(`Country Code`, `Country Name`, Region, `Income Group`, 
         `Indicator Name`, `Indicator Code`, Topic, everything()) %>% 
  separate(Topic, into = c("Topic1", "Topic2"), remove = FALSE, sep = ":") %>% 
  mutate(Topic1 = ifelse(Topic == "World Bank, International Debt Statistics.", 
                         "International Debt Statistics", Topic1),
         Topic2 = ifelse(Topic == "World Bank, International Debt Statistics.",
                         "World Bank, International Debt Statistics.",Topic2))

## Geneate the Goal variable that indicates which sdg each of the indicators belongs to. I will countercheck this with Tony
merged_df3 <- merged_df3 %>%
  mutate(Topic1 = trimws(Topic1)) %>% 
  mutate(Goal = ifelse(Topic1 == "Environment", "SDG 7 : Affordable and Clean Energy",
                       ifelse(Topic1 == "Economic Policy & Debt", "SDG 17 : Partnerships to achieve the Goal",
                              ifelse(Topic1 == "Health", "SDG 3 : Good Health and Well-being",
                                     ifelse(Topic1 == "Education", "SDG 4 : Quality Education",
                                            ifelse(Topic1 == "Infrastructure", "SDG 9 : Industry, Innovation and Infrastructure",
                                                   ifelse(Topic1 == "Financial Sector", "SDG 10 : Reduced Inequality",
                                                          ifelse(Topic1 == "Public Sector", "SDG 10 : Reduced Inequality",
                                                                 ifelse(Topic1 == "Private Sector & Trade", "SDG 17 : Partnerships to achieve the Goal",
                                                                        ifelse(Topic1 == "Social Protection & Labor", "SDG 8 : Decent Work and Economic Growth",
                                                                               ifelse(Topic1 == "International Debt Statistics", "SDG 17 : Partnerships to achieve the Goal", 
                                                                                      ifelse(Topic1 == "Gender", "SDG 5 : Gender Equality","")))))))))))) %>% 
  mutate(Goal = fct_relevel(Goal, "SDG 1 : No Poverty", "SDG 2 : Zero Hunger", "SDG 3 : Good Health and Well-being",
                                    "SDG 4 : Quality Education", "SDG 5 : Gender Equality", "SDG 6 : Clean Water and Sanitation",
                                    "SDG 7 : Affordable and Clean Energy", "SDG 8 : Decent Work and Economic Growth", "SDG 9 : Industry, Innovation and Infrastructure",
                                    "SDG 10 : Reduced Inequality", "SDG 11 : Sustainable Cities and Communities", "SDG 12 : Responsible Consumption and Production",
                                    "SDG 13 : Climate Action", "SDG 14 : Life Below Water", "SDG 15 : Life on Land", "SDG 16 : Peace and Justice Strong Institutions",
                                    "SDG 17 : Partnerships to achieve the Goal"))



## Reorganise the variables
merged_df3 <- merged_df3 %>%
  select(`Country Code`, `Country Name`, Region, `Income Group`, Goal,
         Topic, Topic1, Topic2, `Indicator Code`, `Indicator Name`, everything())


## Save the data externally
write.csv(merged_df3, "www/merged_df.csv") 
