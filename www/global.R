# source("www/data_manipulation.R")
# 
## Read in the dataset
merged_df <- read_csv("www/merged_df.csv")
merged_df <- merged_df %>% select(-X1)

## Inputs lists
goals_list <- unique(merged_df$Goal)
topic_list <- unique(merged_df$Topic)
indicator_list <- unique(merged_df$`Indicator Name`)

## Poverty targets
T1 <- "Target 1. 1: By 2030, eradicate extreme poverty for all people everywhere, currently measured as people living on less than $1.25 a day"
T2 <- "Target 1. 2: By 2030, reduce at least by half the proportion of men, women and children of all ages living in poverty in all its dimensions according to national definitions"
T3 <- "Target 2.1: By 2030, end hunger and ensure access by all people, in particular the poor and people in vulnerable situations, including infants, to safe, nutritious and sufficient food all year round"

## Poverty indicators
I1 <- "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)"
I2 <- "Multidimensional poverty headcount ratio (% of total population)"
I3 <- "Multidimensional poverty headcount ratio, children (% of child population)"





# selectInput("sdg", span(tags$i("Select a goal"),style="color:black;font-size:18px"),
#             choices = unique(sdg_file$Goal_Name))