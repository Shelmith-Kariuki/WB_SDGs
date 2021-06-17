## One country
line_function <- function(goal, topic, indicatorname,countryname,sdg_col, title ){
  
my_df <- merged_df %>% 
          filter(!is.na(Goal)) %>%
          filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
                   `Country Name` == countryname) 

line1 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, group = 1)) +
          geom_point(size = 1, color = sdg_col) +
          geom_line(size = 1, color = sdg_col) +
          plot_theme + 
          labs(title = title, x = "", y = "") +
          theme(legend.position = "none")
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
