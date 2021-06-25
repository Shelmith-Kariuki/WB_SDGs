## One country

img <- grid::rasterGrob("www/worldbank_logo.png", interpolate = TRUE)


line_function <- function(goal, topic, indicatorname,countryname,sdg_col, title ){
  
my_df <- merged_df %>% 
          mutate(across(everything(), ~trimws(.))) %>% 
          mutate(value = as.numeric(value)) %>% 
          filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
                   `Country Name` == countryname) 

if(nrow(my_df) >0){
if(unique(my_df$`Indicator Name`) %in% grep("%", unique(my_df$`Indicator Name`), value = TRUE, ignore.case = TRUE)){
  tooltip_text <-  paste('Country: ', my_df$`Country Name`,"\n",
                         'Year: ', my_df$Year,"\n",
                         'Value: ', round(my_df$value, 1), "%")
}else{
  tooltip_text <-  paste('Country: ', my_df$`Country Name`,"\n",
                         'Year: ', my_df$Year,"\n",
                         'Value: ', round(my_df$value, 2))
}


line1 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, group = 1, 
                                  text = tooltip_text)) +
          #geom_point(size = 0.5, color = sdg_col) +
          geom_line(size = 0.5, color = sdg_col) +
          plot_theme + 
          labs(title = title, subtitle = paste0("\n(",countryname , ")"), x = "", y = "", caption = "") +
          theme(legend.position = "none") +
          coord_cartesian(expand = FALSE, clip = 'off')


if(unique(my_df$`Indicator Name`) %in% grep("%", unique(my_df$`Indicator Name`), value = TRUE, ignore.case = TRUE)){
  
  if(max(my_df$value, na.rm = TRUE) <= 90){
    line1b <- line1 + scale_y_continuous(limits = c(0, max(my_df$value, na.rm = TRUE) + 10), 
                                         breaks = seq(0, max(my_df$value, na.rm = TRUE) + 10, by = 5),
                                         labels = function(x) paste0(x, "%"))
  }else
    line1b <- line1 + scale_y_continuous(limits = c(0, max(my_df$value, na.rm = TRUE)), 
                                         breaks = seq(0, max(my_df$value, na.rm = TRUE), by = 5),
                                         labels = function(x) paste0(x, "%"))
  }else{
  if(max(my_df$value, na.rm = TRUE) > 10000){
    line1b <- line1 + scale_y_continuous(labels = scales::comma) 
  }else{
    line1b <- line1
  }
}


line1_plotly <- ggplotly(line1b, tooltip = c("text")) %>%
                  layout(hovermode = "x") %>% 
                  config(displayModeBar = F)
  

return(line1_plotly)
}}         

## Multiple countries
line_function2 <- function(goal, topic, indicatorname,countryname1,countryname2, sdg_col, sdg_palette, title ){
  
  my_df <- merged_df %>% 
    mutate(across(everything(), ~trimws(.))) %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
             (`Country Name` == countryname1|`Country Name` %in% countryname2)) %>% 
    mutate(`Country Name` = fct_relevel(`Country Name`, countryname1))
  
  if(nrow(my_df) >0){
  if(unique(my_df$`Indicator Name`) %in% grep("%", unique(my_df$`Indicator Name`), value = TRUE, ignore.case = TRUE)){
    tooltip_text <-  paste('Country: ', my_df$`Country Name`,"\n",
                           'Year: ', my_df$Year,"\n",
                           'Value: ', round(my_df$value, 1), "%")
  }else{
    tooltip_text <-  paste('Country: ', my_df$`Country Name`,"\n",
                           'Year: ', my_df$Year,"\n",
                           'Value: ', round(my_df$value, 2))
  }

  line2 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, 
                                    group = `Country Name`, color = `Country Name`,
                                    text = tooltip_text)) +
    # geom_point(size = 0.5) +
    geom_line(size = 0.5) +
    plot_theme +
    scale_color_manual(values = c(sdg_col, sdg_palette)) + 
    labs(title = title, x = "", y = "") +
    coord_cartesian(expand = FALSE, clip = 'off')
  
    #scale_y_continuous(limits = c(0,max(my_df$value, na.rm = TRUE)), expand = FALSE)
    # coord_cartesian(xlim = c(min(my_df$Year, na.rm = TRUE), max(my_df$Year, na.rm = TRUE)), expand = FALSE, clip = 'off')
  
 
 if(unique(my_df$`Indicator Name`) %in% grep("%", unique(my_df$`Indicator Name`), value = TRUE, ignore.case = TRUE)){
   
   if(max(my_df$value, na.rm = TRUE) <= 90){
     line2b <- line2 + scale_y_continuous(limits = c(0, max(my_df$value, na.rm = TRUE) + 10), 
                                          breaks = seq(0, max(my_df$value, na.rm = TRUE) + 10, by = 5),
                                          labels = function(x) paste0(x, "%"))
   }else
     line2b <- line2 + scale_y_continuous(limits = c(0, max(my_df$value, na.rm = TRUE)), 
                                            breaks = seq(0, max(my_df$value, na.rm = TRUE), by = 5),
                                            labels = function(x) paste0(x, "%"))
   }else{
     if(max(my_df$value, na.rm = TRUE) > 10000){
       line2b <- line2 + scale_y_continuous(labels = scales::comma) 
     }else{
       line2b <- line2
     }
   }
  
  line2_plotly <- ggplotly(line2b, tooltip = c("text")) %>%
                    layout(hovermode = "x") %>% 
                      config(displayModeBar = F)
  
  
  return(line2_plotly)
} } 
