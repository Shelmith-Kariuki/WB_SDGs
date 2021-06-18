## One country

img <- grid::rasterGrob("www/worldbank_logo.png", interpolate = TRUE)


line_function <- function(goal, topic, indicatorname,countryname,sdg_col, title ){
  
my_df <- merged_df %>% 
          filter(!is.na(Goal)) %>%
          filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
                   `Country Name` == countryname) 

line1 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, group = 1, 
                                  text = paste('Country: ', `Country Name`,"\n",
                                         'Year: ', Year,"\n",
                                         'Value: ', round(value, 2)))) +
          geom_point(size = 1, color = sdg_col) +
          geom_line(size = 1, color = sdg_col) +
          plot_theme + 
          labs(title = title, subtitle = paste0("\n(",countryname , ")"), x = "", y = "", caption = "") +
          theme(legend.position = "none") +
          coord_cartesian(xlim = c(min(my_df$Year, na.rm = TRUE), max(my_df$Year, na.rm = TRUE)), expand = FALSE, clip = 'off') +
          annotation_custom(img, ymin = max(my_df$value, na.rm = TRUE)-2, ymax = max(my_df$value, na.rm = TRUE), 
                            xmin = max(my_df$Year, na.rm = TRUE)-2, xmax = max(my_df$Year, na.rm = TRUE))


if(title %in% grep("%", title, value = TRUE, ignore.case = TRUE)){
  line1 <- line1 + scale_y_continuous(labels = scales::percent)
}else{
  if(max(my_df$value, na.rm = TRUE) > 10000){
    line1 <- line1 + scale_y_continuous(labels = scales::comma) 
  }
}

line1_plotly <- ggplotly(line1, dynamicTicks = TRUE, tooltip = c("text")) %>%
                  layout(hovermode = "x") %>% 
                  config(displayModeBar = F)
  

return(line1_plotly)
}          

## Multiple countries
line_function2 <- function(goal, topic, indicatorname,countryname1,countryname2, sdg_col, sdg_palette, title ){
  
  my_df <- merged_df %>% 
    filter(!is.na(Goal)) %>%
    filter(Goal == goal & Topic == topic & `Indicator Name` == indicatorname &
             (`Country Name` == countryname1|`Country Name` %in% countryname2)) %>% 
    mutate(`Country Name` = fct_relevel(`Country Name`, countryname1))
  
  line2 <- ggplot(data = my_df, aes(x = as.factor(Year), y = value, 
                                    group = `Country Name`, color = `Country Name`,
                                    text = paste('Country: ', `Country Name`,"\n",
                                                 'Year: ', Year,"\n",
                                                 'Value: ', round(value, 2)))) +
    geom_point(size = 0.5) +
    geom_line(size = 0.5) +
    plot_theme +
    # scale_colour_discrete(guide = 'none') +
    # scale_x_discrete(expand=c(0, 1)) +
    # geom_dl(aes(label = `Country Name`), method = list(dl.combine("last.points")), cex = 1.0) +
    scale_color_manual(values = c(sdg_col, sdg_palette)) + 
    labs(title = title, x = "", y = "") +
    coord_cartesian(xlim = c(min(my_df$Year, na.rm = TRUE), max(my_df$Year, na.rm = TRUE)), expand = FALSE, clip = 'off')
  
  
  if(title %in% grep("%", title, value = TRUE, ignore.case = TRUE)){
    line2 <- line2 + scale_y_continuous(labels = scales::percent)
  }else{
    if(max(my_df$value, na.rm = TRUE) > 10000){
      line2 <- line2 + scale_y_continuous(labels = scales::comma) 
    }
  }

  
  line2_plotly <- ggplotly(line2, dynamicTicks = TRUE, tooltip = c("text")) %>%
                    layout(hovermode = "x") %>% 
                      config(displayModeBar = F)
  
  
  return(line2_plotly)
}  
