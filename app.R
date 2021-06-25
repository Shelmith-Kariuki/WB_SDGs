source("www/global.R")
source("www/custom_css.R")
source("www/line_graphs.R")
source("www/leaflet_function.R")

# gs4_auth(cache = ".secrets")
# gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
# sheets_auth(
#   cache = ".secrets",
#   email = "kariukishelmith@gmail.com"
# )
ui <- fluidPage(title = "WDI: Sustainable Development Goals",
                useSweetAlert(), 
                tagList(
                  tags$head(tags$script(type="text/javascript", src = "code.js")),
                  navbarPage(
                    title = tags$a(href = "https://datatopics.worldbank.org/sdgs/", 
                                   span(h2(tags$b("WDI: Sustainable Development Goals\n(Africa)")),
                                 style = "color:#d5184e; font-style: bold;")),
                      tabPanel(span(h3(tags$b("Home")),
                                    style = "color:#002f54;"),
                               height = 780,
                               fluidRow(column(10,offset = 1,
                                    includeMarkdown("www/home_page.md")))
                               ),
                      tabPanel(span(h3(tags$b("Goals")),style = "color:#002f54;"),
                               tags$style(type="text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"
                               ),
                              fluidRow(
                                 column(width = 3,
                                        pickerInput(
                                          inputId = "sdg",
                                          label = span(tags$b("Click to select a goal!"),style="color:black;font-size:16px"), 
                                           choices = goals_list,
                                          selected = "SDG 8 : Decent Work and Economic Growth",
                                          choicesOpt = list(
                                            style = rep_len("font-size: 16px; line-height: 1.8 em;", length(goals_list))
                                          ))),
                               column(width = 8, offset = 1,
                                       imageOutput("image", height = 5))
                                        ),
                              br(), br(),
                              fluidRow(style = "margin-left: 3px;",
                                tabsetPanel(type = "pills", 
                                  tabPanel(span("Targets",style="color:black; font-style: bold; font-size:16px;"), 
                                           dataTableOutput("targets")),
                                  tabPanel(span("Output", style="color:black; font-style: bold; font-size:16px"), 
                                           fluidRow(style = "margin-left: 3px;",
                                             #br(),
                                             column(width = 2, 
                                                    dropdown(
                                                      tags$h5(""),
                                                      pickerInput(inputId = 'topic',
                                                                  label = 'Select Topic',
                                                                  choices = "",
                                                                  selected = "",
                                                                  options = list(`style` = "btn-primary"),
                                                                  width="950px"),
                                                      
                                                      pickerInput(inputId = 'indicator',
                                                                  label = 'Select Indicator',
                                                                  choices = "",
                                                                  selected = "",
                                                                  options = list(`style` = "btn-primary"),
                                                                  width="500px"),
                                                      
                                                      style = "unite", icon = icon("gear"),
                                                      status = "danger", width = "1000px",
                                                      tooltip = tooltipOptions(title = "Click to select topic and indicator !"),
                                                      animate = animateOptions(
                                                        enter = animations$fading_entrances$fadeInLeftBig,
                                                        exit = animations$fading_exits$fadeOutRightBig
                                                      ))),
                                             column(width = 2, offset = 8,
                                                    uiOutput("country_dropdown"))),
                                           #),
                                           br(),
                                           fluidRow(style = "margin-left: 3px;",
                                             column(width = 5, 
                                                    fluidRow(
                                                      uiOutput("slider_col"),
                                                     # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #d5184e}")),
                                                      uiOutput("year_slider")),
                                                    fluidRow(
                                                      withSpinner(leafletOutput('pl', height = 500), color = "#d5184e")
                                                    )),
                                           column(width = 7,
                                                  br(),
                                                 withSpinner(plotlyOutput("pl2", height = 630), color = "#d5184e"))))
                                           )
                                 
                                       )
                               ))))
                    


server <- function(input, output, session)({
  
  # List of goals
  sdg_reactive <- reactive({
    merged_df %>% 
      filter(Goal == input$sdg)
  })

  # Add an image for each of the goals
  output$image <- renderImage({
    
    filename = paste0("www/sdg_banners/",input$sdg,".png")
    list(src = filename, width = 800)
  }, deleteFile = FALSE)
  
  # Specific targets for each goal
  output$targets <- renderDataTable({
    targs <- goal_target_cols %>% 
      filter(Goal_Name == input$sdg) %>% 
      distinct(Target2)
    
    datatable(targs, 
              extensions = c('FixedHeader','Scroller'),
              rownames = FALSE, colnames = "",
              options = list(dom = 't', fixedHeader = TRUE))
  })
  
  ## Topics depend on the goal selected
  observeEvent(input$sdg,{
    choices <- merged_df %>% filter(Goal == input$sdg) %>% 
      mutate(Topic = trimws(Topic)) %>% 
      distinct(Topic) %>% 
      pull()
    # browser()
    updatePickerInput(session, "topic", choices = choices, selected = choices[1])
  })
  
  ## Indicators depend on the sdg and topic selected
  observeEvent(paste(input$sdg,input$topic), {
    choices <- merged_df %>%
      filter(Goal %in% input$sdg, Topic %in% input$topic) %>% 
      mutate(`Indicator Name` = trimws(`Indicator Name`)) %>%  
      distinct(`Indicator Name`) %>% 
      pull()
    updatePickerInput(session, "indicator", choices = choices, selected = choices[1])
  })
  

  output$year_slider <- renderUI({
    req(input$sdg)
    req(input$indicator!= "")
    df_years <- merged_df %>% 
      filter(`Indicator Name` == input$indicator) %>% 
      pull(Year) %>% 
      unique() %>% 
      sort()
    
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #d5184e}"))
      sliderTextInput(
      inputId = "year",
      label = "Select Year",
      grid = TRUE,
      force_edges = TRUE,
      choices = df_years,
      selected = df_years[1],
      width = "190%"
    )
  })
  
  ## Reactive elements that will be used on the table and map
  year_reactive <- reactive({
    req(input$sdg)
    req(input$topic)
    req(input$indicator)
    req(input$year)
    merged_mapping_df %>% 
      dplyr::filter(as.character(Goal) %in% input$sdg, 
                    Topic %in% input$topic,
                    `Indicator Name` %in% input$indicator,
                    Year %in% input$year) %>% 
      select(Goal, Topic, `Indicator Name`, Year, ADM0_NAME, value) %>% 
      arrange(Goal, Topic, `Indicator Name`, Year, ADM0_NAME) 
  })

  
  # Indicator reactive
  indicator_reactive <- reactive({
    req(input$sdg)
    req(input$topic)
    req(input$indicator)
    merged_df %>% 
      filter(Goal %in% input$sdg & Topic %in% input$topic & `Indicator Name` %in% input$indicator)
  })
  
    ## Leaflet map
    # Parts of the leaflet map that are static
    output$pl <- renderLeaflet({
      req(input$sdg,input$topic, input$indicator, input$year)
      leaflet(data = merged_mapping_df) %>%
        addTiles() %>%
        setView(lng = 20.48554, lat = 6.57549,  zoom = 3)
    })

    observeEvent(year_reactive(),{
      req(input$year)
      req(nrow(year_reactive())>0)
      ## Define color palette
      polycolor <- goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Goal_col) %>% pull()
      pal <- colorBin(palette = colorRampPalette(c("white", polycolor))(5), domain = year_reactive()$value)
      
      ## Define the labels
      
      if(unique(year_reactive()$`Indicator Name`) %in% grep("%", unique(year_reactive()$`Indicator Name`), value = TRUE, ignore.case = TRUE)){
        labels <- sprintf(
          "<strong>%s</strong><br/><strong>%s</strong>%s<br/><strong>%s</strong>%s",
          year_reactive()$ADM0_NAME,"Year: ",year_reactive()$Year,"Value: ",
          paste0(round(year_reactive()$value, 1), "%")) %>%
          lapply(htmltools::HTML)
      }else{
        labels <- sprintf(
          "<strong>%s</strong><br/><strong>%s</strong>%s<br/><strong>%s</strong>%g",
          year_reactive()$ADM0_NAME,"Year: ",year_reactive()$Year,"Value: ",
          year_reactive()$value) %>%
          lapply(htmltools::HTML)
      }
      
      ## Generate the dynamic part of the map
      leafletProxy('pl') %>%
        addPolygons(data = year_reactive(), color = polycolor, weight = 1, dashArray = "3", fillColor = ~pal(year_reactive()$value),
                    highlight = highlightOptions(
                      weight = 4,
                      color = polycolor,
                      dashArray = "",
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) #%>%
        #addLegend(position = "bottomright",pal = pal, values = ~year_reactive()$value, title = "")
      
      observeEvent(year_reactive(),{
      proxy <- leafletProxy("pl", data = indicator_reactive())
      
      # Remove any existing legend, and only if the legend is
      # enabled, create a new one.
      proxy %>% clearControls()
      # pal <- colorBin(palette = "YlOrRd", domain = indicator_reactive()$value)
      proxy %>% addLegend(position = "bottomright",pal = pal, values = ~value, title = ""
      )
    })
    })


# Country dropdown

    
output$country_dropdown <- renderUI({
  # req(input$sdg)
  countries <- indicator_reactive() %>% distinct(`Country Name`) %>% pull
  dropdown(
    tags$h5(""),
    pickerInput(inputId = 'cn1',
                label = 'Select Country',
                choices = countries,
                selected = countries[1],
                options = pickerOptions(liveSearch	= TRUE,
                                        `style` = "btn-primary"),
                width="260px"),
    
    pickerInput(inputId = 'cn2',
                label = 'Add Country',
                choices = countries,
                selected = "",
                multiple = TRUE,
                width="260px",
                options = pickerOptions(`style` = "btn-primary",
                                        liveSearch	= TRUE,
                                        actionsBox = TRUE,
                                        maxOptions = 5, 
                                        maxOptionsText = "You can only select up to 5 countries", 
                                        noneSelectedText = "Select comparison countries")),
    
    style = "unite", icon = icon("plus"),
    status = "danger", width = "300px",
    tooltip = tooltipOptions(title = "Click to select country !"),
    animate = animateOptions(
      enter = animations$fading_entrances$fadeInLeftBig,
      exit = animations$fading_exits$fadeOutRightBig
    ))
})
# Line graph
output$pl2 <- renderPlotly({

  req(input$sdg)
  req(input$indicator!= "")
  
  sdg_col <- goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Goal_col) %>% pull()
  sdg_palette <- trimws(unlist(str_split(goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Palette) %>% pull(), 
                           pattern = ",")))

  if(input$cn1 %in% input$cn2 |"" %in% input$cn2){
    line_function(input$sdg, input$topic, input$indicator,input$cn1, sdg_col, input$indicator) 
  }else{
    line_function2(input$sdg, input$topic, input$indicator,input$cn1, input$cn2,sdg_col, sdg_palette, input$indicator)
  }
  
  
})


})

shinyApp(ui, server)