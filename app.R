source("www/global.R")
source("www/custom_css.R")
source("www/line_graphs.R")
source("www/leaflet_function.R")



ui <- fluidPage(title = "WDI: Sustainable Development Goals",
                useSweetAlert(), 
                tagList(
                  tags$head(tags$script(type="text/javascript", src = "code.js")),
                  navbarPage(
                    title = tags$a(href = "https://datatopics.worldbank.org/sdgs/", 
                                   span(h2(tags$b("WDI: Sustainable Development Goals\n(Africa)")),
                                 style = "color:#d5184e; font-style: bold;")),
                      tabPanel(span(h3(tags$b("Home")),style = "color:#002f54;"),
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
                                          label = span("Click to select a goal!",style="color:black;font-size:16px"), 
                                          choices = goals_list)),
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
                                             column(width = 2, offset = 7,
                                                    uiOutput("country_dropdown"))),
                                           #),
                                           br(),
                                           fluidRow(style = "margin-left: 3px;",
                                             column(width = 5, 
                                                    fluidRow(
                                                      #uiOutput("slider_color"),
                                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #052D56}")),
                                                      uiOutput("year_slider")),
                                                    fluidRow(
                                                      withSpinner(leafletOutput('pl', height = 500), color = "#d5184e")
                                                    )),
                                           column(width = 7,
                                                 withSpinner(plotlyOutput("pl2", height = 600), color = "#d5184e"))))
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
  
  # The topics shown should be for each goal
  observeEvent(sdg_reactive(), {
    choices <- sdg_reactive() %>% distinct(Topic) %>% pull()
    updatePickerInput(session, "topic", choices = choices)
  })
  
  # The indicators shown should be for each topic
  topic_reactive <- reactive({
    merged_df %>% 
      filter(Goal == input$sdg & Topic == input$topic)
  })
  
  observeEvent(topic_reactive(), {
    choices <- topic_reactive() %>% distinct(`Indicator Name`) %>% pull()
    updatePickerInput(session, "indicator", choices = choices)
  })
  
  # Slider color depends on the color theme of the SDG
  # output$slider_color <- renderUI({
  #   sdg_col <- goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Goal_col) %>% pull()
  #   setSliderColor(sdg_col, 1)
  # })

  #Update years slicer
  year_reactive <- reactive({
    df <- merged_df %>% 
      filter(`Indicator Name` == input$indicator)
  })
  
  output$year_slider <- renderUI({
    req(input$indicator)
    sdg_col <- goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Goal_col) %>% pull()
    years <- as.character(sort(year_reactive() %>% distinct(Year) %>% pull()))
    setSliderColor(sdg_col, 1)
    sliderTextInput(
      inputId = "what",
      label = "Select Year", 
      grid = TRUE,
      force_edges = TRUE,
      choices = years, 
      width = "190%"
    ) 
  })

  
  # Leaflet output
  ## Parts of the leaflet map that are static
  output$pl <- renderLeaflet({
      map <- leaflet(merged_mapping_df,options = leafletOptions(zoomControl = FALSE)) %>%
             addTiles() %>%
              setView(lng = 20.48554, lat = 6.57549,  zoom = 3)
      #map$x$options = append(map$x$options, list("zoomControl" = FALSE))

    })

    ## observers: leaflet map should change per year selected
    selectedyear <- reactive({
      df <- merged_mapping_df %>%
        filter(Year == 2004)
    })


    ## Parts of the leaflet map that are dynamic (depends on the year selected)
    observe({
      pal <- colorBin(palette = "YlOrRd", domain = selectedyear()$value)

      labels <- sprintf(
        "<strong>%s</strong><br/><strong>%s</strong>%g<br/><strong>%s</strong>%g",
        selectedyear()$ADM0_NAME,"Year: ",selectedyear()$Year,"Value: ",
        selectedyear()$value) %>%
        lapply(htmltools::HTML)

      leafletProxy('pl') %>%
        addPolygons(data = selectedyear() , color = "#397E4A", weight = 1, dashArray = "3", fillColor = ~pal(selectedyear()$value),
                    highlight = highlightOptions(
                      weight = 4,
                      color = "#397E4A",
                      dashArray = "",
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))

      ## Use a separate observer to recreate the legend as needed.
      observe({
        proxy <- leafletProxy("pl", data = merged_mapping_df)

        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
          proxy %>% clearControls()
          pal <- colorBin(palette = "YlOrRd", domain = merged_mapping_df$value)
          proxy %>% addLegend(position = "bottomright",pal = pal, values = ~value, title = ""
          )

      })
  })

# Country dropdown

# Indicator reactive
    indicator_reactive <- reactive({
      merged_df %>% 
        filter(Goal == input$sdg & Topic == input$topic & `Indicator Name` == input$indicator)
    })
    
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
                width="290px"),
    
    pickerInput(inputId = 'cn2',
                label = 'Add Country',
                choices = c("None", countries),
                selected = "None",
                multiple = TRUE,
                width="290px",
                options = pickerOptions(`style` = "btn-primary",
                                        liveSearch	= TRUE,
                                        actionsBox = TRUE,
                                        maxOptions = 5, 
                                        maxOptionsText = "You can only select up to 5 countries", 
                                        noneSelectedText = "Select comparison countries")),
    
    style = "unite", icon = icon("plus"),
    status = "danger", width = "320px",
    tooltip = tooltipOptions(title = "Click to select country !"),
    animate = animateOptions(
      enter = animations$fading_entrances$fadeInLeftBig,
      exit = animations$fading_exits$fadeOutRightBig
    ))
})
# Line graph
output$pl2 <- renderPlotly({

  sdg_col <- goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Goal_col) %>% pull()
  sdg_palette <- trimws(unlist(str_split(goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Palette) %>% pull(), 
                           pattern = ",")))

  if(input$cn1 %in% input$cn2 |"None" %in% input$cn2){
    # req(input$sdg, input$topic)
    line_function(input$sdg, input$topic, input$indicator,input$cn1, sdg_col, input$indicator) 
  }else{
    # req(input$sdg, input$topic)
    line_function2(input$sdg, input$topic, input$indicator,input$cn1, input$cn2,sdg_col, sdg_palette, input$indicator)
  }
  
  
})


})

shinyApp(ui, server)