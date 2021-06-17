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
                                  tabPanel(span("Targets",style="color:black; font-style: bold; font-size:16px"), 
                                           dataTableOutput("targets")),
                                  tabPanel(span("Output", style="color:black; font-style: bold; font-size:16px"), 
                                           fluidRow(style = "margin-left: 3px;",
                                             br(),
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
                                                      )))),
                                           br(), br(),br(),
                                           fluidRow(
                                             column(width = 4, 
                                                    fluidRow(
                                                      #uiOutput("slider_color"),
                                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #052D56}")),
                                                      uiOutput("year_slider")),
                                                    fluidRow(
                                                      withSpinner(leafletOutput('pl', height = 500), color = "#d5184e")
                                                    )),
                                           column(width = 8, 
                                                  fluidRow(
                                                    column(width = 5, offset = 8,
                                                           
                                                           dropdown(
                                                             tags$h5(""),
                                                             pickerInput(inputId = 'cn1',
                                                                         label = 'Select Country',
                                                                         choices = list(
                                                                           `Low Income` = low_income,
                                                                           `Lower Middle Income`= lower_middle_income,
                                                                           `Upper Middle Income` = upper_middle_income,
                                                                           `High Income` = high_income
                                                                         ),
                                                                         selected = "Burkina Faso",
                                                                         options = list(`style` = "btn-primary"),
                                                                         width="290px"),
                                                             
                                                             pickerInput(inputId = 'cn2',
                                                                         label = 'Add Country',
                                                                         choices = list("None",
                                                                           `Low Income` = low_income,
                                                                           `Lower Middle Income`= lower_middle_income,
                                                                           `Upper Middle Income` = upper_middle_income,
                                                                           `High Income` = high_income
                                                                         ),
                                                                         selected = "Burkina Faso",
                                                                         multiple = TRUE,
                                                                         width="290px",
                                                                         options = pickerOptions(`style` = "btn-primary",
                                                                                                 maxOptions = 5, 
                                                                                        maxOptionsText = "You can only select up to 5 countries", 
                                                                                        noneSelectedText = "Select comparison countries")),
                                                             
                                                             style = "unite", icon = icon("plus"),
                                                             status = "danger", width = "320px",
                                                             tooltip = tooltipOptions(title = "Click to select country !"),
                                                             animate = animateOptions(
                                                               enter = animations$fading_entrances$fadeInLeftBig,
                                                               exit = animations$fading_exits$fadeOutRightBig
                                                             )))),
                                                  fluidRow(
                                                  withSpinner(plotOutput("pl2", height = 500), color = "#d5184e"))))
                                           ))
                                 
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
      leaflet(merged_mapping_df) %>%
      addTiles() %>%
      setView(lng = 20.48554, lat = 6.57549,  zoom = 3)

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
          proxy %>% addLegend(position = "bottomright",pal = pal, values = ~value, title = "",
          )

      })
  })

# Line graph
output$pl2 <- renderPlot({
  req(input$sdg, input$topic, input$indicator)
  sdg_col <- goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Goal_col) %>% pull()
  sdg_palette <- trimws(unlist(str_split(goal_target_cols %>% filter(Goal_Name == input$sdg) %>% distinct(Palette) %>% pull(), 
                           pattern = ",")))
  if(input$cn1 == input$cn2 | (any("None" %in% input$cn2) & length(unique(input$cn2))==1 )){
    line_function(input$sdg, input$topic, input$indicator,input$cn1, sdg_col, input$indicator) 
  }else{
    line_function2(input$sdg, input$topic, input$indicator,input$cn1, input$cn2, sdg_palette, input$indicator)
  }
  
  
}, height = 560)


})

shinyApp(ui, server)