library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(kableExtra)
library(shinycssloaders)
library(shinyalert)
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
                                 # column(width = 7, 
                                 #        htmlOutput("narrative")),
  
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
                                                      animate = animateOptions(
                                                        enter = animations$fading_entrances$fadeInLeftBig,
                                                        exit = animations$fading_exits$fadeOutRightBig
                                                      )))),
                                           br(), br(),br(),
                                           fluidRow(
                                             column(width = 6, 
                                                    fluidRow(
                                                      setSliderColor("#d5184e", 1),
                                                      sliderTextInput(
                                                        inputId = "year_slider",
                                                        label = "Select Year", 
                                                        grid = TRUE,
                                                        force_edges = TRUE,
                                                        choices = years,
                                                        width = "190%"
                                                      )),
                                                    fluidRow(
                                                      withSpinner(leafletOutput('pl'), color = "#d5184e")
                                                    )),
                                           column(width = 6, 
                                                  fluidRow(
                                                    column(width = 3, offset = 0.5,selectInput("cn1", "Select Country", list_countries, selected = "Kenya")),
                                                    column(width = 3,offset = 4, selectInput("cn2", "Add Country", list_countries))
                                                  ),
                                                  fluidRow(
                                                  withSpinner(plotOutput("pl2"), color = "#d5184e"))))))
                                 
                                       )
                               ))))
                    


server <- function(input, output, session)({

  ## Add an image for each of the indicator
  output$image <- renderImage({

    filename = paste0("www/sdg_banners/",input$sdg,".png")
    list(src = filename, width = 800)
  }, deleteFile = FALSE)


  output$pl <- renderLeaflet({
      leaflet(merged_mapping_df) %>% 
      addTiles() %>% 
      setView(lng = 20.48554, lat = 6.57549,  zoom = 3)
      
    })
    # observers
    # selected country
    selectedyear <- reactive({
      merged_mapping_df %>% 
        filter(Year == input$year_slider)
    })
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
                      direction = "auto")) #%>%
        #addLegend(position = c("bottomright"),pal = pal, values = ~selectedyear()$value, title = "")
      # Use a separate observer to recreate the legend as needed.
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


## Specific targets for each goal
output$targets <- renderDataTable({
  targs <- goal_target_cols %>% 
            filter(Goal_Name == input$sdg) %>% 
            distinct(Target2)
  
  datatable(targs, 
            extensions = c('FixedHeader','Scroller'),
            rownames = FALSE, colnames = "",
            options = list(dom = 't', fixedHeader = TRUE))
})


## List of goals
sdg_reactive <- reactive({
  merged_df %>% 
    filter(Goal == input$sdg)
})

## The topics shown should be for each goal
observeEvent(sdg_reactive(), {
  choices <- sdg_reactive() %>% distinct(Topic) %>% pull()
  updatePickerInput(session, "topic", choices = choices)
})

## The indicators shown should be for each topic
topic_reactive <- reactive({
  merged_df %>% 
    filter(Goal == input$sdg & Topic == input$topic)
})

observeEvent(topic_reactive(), {
  choices <- topic_reactive() %>% distinct(`Indicator Name`) %>% pull()
  updatePickerInput(session, "indicator", choices = choices)
})

## Line graphs
output$pl2 <- renderPlot({
  line_function(input$sdg, input$topic, input$indicator,input$cn1, input$indicator)
  
})

## Pop up messages for error
observeEvent(input$error, {
  sendSweetAlert(
    session = session,
    title = "Error...",
    text = "Oups !",
    type = "error"
  )
})

})

shinyApp(ui, server)