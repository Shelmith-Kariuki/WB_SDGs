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

ui <- fluidPage(title = "WDI: Sustainable Development Goals",
                useShinyalert(), 
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
                                             column(width = 5, 
                                                    withSpinner(leafletOutput("pl"), color = "#d5184e")),
                                           column(width = 7, 
                                                  withSpinner(plotOutput("pl2"), color = "#d5184e")))))
                                 
                                       )
                               ))))
                    


server <- function(input, output, session)({

  ## Add an image for each of the indicator
  output$image <- renderImage({

    filename = paste0("www/sdg_banners/",input$sdg,".png")
    list(src = filename, width = 800)
  }, deleteFile = FALSE)

  ## Definition and importance of each sdg
  # output$definition <- renderText({
  #   def <- sdg_file_codebook %>%
  #     filter(Goal_Name == input$sdg) %>%
  #     distinct(Narrative) %>%
  #     pull
  #   def
  # })

  ## narrative
  # output$narrative <- renderText({
  # 
  #   def <- sdg_file_codebook %>%
  #     filter(Goal_Name == input$sdg) %>%
  #     distinct(Narrative) %>%
  #     pull
  # 
  #   
  #   link <- sdg_file_codebook %>%
  #     filter(Goal_Name == input$sdg) %>%
  #     distinct(Links) %>%
  #     pull()
  #   HTML(paste("<p style = 'color: black; font-size: 14px;'>", def, "To learn more about this goal,
  #                     <a href=", paste0("'",link ,"'"), ">click here.</a></p>"))
  # })
  
  # output$pl <- renderLeaflet({
  #   lm
  # })


  # output$indicator_list <- renderDataTable({
  #   df <- goal_target_cols %>% 
  #           filter(Goal_Name == input$sdg) %>% 
  #           arrange(Goal_Name, Target2) %>%
  #           select(Target2, Indicator_Name)  
  # 
  #  datatable(df, 
  #            extensions = c('FixedHeader','Scroller'),
  #            options = list(dom = 't', fixedHeader = TRUE))
  # })
 
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
  line_function(input$sdg, input$topic, input$indicator, input$indicator)
  
})

## Pop up messages for error
observeEvent(input$pl2, {
  # Show a modal when the button is pressed
  shinyalert("Oops!", "Data doesn't exist!", type = "error")
})

})

shinyApp(ui, server)