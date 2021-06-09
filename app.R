library(shiny)
library(shinyWidgets)
library(tidyverse)
source("www/global.R")
ui <- fluidPage(title = "WDI: Sustainable Development Goals",
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
                                        selectInput("sdg", span(tags$i("Select a goal"),style="color:black;font-size:18px"),
                                                    choices = unique(sdg_file$Goal_Name))
                                        ),
                                 column(width = 5, 
                                        htmlOutput("narrative")),
                                 
                                 column(width = 2,
                                        dropdown(
                                          tags$h5(""),
                                          pickerInput(inputId = 'target',
                                                      label = 'Select Target',
                                                      choices = c("T1", "T2", "T3"),
                                                      selected = "T1",
                                                      options = list(`style` = "btn-primary"),
                                                      width="380px"),
                                          
                                          pickerInput(inputId = 'indicator',
                                                      label = 'Select Indicator',
                                                      choices = c("Ind1", "Ind2", "Ind3"),
                                                      selected = "Ind1",
                                                      options = list(`style` = "btn-primary"),
                                                      width="380px"),
                                          
                                          style = "unite", icon = icon("gear"),
                                          status = "danger", width = "400px",
                                          animate = animateOptions(
                                            enter = animations$fading_entrances$fadeInLeftBig,
                                            exit = animations$fading_exits$fadeOutRightBig
                                          ))),
                                 column(width = 1, offset = 1,
                                       imageOutput("image")))
                               ))))
                    


server <- function(input, output)({

  ## Add an image for each of the indicator
  output$image <- renderImage({

    filename = paste0("www/",input$sdg,".png")
    list(src = filename, width = 80, height = 80)
  }, deleteFile = FALSE)

  ## Definition and importance of each sdg
  # output$definition <- renderText({
  #   def <- sdg_file %>%
  #     filter(Goal_Name == input$sdg) %>%
  #     distinct(Narrative) %>%
  #     pull
  #   def
  # })

  ## narrative
  output$narrative <- renderText({

    def <- sdg_file %>%
      filter(Goal_Name == input$sdg) %>%
      distinct(Narrative) %>%
      pull
 
    
    link <- sdg_file %>%
      filter(Goal_Name == input$sdg) %>%
      distinct(Links) %>%
      pull()
    HTML(paste("<p style = 'color: black; font-size: 14px;'>", def, "To learn more about this goal,
                      <a href=", paste0("'",link ,"'"), ">click here.</a></p>"))
  })


})

shinyApp(ui, server)