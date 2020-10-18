library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("primeCalc.R")
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

header <- dashboardHeader(title = "Multiplication Table in Zn",
                          titleWidth = 500)
sidebar <- dashboardSidebar(
  fluidRow(
    stylesheet,
    column(width=12,
           shinyWidgets::sliderTextInput(inputId = "sliderpicker", 
                                         label = "You can only pick a composite number", 
                                         choices = prime.findCompositeNumbersUpto53())
    )
  )
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=12,
           box(width = NULL,
               height = 900,
               h3("Multiplication Table"),
               tableOutput("multable")
           )
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


server <- function(session, input, output) {
  
  observeEvent(input$sliderpicker,{
    
    tbl <- outer(as.integer(prime.findCoPrimes(input$sliderpicker)), 
                 as.integer(prime.findCoPrimes(input$sliderpicker))) %% input$sliderpicker
    
    colnames(tbl) <- as.integer(prime.findCoPrimes(input$sliderpicker))
    rownames(tbl) <- as.integer(prime.findCoPrimes(input$sliderpicker))
    
    output$multable <- renderTable(tbl,rownames = TRUE)
  })

}

shinyApp(ui = ui, server = server)