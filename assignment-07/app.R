library(shiny)
library(shinydashboard)
library(shinyWidgets)
library("tidyverse")
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
  ),
  fluidRow(stylesheet,
           column(width=12,
                  box(width = NULL,
                      height = 500,
                      h3("Order Chart"),
                      plotOutput("orderchart")
                  )
           )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


server <- function(session, input, output) {
  
  observeEvent(input$sliderpicker,{
    
    tbl <- outer(as.integer(prime.findCoPrimes(input$sliderpicker), length = 0), 
                 as.integer(prime.findCoPrimes(input$sliderpicker), length = 0)) %% as.integer(input$sliderpicker, length = 0)
    
    colnames(tbl) <- as.integer(prime.findCoPrimes(input$sliderpicker), length = 0)
    rownames(tbl) <- as.integer(prime.findCoPrimes(input$sliderpicker), length = 0)
    
    output$multable <- renderTable(tbl,rownames = TRUE, digits = 0)
    
    coprimes <- as.integer(prime.findCoPrimes(input$sliderpicker))
    
    coprimesorder <- c()
    
    for(val in coprimes){
      order <- prime.findOrder(val, as.integer(input$sliderpicker))
      coprimesorder <- cbind(coprimesorder, order)
    }
    
    DF <- data.frame(coprimes = c(coprimes), orders =c(coprimesorder))
    
    output$orderchart <- renderPlot(
      ggplot(data = DF,
             aes(x=factor(coprimes), y=orders, fill=orders)) +
        geom_bar(stat="identity", position = "dodge") +
        scale_size_area() + 
        xlab("Coprime Number") +
        ylab("Order"))
  })

}

shinyApp(ui = ui, server = server)