#PermutePartial
#Change the name to Permutation when you finish the homework
library(shiny)
library(shinydashboard)
library(shinyWidgets)

header <- dashboardHeader(title = "Permutation")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
    column(
      width = 4,
      box(
          width = NULL, height = 220,
          h3("Input"),
          textInput("atext","a","(12)"),
          textInput("btext","b","(13)")
      ),
      box(
          width = NULL, height = 150,
          h3("Products"),
          h4(uiOutput("prodab")),
          h4(uiOutput("prodba"))
      ),
      box(
        width = NULL, height = 150,
        h3("Inverses"),
        h4(uiOutput("prodab")),
        h4(uiOutput("prodba"))
      ),
      box(
        width = NULL, height = 150,
        h3("Conjugates"),
        h4(uiOutput("prodab")),
        h4(uiOutput("prodba"))
      ),
    ),
    column(
        width = 4,
        box(
            width = NULL, height = 350,
            h3("Powers of a"),
            uiOutput("powersa"),
            tags$head(tags$style("#powersa{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        box(
          width = NULL, height = 350,
          h3("Powers of ab"),
          uiOutput("powersa"),
          tags$head(tags$style("#powersa{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        actionBttn("btncalc","Calculate",
                   color = "primary", size = "lg")
    ),
    column(
        width = 4,
        box(
          width = NULL, height = 350,
          h3("Powers of b"),
          uiOutput("powersb"),
          tags$head(tags$style("#powersa{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        box(
          width = NULL, height = 350,
          h3("Powers of ba"),
          uiOutput("powersa"),
          tags$head(tags$style("#powersa{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
    )
  )    
)

ui <- dashboardPage(header, sidebar, body)


source("permutecalc.R")    


server <- function(input, output) {
  output$prodab <- renderUI("ab = (132)")
  output$prodba <- renderUI("ba = (123)")
  output$powersa <- renderUI(HTML(paste("(12)","I",sep = "<br/>")))
  observeEvent(input$btncalc, {
    ab <- Perm.multiply(input$atext,input$btext)
    output$prodab <- renderUI(paste("ab =  ",ab))
    ba <- Perm.multiply(input$btext,input$atext)
    output$prodba <- renderUI(paste("ba =  ",ba))
    output$powersa <- renderUI(HTML(Perm.powerString(input$atext)))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
