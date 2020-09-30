library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")

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
        h4(uiOutput("inva")),
        h4(uiOutput("invb"))
      ),
      box(
        width = NULL, height = 150,
        h3("Conjugates"),
        h4(uiOutput("conjabainv")),
        h4(uiOutput("conjbabinv"))
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
          uiOutput("powersab"),
          tags$head(tags$style("#powersab{color:red; font-size:20px;
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
          tags$head(tags$style("#powersb{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        box(
          width = NULL, height = 350,
          h3("Powers of ba"),
          uiOutput("powersba"),
          tags$head(tags$style("#powersba{color:red; font-size:20px;
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
    output$powersb <- renderUI(HTML(Perm.powerString(input$btext)))
    output$powersab <- renderUI(HTML(Perm.powerString(ab)))
    output$powersba <- renderUI(HTML(Perm.powerString(ba)))
    output$inva <- renderUI(jaxD(
      paste("a^{-1}=", Perm.inverse(input$atext)
      )
    ))
    output$invb <- renderUI(jaxD(
      paste("b^{-1}=", Perm.inverse(input$btext)
      )
    ))
    
    output$conjabainv <-  renderUI(jaxD(
      paste("aba^{-1}=", Perm.conjugate(input$atext,input$btext)
      )
    ))
    
    output$conjbabinv <- renderUI(jaxD(
      paste("bab^{-1}=", Perm.conjugate(input$btext,input$atext)
      )
    ))
  })

}

shinyApp(ui = ui, server = server)
