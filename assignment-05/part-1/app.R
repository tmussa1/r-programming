#Starter file for any Shiny dashboard app
#This should replace the default app.r that displays Old Faithful data
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
#The user interface
header <- dashboardHeader(title = "D4 According to Biggs")
sidebar <- dashboardSidebar(
  width = 100,
  actionButton("btninit", "Initialize"),
  actionButton("btni","Apply i"),
  actionButton("btnr","Apply r"),
  actionButton("btnt","Apply t"),
  actionButton("btns","Apply s"),
  actionButton("btnx","Apply x"),
  actionButton("btny","Apply y"),
  actionButton("btnz","Apply z"),
  actionButton("btnw","Apply w")
)
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height =200)
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("triangle", height = 300)
    ),
    column(
      width = 6,
      dataTableOutput("multable")
#      tableOutput("multable")
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("d3calc.R")



#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  D3DF <- D3.makeDataFrame()
  config <- "ABCD"
    #Initialization
  output$configs <- renderPlot(D3.showConfigs(D3DF))
  output$triangle <- renderPlot(D3.showTriangle(config))
  tbl <-outer(D3DF$name,D3DF$name,vD3.multiply,DF=D3DF)
  colnames(tbl) <- D3DF$name
  rownames(tbl) <- D3DF$name 
  #Use options to suppress the fancy controls
  output$multable <- renderDataTable(tbl, options = list(dom = "t"))
#  output$multable <- renderTable(tbl, rownames = TRUE)
    #Functions that respond to events in the input
  observeEvent(input$btninit,{
    config <<- "ABCD"
    output$triangle <- renderPlot(D3.showTriangle(config))
  })

  observeEvent(input$btni,{
      config <<- D3.apply("i",config)
      output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btnr,{
      config <<- D3.apply("r",config)
      output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btnt,{
    config <<- D3.apply("t",config)
    output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btns,{
      config <<- D3.apply("s",config)
      output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btnx,{
      config <<- D3.apply("x",config)
      output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btny,{
      config <<- D3.apply("y",config)
      output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btnz,{
      config <<- D3.apply("z",config)
      output$triangle <- renderPlot(D3.showTriangle(config))
  })
  observeEvent(input$btnw,{
    config <<- D3.apply("w",config)
    output$triangle <- renderPlot(D3.showTriangle(config))
  })
}

#Run the app
shinyApp(ui = ui, server = server)