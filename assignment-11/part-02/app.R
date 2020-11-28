#PolyFit
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(datasets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Fitting a polynomial",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h3("Fitting a cubic polynomial"),
      sliderInput("p1","Specify the value p(1)", min = 0, max = 9, value = 2),
      sliderInput("p2","Specify the value p(2)", min = 0, max = 9, value = 4),
      sliderInput("p5","Specify the value p(5)", min = 0, max = 9, value = 6),
      sliderInput("p7","Specify the value p(7)", min = 0, max = 9, value = 8),
      actionBttn("btnHubb", "Example invented by John Hubbard"),
      actionBttn("btnCOVID", "Number of Australians between years 1972 and 1982 in millions")
    ),
    column(width = 6,
      plotOutput("graph"),
      h3(uiOutput("formula")),
      h3(uiOutput("notes"))
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
source("fitcalc.R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables

CER <- get("austres")
CER

server <- function(session, input, output) {
  y <- reactive({c(input$p1, input$p2, input$p5, input$p7)})
  x <- c(1,2,5,7)
  coeff <- numeric(4)
  output$graph <- renderPlot({
    coeff <- fit.makePoly(x,y())
    curve(fit.eval(coeff,x), from = 0, to = 10)
    points(x,fit.eval(coeff,x),col = "red", pch = 19)
  })
  output$formula <- renderUI({
    coeff <- fit.makePoly(x,c(input$p1, input$p2, input$p5, input$p7))
    HTML(convertPoly(round(coeff,2)))
  })
  observeEvent(input$btnHubb, {
    output$formula <- renderUI(jax.fracD("1","x^2+0.1"))
    f <- Vectorize(function(x) 1/(x^2+0.1), "x")
    t <- (-10:10)/10
    y <- f(t)
    coeff <- fit.makePoly(t,y)
    output$graph <- renderPlot({
      curve(fit.eval(coeff,x), from = -1, to = 1)
      points(t,y, pch = 21, col = "red")
    })
  })
  observeEvent(input$btnCOVID,{
    y <- c(13.2542, 13.4592, 13.6695, 13.8626, 14.0047, 14.1556, 14.3303,14.4784, 14.6464, 14.8744)
    x <- 1:10
    output$formula <- NULL
    coeff <- fit.makePoly(x,y)
    output$graph <- renderPlot({
      curve(fit.eval(coeff,x), from = 1, to = 10)
      points(x,y, pch = 21, col = "red")
    })
    output$notes <- renderUI("Where one stands for 1972 and 10 stands for 1982")
  })
 
  
  #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)