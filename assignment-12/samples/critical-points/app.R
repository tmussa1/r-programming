#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    info {
      font-size: 150%;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Find the Critical Points",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h3("Available functions:"),
      h4(uiOutput("f1")),
      h4(uiOutput("f2")),
      h4(uiOutput("f3")),
      radioButtons("select","Select a function",c("f1","f2","f3")),
      h3(uiOutput("info"))
    ),
    column(width = 4,
      plotOutput("plot1", click ="plot_click", brush = "plot_brush", height = 600),
      plotOutput("zoom", click ="plot_click", brush = "zoom_brush", height = 600)
    ),
    column(width = 4,
           plotOutput("zoom2", click ="plot_click", brush = "zoom2_brush", height = 600),
           plotOutput("zoom3", click ="plot_click", height = 600)
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

f1 <- function(x,y){
  return (-x^3+y^3+x*y+4*x - 5*y)  #Hubbard exercise 3.6.4
}
f2 <- function(x,y){
  return (x^3-8*x^2+y^2-x*y+20*x-3*y)  
}
f3 <- function(x,y){
  return (x^3+3*x^2+y^3-9*x-12*y)  
}


server <- function(session, input, output) {
  N.out <- 50
  n.levels <- 20
  xmin <- 0
  xmax <- 0
  ymin <- 0
  ymax <- 0
  xZoom <- seq(0,1)
  yZoom <- seq(0,1)
  zZoom <- numeric(0)
  x2min <- 0
  x2max <- 0
  y2min <- 0
  y2max <- 0
  x2Zoom <- seq(0,1)
  y2Zoom <- seq(0,1)
  z2Zoom <- numeric(0)
  x3min <- 0
  x3max <- 0
  y3min <- 0
  y3max <- 0
  x3Zoom <- seq(0,1)
  y3Zoom <- seq(0,1)
  z3Zoom <- numeric(0)
  output$f1 <- renderUI(jax.fTimesP("f_1",c("x","y"),"-x^3+y^3+xy+4x-5y"))
  output$f2 <- renderUI(jax.fTimesP("f_2",c("x","y"),"x^3+8x^2+y^2-xy+20x-3y"))
  output$f3 <- renderUI(jax.fTimesP("f_3",c("x","y"),"x^3+3x^2+y^3-9x-12y"))
  fselect <- "f1"
  f <- function(x,y) {
    return (switch(fselect,
      f1 = f1(x,y),
      f2 = f2(x,y),
      f3 = f3(x,y)
    ))
  }
  x <- seq(from = -5, to = 5, length.out =  N.out)
  z <- outer(x,x,"f")
  output$plot1<- renderPlot(contour(x,x,z,nlevels = n.levels))
  observeEvent(input$select,{
    fselect <<- input$select
    xm <- switch(fselect,
        f1 = -5,
        f2 = 0,
        f3 = -5)
    x <<- seq(from = xm, to = 5, length.out =  N.out)
    z <<- outer(x,x,"f")
    output$plot1<- renderPlot(contour(x,x,z,nlevels = n.levels, labcex = 1))
    output$zoom <- NULL
    output$zoom2 <- NULL
    output$zoom3 <- NULL
  })
  observeEvent(input$plot_brush,{
    xmin <<- input$plot_brush$xmin
    xmax <<- input$plot_brush$xmax
    ymin <<- input$plot_brush$ymin
    ymax <<- input$plot_brush$ymax
    xZoom <<- seq(from = xmin, to = xmax, length.out =  N.out)
    yZoom <<- seq(from = ymin, to = ymax, length.out =  N.out)
    zZoom <- outer(xZoom,yZoom,"f")
    output$zoom<- renderPlot(contour(xZoom,yZoom,zZoom,nlevels = n.levels, labcex = 1))
  })
  observeEvent(input$zoom_brush,{
    x2min <<- input$zoom_brush$xmin
    x2max <<- input$zoom_brush$xmax
    y2min <<- input$zoom_brush$ymin
    y2max <<- input$zoom_brush$ymax
    x2Zoom <<- seq(from = x2min, to = x2max, length.out =  N.out)
    y2Zoom <<- seq(from = y2min, to = y2max, length.out =  N.out)
    z2Zoom <- outer(x2Zoom,y2Zoom,"f")
    output$zoom2<- renderPlot(contour(x2Zoom,y2Zoom,z2Zoom,nlevels = n.levels, labcex = 1))
  })
  observeEvent(input$zoom2_brush,{
    x3min <<- input$zoom2_brush$xmin
    x3max <<- input$zoom2_brush$xmax
    y3min <<- input$zoom2_brush$ymin
    y3max <<- input$zoom2_brush$ymax
    x3Zoom <<- seq(from = x3min, to = x3max, length.out =  N.out)
    y3Zoom <<- seq(from = y3min, to = y3max, length.out =  N.out)
    z3Zoom <- outer(x3Zoom,y3Zoom,"f")
    output$zoom3<- renderPlot(contour(x3Zoom,y3Zoom,z3Zoom,nlevels = n.levels, labcex = 1))
  })
  observeEvent(input$plot_click,{
    output$info <- renderUI(sprintf("x= %8.6f \n y= %8.6f z= %8.6f",
      input$plot_click$x,input$plot_click$y,f(input$plot_click$x,input$plot_click$y)))
  })
}

#Run the app
shinyApp(ui = ui, server = server)