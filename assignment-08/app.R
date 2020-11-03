library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")
stylesheet <- tags$head(tags$style(HTML('
    #btncalculate {
       color: blue;
    }
    actionButton{
      color : red;
    }
  '),
))

header <- dashboardHeader(title = "Conformal Matrices",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=6,
      uiOutput("btnitem1"),
      actionButton("item1clicka", "click to pick this matrix for A"),
      actionButton("item1clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem2"),
      actionButton("item2clicka", "click to pick this matrix for A"),
      actionButton("item2clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem3"),
      actionButton("item3clicka", "click to pick this matrix for A"),
      actionButton("item3clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem4"),
      actionButton("item4clicka", "click to pick this matrix for A"),
      actionButton("item4clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem5"),
      actionButton("item5clicka", "click to pick this matrix for A"),
      actionButton("item5clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem6"),
      actionButton("item6clicka", "click to pick this matrix for A"),
      actionButton("item6clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem7"),
      actionButton("item7clicka", "click to pick this matrix for A"),
      actionButton("item7clickb", "click to pick this matrix for B"),
      h3("=================================================="),
      uiOutput("btnitem8"),
      actionButton("item8clicka", "click to pick this matrix for A"),
      actionButton("item8clickb", "click to pick this matrix for B"),
    ),
    column(width=6,
        h3("A = "),
        uiOutput("choicea"),
        h3("=================================================="),
        h3("B = "),
        uiOutput("choiceb"),
        h3("=================================================="),
        actionButton("btncalculate", "Click to Calculate"),
        h3("=================================================="),
        h3("A * B = "),
        uiOutput("atimesb"),
        h3("=================================================="),
        h3("A + B = "),
        uiOutput("aplusb"),
        h3("=================================================="),
        h3("A - B = "),
        uiOutput("aminusb"),
        h3("=================================================="),
        h3("A / B = "),
        uiOutput("adividedb"),
        h3("=================================================="),
    )
  )
)

findConformalMatrices <- function(){
  
  iden <- matrix(c(1,0, 0, 1), nrow=2, ncol = 2)
  generator <- matrix(c(1,1, 2, 1), nrow=2, ncol = 2)
  result <<- c()
  prev <- generator
  
  for(val in (1: 10)){
    
    result <<- cbind(result , prev)
    prev <- (generator %*% prev) %% 3
  }
  
  return (result)
}

ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


server <- function(session, input, output) {
  
  item1 <- matrix(findConformalMatrices()[, 1 : 2], nrow = 2, ncol = 2)
  item2 <- matrix(findConformalMatrices()[, 3 : 4], nrow = 2, ncol = 2)
  item3 <- matrix(findConformalMatrices()[, 5 : 6], nrow = 2, ncol = 2)
  item4 <- matrix(findConformalMatrices()[, 7 : 8], nrow = 2, ncol = 2)
  item5 <- matrix(findConformalMatrices()[, 9 : 10], nrow = 2, ncol = 2)
  item6 <- matrix(findConformalMatrices()[, 11 : 12], nrow = 2, ncol = 2)
  item7 <- matrix(findConformalMatrices()[, 13 : 14], nrow = 2, ncol = 2)
  item8 <- matrix(findConformalMatrices()[, 15 : 16], nrow = 2, ncol = 2)
  
  A <- 0
  B <- 0
  
  output$btnitem1 <- renderUI(jax.matrix(item1))
  output$btnitem2 <- renderUI(jax.matrix(item2))
  output$btnitem3 <- renderUI(jax.matrix(item3))
  output$btnitem4 <- renderUI(jax.matrix(item4))
  output$btnitem5 <- renderUI(jax.matrix(item5))
  output$btnitem6 <- renderUI(jax.matrix(item6))
  output$btnitem7 <- renderUI(jax.matrix(item7))
  output$btnitem8 <- renderUI(jax.matrix(item8))
  
  observeEvent(input$item1clicka,{
    A <<- item1
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item2clicka,{
    A <<- item2
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item3clicka,{
    A <<- item3
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item4clicka,{
    A <<- item4
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item5clicka,{
    A <<- item5
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item6clicka,{
    A <<- item6
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item7clicka,{
    A <<- item7
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item8clicka,{
    A <<- item8
    output$choicea <- renderUI(jax.matrix(A))
  })
  
  observeEvent(input$item1clickb,{
    B <<- item1
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item2clickb,{
    B <<- item2
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item3clickb,{
    B <<- item3
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item4clickb,{
    B <<- item4
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item5clickb,{
    B <<- item5
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item6clickb,{
    B <<- item6
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item7clickb,{
    B <<- item7
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$item8clickb,{
    B <<- item8
    output$choiceb <- renderUI(jax.matrix(B))
  })
  
  observeEvent(input$btncalculate,{
    
    
    product <- (A %*% B) %% 3
    output$atimesb <- renderUI(jax.matrix(product))
    
    sum <- (A + B) %% 3
    output$aplusb <- renderUI(jax.matrix(sum))
   
    subt <- (A - B) %% 3
    output$aminusb <- renderUI(jax.matrix(subt))
    
    deterInv <- 3 - (det(B) %% 3)
    inv <- (matrix(c(B[2, 2], -B[2, 1], -B[1, 2], B[1, 1]), nrow =2, ncol =2) * deterInv) %% 3
    
    divide <- (inv %*% A) %% 3
    output$adividedb <- renderUI(jax.matrix(divide))
  })
}


shinyApp(ui = ui, server = server)