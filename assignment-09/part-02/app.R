library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))


header <- dashboardHeader(title = HTML("From Matrix to Permutation - GL(2, Z<sub>3</sub>)"),
                          titleWidth = 600)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 3,
           title = "Create a Matrix",
           radioButtons("det", "Choose the determinant",
                        choiceNames = c("Determinant  1","Determinant  -1"),
                        choiceValues = c(1,-1)
           ),
           radioButtons("trace", "Choose the trace",
                        choiceNames = c("Trace 0","Trace 1","Trace -1"),
                        choiceValues = c(0,1,-1)
           ),
           actionButton("generate","Create the Matrix"),
           uiOutput("matrixA"),
           actionButton("applysub","Apply to Subspaces"),
           actionButton("permute","Construct the Permutation"),
           actionButton("powers","Calculate Powers")
    ),
    column(
      width = 3,
      h3("The Four Subspaces"),
      h4("Subspace 1"),
      h5(jax.vecList(rbind(c(0,1,-1),c(0,0,0)))),
      h4("Subspace 2"),
      h5(jax.vecList(rbind(c(0,0,0),c(0,1,-1)))),
      h4("Subspace 3"),
      h5(jax.vecList(rbind(c(0,1,-1),c(0,1,-1)))),
      h4("Subspace 4"),
      h5(jax.vecList(rbind(c(0,1,-1),c(0,-1,1))))
    ),
    column(
      width = 3,
      h3("Action on Subspaces"),
      h4("Action on Subspace 1"),
      uiOutput("sub1"),
      h4("Action on Subspace 2"),
      uiOutput("sub2"),
      h4("Action on Subspace 3"),
      uiOutput("sub3"),
      h4("Action on Subspace 4"),
      uiOutput("sub4"),
      h3("Cycle Representation"),
      uiOutput("perm")
    ),
    column(
      width = 3,
      h3("The Powers of A"),
      uiOutput("power1"),
      uiOutput("power2"),
      uiOutput("power3"),
      uiOutput("power4"),
      uiOutput("power5"),
      uiOutput("power6"),
      uiOutput("power7"),
      uiOutput("power8"),
      uiOutput("power9"),
      uiOutput("power10"),
      uiOutput("power11"),
      uiOutput("power12"),
      uiOutput("power16"),
      uiOutput("power20"),
      uiOutput("power24")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

source("Z3calc.R")
source("permutecalc.R")

server <- function(session, input, output) {

  A <- matrix(nrow = 2, ncol = 2)
  
  observeEvent(input$generate,{
    A <<- Z3CreateMatrix(as.numeric(input$det),as.numeric(input$trace))
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})
    output$sub1 <- renderUI("")
    output$sub2 <- renderUI("")
    output$sub3 <- renderUI("")
    output$sub4 <- renderUI("")
    output$power1 <- renderUI("")
    output$power2 <- renderUI("")
    output$power3 <- renderUI("")
    output$power4 <- renderUI("")
    output$power5 <- renderUI("")
    output$power6 <- renderUI("")
    output$power7 <- renderUI("")
    output$power8 <- renderUI("")
    output$power9 <- renderUI("")
    output$power10 <- renderUI("")
    output$power11 <- renderUI("")
    output$power12 <- renderUI("")
    output$power16 <- renderUI("")
    output$power20 <- renderUI("")
    output$power24 <- renderUI("")
    output$perm <- renderUI("")
  })
  
  observeEvent(input$applysub,{
    v1 <- c(1,0)
    x <- ActOnVector(A, v1)
    output$sub1 <- renderUI({jax.mTimesV("A",v1,x)})
    v2 <- c(0,1)
    x2 <- ActOnVector(A, v2)
    output$sub2 <- renderUI({jax.mTimesV("A",v2,x2)})
    v3 <- c(1,1)
    x3 <- ActOnVector(A, v3)
    output$sub3 <- renderUI({jax.mTimesV("A",v3,x3)})
    v4 <- c(1,-1)
    x4 <- ActOnVector(A, v4)
    output$sub4 <- renderUI({jax.mTimesV("A",v4,x4)})
  })
  
  observeEvent(input$permute,{
    fval <- sapply(1:4,Transform,A=A)
    output$perm <-renderUI(h3(Perm.cycle.convert(fval)))
  })
  
  observeEvent(input$powers,{
    output$power1 <- renderUI({jax.matrix(A, name = "A")})
    B <- Z3MatProd(A,A)
    output$power2 <- renderUI({jax.matrix(B, name = "A^2")})
    B2 <- Z3MatProd(A,B)
    output$power3 <- renderUI({jax.matrix(B2, name = "A^3")})
    B3 <- Z3MatProd(A,B2)
    output$power4 <- renderUI({jax.matrix(B3, name = "A^4")})
    B4 <- Z3MatProd(A,B3)
    output$power5 <- renderUI({jax.matrix(B4, name = "A^5")})
    B5 <- Z3MatProd(A,B4)
    output$power6 <- renderUI({jax.matrix(B5, name = "A^6")})
    B6 <- Z3MatProd(A,B5)
    output$power7 <- renderUI({jax.matrix(B6, name = "A^7")})
    B7 <- Z3MatProd(A,B6)
    output$power8 <- renderUI({jax.matrix(B7, name = "A^8")})
    B8 <- Z3MatProd(A,B7)
    output$power9 <- renderUI({jax.matrix(B8, name = "A^9")})
    
    B10 <- Z3MatProd(B4,B4)
    output$power10 <- renderUI({jax.matrix(B10, name = "A^{10}")})
    B11 <- Z3MatProd(A,B10)
    output$power11 <- renderUI({jax.matrix(B11, name = "A^{11}")})
    B12 <- Z3MatProd(B5,B5)
    output$power12 <- renderUI({jax.matrix(B12, name = "A^{12}")})
    B16 <- Z3MatProd(B7,B7)
    output$power16 <- renderUI({jax.matrix(B16, name = "A^{16}")})
    B20 <- Z3MatProd(B10,B10)
    output$power20 <- renderUI({jax.matrix(B20, name = "A^{20}")})
    B24 <- Z3MatProd(B12,B12)
    output$power24 <- renderUI({jax.matrix(B24, name = "A^{24}")})
  })
}

shinyApp(ui = ui, server = server)