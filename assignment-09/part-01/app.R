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

header <- dashboardHeader(title = "IsoF4",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=2,
           actionButton("generatea","Generate Matrix A"),
           uiOutput("matrixA"),
           actionButton("pamult","P(A)"),
           uiOutput("perma")
           
    ),
    column(width=2,
           actionButton("generateb","Generate Matrix B"),
           uiOutput("matrixB"),
           actionButton("pbmult","P(B)"),
           uiOutput("permb")
    ),
    column(width=2,
           actionButton("generateab", "The Product AB. You need to select A and B first"),
           uiOutput("matrixAB"),
           actionButton("pabmult", "P(AB)"),
           uiOutput("permab"),
           actionButton("pabmulttimes","P(A) * P(B)"),
           uiOutput("permabtimes"),
    ),
    column(width=2,),
    column(width=3,
           actionButton("reset", "Reset"),
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available
source("F4calc.R")
source("permutecalc.R")

server <- function(session, input, output) {
  
  A <- matrix(c("0","x","x+1","1"))
  B <- matrix(c("0","x","x+1","1"))
  AB <- matrix(c("0","x","x+1","1"))
  fvala <- c()
  fvalb <- c()
  fvalab <- c()
  fvalabtimes <- c()
  
  observeEvent(input$generatea, {
    rand <<- floor(runif(1, min=1, max=4))
    
    if(rand == 1){
      A <<- F4CreateMatrix("0")
    } else if(rand == 2){
      A <<- F4CreateMatrix("x")
    } else if(rand == 3){
      A <<- F4CreateMatrix("x+1")
    } else {
      A <<- F4CreateMatrix("1")
    }
    
    output$matrixB <- renderUI("")
    output$matrixAB <- renderUI("")
    output$perma <- renderUI("")
    output$permb <- renderUI("")
    output$permab <- renderUI("")
    output$permabtimes <- renderUI("")
    
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})
    
    fvala <<- sapply(1:5,Transform,A=A)
    output$perma <-renderUI(h3(cycle.convert(fvala)))
  })
  
  observeEvent(input$generateb, {
    rand <<- floor(runif(1, min=1, max=4))
    
    if(rand == 1){
      B <<- F4CreateMatrix("0")
    } else if(rand == 2){
      B <<- F4CreateMatrix("x")
    } else if(rand == 3){
      B <<- F4CreateMatrix("x+1")
    } else {
      B <<- F4CreateMatrix("1")
    }
    
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
    
    fvalb <<- sapply(1:5,Transform,A=B)
    output$permb <-renderUI(h3(cycle.convert(fvalb)))
  })
  
  observeEvent(input$generateab, {
    
    AB <<- F4MatProd(A, B)
    
    output$matrixAB <<- renderUI({jax.matrix(AB, name = "AB")})
    
    fvalab <<- sapply(1:5,Transform,A=AB)
    output$permab <- renderUI(h3(cycle.convert(fvalab)))
    
    output$permabtimes <- renderUI(h3(Perm.multiply(cycle.convert(fvala), cycle.convert(fvalb))))
  })
  
  observeEvent(input$reset, {
    
    output$matrixA <- renderUI("")
    output$matrixB <- renderUI("")
    output$matrixAB <- renderUI("")
    output$perma <- renderUI("")
    output$permb <- renderUI("")
    output$permab <- renderUI("")
    output$permabtimes <- renderUI("")
  })
}

#Run the app
shinyApp(ui = ui, server = server)