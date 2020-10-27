library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")
source("fieldcalc.R")
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    #conclusion{
      color : blue;
    }
  ')
))

header <- dashboardHeader(title = "Irreducible Polynomials and Generators in Z5 of Degree 2",
                          titleWidth = 1000)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width= 4,
           actionBttn("btnfind","Click the buttons to see what gets generated")
    ),
    column(width= 8,
           actionBttn("btnfirst",jaxD("x^{2} + 2")),
           actionBttn("btnsecond",jaxD("x^{2} + 3")),
           actionBttn("btnthird",jaxD("x^{2} + x + 1")),
           actionBttn("btnfourth",jaxD("x^{2} + x + 2")),
           actionBttn("btnfifth",jaxD("x^{2} + 2x + 3")),
           actionBttn("btnsixth",jaxD("x^{2} + 2x + 4")),
           actionBttn("btnseventh",jaxD("x^{2} + 3x + 3")),
           actionBttn("btneighth",jaxD("x^{2} + 3x + 4")),
           actionBttn("btnninth",jaxD("x^{2} + 4x + 1")),
           actionBttn("btntenth",jaxD("x^{2} + 4x + 2")),
           h3(uiOutput("list")
    ),
    column(width = 6,
           h3(uiOutput("conclusion"))
          ))
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green")

quadPoly <- function(x, a, b, p) {
  (x^2 + a*x + b)%%p
}

server <- function(session, input, output) {
 
  observeEvent(input$btnfind,{
    p <- as.numeric(5)
    n <- as.numeric(2)
    result <- character(0)
    if (n == 2) {
      for (a in 0:(p-1)){
        for (b in 0:(p-1))
          for (x in 0:(p-1)){
            if(quadPoly(x,a,b,p)==0) break
            if(x == (p-1)){
              polystr <- convertPoly(c(b,a,1))
              result <- paste(result,polystr,sep="<br/>")
            }
          }
        
      } 
    }})
  
  observeEvent(input$btnfirst,{
    
    res1 <- powerTable(5, 2, c(0,1),c(3,0))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is not a generator. It is not of order 25")
  })
  
  observeEvent(input$btnsecond,{
    
    res1 <- powerTable(5, 2, c(0,1),c(2, 0))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is not a generator. It is not of order 25")
    
  })
  
  observeEvent(input$btnthird,{
    
    res1 <- powerTable(5, 2, c(0,1),c(4, 4))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is not a generator. It is not of order 25")
    
  })
  
  observeEvent(input$btnfourth,{
    
    res1 <- powerTable(5, 2, c(0,1),c(3, 4))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is a generator. It is of order 25")
  })
  
  observeEvent(input$btnfifth,{
    
    res1 <- powerTable(5, 2, c(0,1),c(2,3))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is a generator. It is of order 25")
    
  })
  
  observeEvent(input$btnsixth,{
    
    res1 <- powerTable(5, 2, c(0,1),c(1,3))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is not a generator. It is not of order 25")
    
  })
  
  observeEvent(input$btnseventh,{
    
    res1 <- powerTable(5, 2, c(0,1),c(2,2))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is a generator. It is of order 25")
    
  })
  
  observeEvent(input$btneighth,{
    
    res1 <- powerTable(5, 2, c(0,1),c(1,2))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is not a generator. It is not of order 25")
  })
  
  observeEvent(input$btnninth,{
    
    res1 <- powerTable(5, 2, c(0,1),c(4,1))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is not a generator. It is not of order 25")
    
  })
  
  observeEvent(input$btntenth,{
    
    res1 <- powerTable(5, 2, c(0,1),c(3,1))
    
    print(lapply(res1,convertPoly))
    
    output$list <- renderUI(h3(HTML(paste0(lapply(res1,convertPoly), sep="<br/>"))))
    output$conclusion <- renderText("X is a generator. It is of order 25")
    
  })
}


shinyApp(ui = ui, server = server)