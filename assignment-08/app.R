library(shiny)
library(shinydashboard)
library(shinyWidgets)
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

header <- dashboardHeader(title = "Irreducible Polynomials and Generators in Z5 of Degree 2",
                          titleWidth = 1000)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
           actionBttn("btnfind","Find Irreducible Polynomials")
    ),
    column(
      width = 4,
      h3("Irreducible polynomials"),
      h3(uiOutput("list"))
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green")

convertPoly <- function(coeff, carats = FALSE) {
  v <- character(0)
  n <- length(coeff)
  while(n > 0) {
    if (n > 2 && coeff[n] > 0){
      if (coeff[n] > 1) v <- c(v, coeff[n])
      v <- c(v,"x",ifelse(carats,"^","<sup>"),n-1,ifelse(carats,"","</sup>"),"+")
    }
    
    if (n== 2 && coeff[2] > 0) {
      if (coeff[2] > 1) v <- c(v, coeff[2])
      v <- c(v,"x","+")
    }
    if ((n == 1) && (length(v) == 0 || coeff[1]>0)) v <- c(v, coeff[1])
    n <- n-1
  }
  if (tail(v,1)=="+") v <- head(v,-1)
  return(paste(v,sep="",collapse=""))
}

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
      output$list <- renderUI(h3(HTML(result)))
    }})
}


shinyApp(ui = ui, server = server)