library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(flextable)
#The user interface
header <- dashboardHeader(title = "Euclid's GCD Algorithm", titleWidth = 400)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
    column(
      width = 3,
      h3("Specify two integers whose GCD you want"),
      textInput("a","a", value = 37),
      textInput("b","b",value = 30),
      actionBttn("btnshow", "Show the table"),
      actionBttn("btnfirst", "The first step"),
      actionBttn("btnnext", "The next step")
    ),
    column(
      width = 6,
      tableOutput("flex"),
      uiOutput("msg"),
      uiOutput("msg2"),
      uiOutput("msg3")
    )
  )
  
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("gcdcalc.R")



#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  
  #Variables that are shared among server functions
  DF <- data.frame(ncol = 6)
  tbl <- flextable(DF)
  a <- 1
  b <- 1
  down <- TRUE
  row <- 1
  nstep <- 1
  
  observeEvent(input$btnshow,{
    a <<- as.numeric(input$a)
    b <<- as.numeric(input$b)
    DF <<- gcd.euclid(a,b, details=TRUE)$df
    nstep <<- nrow(DF)
    tbl <<- flextable(DF)
    tbl <<- padding(tbl, padding = 15,part = "all")
    tbl <<- fontsize(tbl,size = 30,part = "all")
    output$flex <- renderUI({htmltools_value(tbl)})
    output$msg <- renderUI("")
    output$msg2 <- renderUI("")
    output$msg3 <- renderUI("")
  })
  observeEvent(input$btnfirst,{
    down <<- TRUE
    row <<- 1
    tbl <<- bg(tbl, 1, 1:4, 'yellow')
    output$flex <- renderUI({htmltools_value(tbl)})
    output$msg <-renderUI(h2(paste(DF[1,1],"=",DF[1,4],"x",DF[1,2],"+",DF[1,3])))
  })
  observeEvent(input$btnnext,{
    row <<- ifelse(down,row+1,row-1)
    if (row <1) row <<- 1
    if( down == TRUE){
      tbl <- bg(tbl, 1:row, 1:4, 'yellow')
    }
    if (down == FALSE) {
      tbl <-bg(tbl,  row:(nstep-1),5:6,'pink')
    }
    output$flex <- renderUI({htmltools_value(tbl)})
    if (down == TRUE)
    output$msg <-renderUI(h2(paste(DF[row,1],"=",DF[row,4],"x",DF[row,2],"+",DF[row,3])))
    if (down == FALSE){
      output$msg2 <-renderUI("")
      output$msg <-renderUI(h2(paste(DF[nstep,2],"=",DF[row,5],"x",DF[row,1],"+",DF[row,6],"x",DF[row,2])))
  }
    if (row == nstep){
      output$msg2 <- renderUI(h2("The remainder is 0 so we can stop"))
      output$msg3 <- renderUI(h2(paste("The greatest common divisor is ",DF[row,2])))
      down <<- FALSE
    }
  })
}
#Run the app
shinyApp(ui = ui, server = server)


