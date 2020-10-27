library(shiny)
library(shinydashboard)
library(shinyWidgets)
#The user interface
header <- dashboardHeader(title = "Finite Field Calculator")
sidebar <- dashboardSidebar(
  sidebarMenu(id="menu",
    menuItem("Order 3", tabName = "order3"),
    menuItem("Order 4", tabName = "order4"),
    menuItem("Order 5", tabName = "order5"),
    menuItem("Order 7", tabName = "order7"),
    menuItem("Order 8", tabName = "order8"),
    menuItem("Order 9", tabName = "order9"),
    menuItem("Order 16", tabName = "order16"),
    menuItem("Order 25", tabName = "order25"),
    menuItem("Order 27", tabName = "order27")
  )    
)
body <- dashboardBody(
    tags$style(type='text/css', "#select {font-size: 32px !important} "),
    tabItems(
      tabItem(tabName = "order3",
        h2("Arithmetic in the Field of Order 3"),
      ),
      tabItem(tabName = "order4",
        h2("Arithmetic in the Field of Order 4")
      ),
      tabItem(tabName = "order5",
        h2("Arithmetic in the Field of Order 5")
      ),
      tabItem(tabName = "order7",
              h2("Arithmetic in the Field of Order 7")
      ),
      tabItem(tabName = "order8",
              h2("Arithmetic in the Field of Order 8")
      ),
      tabItem(tabName = "order9",
              h2("Arithmetic in the Field of Order 9")
      ),
      tabItem(tabName = "order16",
              h2("Arithmetic in the Field of Order 16")
      ),
      tabItem(tabName = "order25",
              h2("Arithmetic in the Field of Order 25")
      ),
      tabItem(tabName = "order27",
              h2("Arithmetic in the Field of Order 27")
      )
    ),
    fluidRow(
        column(3,selectInput("left",NULL,c("[0]","[1]","[2]"),selected = "[1]"),selectize = FALSE),
        column(2,awesomeRadio("op",NULL,c("+","-","*","/"), width = "100px")),
        column(3,selectInput("right",NULL,c("[0]","[1]","[2]"),selected = "[1]"),selectize = FALSE),
        column(2,actionBttn(inputId = "btncomp",label = "=")),
        column(2,uiOutput(outputId = "result"))        
    ),
    fluidRow(align = "center",uiOutput("mod")),
    fluidRow(align = "center",uiOutput("replace")),
    fluidRow(align = "center",uiOutput("show"))
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("fieldcalc.R")


#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  p <- 3
  n <- 1
  coeffs <- vector("list",49)
  dropdowns <- vector("list",49)
  displays <- vector("list",49)
  
    #Initialization
  updateSelectInput(session,"left",choices = c("[0]","[1]","[2]")) 
  updateSelectInput(session,"right",choices = c("[0]","[1]","[2]"))            
    #Functions that respond to events in the input
  observeEvent(input$menu, {
    if(input$menu == "order3") {
        p <<- 3; n <<- 1
        coeffs<<-powerTable(p,n,2,0)
    }
    if(input$menu == "order4") {
      p <<- 2; n <<- 2
      coeffs<<-powerTable(p,n,c(0,1),c(1,1))          
    }
    if(input$menu == "order5"){
      p <<- 5; n <<- 1
      coeffs<<-powerTable(5,1,2,0)          
    }
    if(input$menu == "order7") {
      p <<- 7; n <<- 1
      coeffs<<-powerTable(7,1,3,0)          
    }
    if(input$menu == "order8") {
      p <<- 2; n <<- 3
      coeffs<<-powerTable(2,3,c(0,1,0),c(1,1,0))          
    }
    if(input$menu == "order9") {
      p <<- 3; n <<- 2
      coeffs<<-powerTable(p,n,c(0,1),c(1,1))          
    }
    if(input$menu == "order16") {
      p <<- 2; n <<- 4
      coeffs<<-powerTable(2,4,c(0,1,0,0),c(1,1,0,0))          
    }
    if(input$menu == "order25") {
      p <<- 5; n <<- 2
      coeffs<<-powerTable(5,2,c(0,1),c(2,2))          
    }
    if(input$menu == "order27") {
      p <<- 3; n <<- 3
      coeffs<<-powerTable(3,3,c(0,1,0),c(2,1,0))          
    }
    dropdowns <<- lapply(coeffs,convertPoly)
    displays <<- lapply(coeffs,convertPoly,carats = FALSE)
    updateSelectInput(session,"left",choices = dropdowns) 
    updateSelectInput(session,"right",choices = dropdowns) 
    output$mod <- renderUI(h3(paste("All coefficients are modulo ",p)))
    output$replace <- renderUI("")
    if (n > 1)
      output$replace <- renderUI(
              h3(HTML(paste("Replace x<sup>",n,"</sup>   by ",displays[n+2]))))
  })
  observeEvent(input$btncomp, {
    index.left <- which(dropdowns == input$left)[1]
    index.right <- which(dropdowns == input$right)[1]
    if (input$op == "+"){
      sum <- (coeffs[[index.left]]+coeffs[[index.right]])%%p
      index.sum <- which(sapply(coeffs,function(x)all(x==sum)))
      output$result <- renderUI(dropdowns[index.sum])
      s <- paste("[",displays[index.left],"]+","[",displays[index.right],
                 "]=[",displays[index.sum],"]")
      output$show <- renderUI(h3(HTML(s)))
    }
    if (input$op == "-"){
      diff <- (coeffs[[index.left]]-coeffs[[index.right]]+p)%%p
      index.diff <- which(sapply(coeffs,function(x)all(x==diff)))
      output$result <- renderUI(dropdowns[index.diff])
      s <- paste("[",displays[index.left],"]-","[",displays[index.right],
                 "]=[",displays[index.diff],"]")
      output$show <- renderUI(h3(HTML(s)))
    }
    if (input$op == "*"){
      if ((index.left==1)||(index.right==1)) output$result <- renderUI(dropdowns[1]) 
      #Now the index is two greater than the logarithm
      if ((index.left>1)&&(index.right>1)) {
        log <- index.left+index.right-4
        index.prod <- log%%(p^n-1)+2
        output$result <- renderUI(h4(dropdowns[index.prod]))
        s <- paste("[",displays[index.left],"]*","[",displays[index.right],
                   "]=[",displays[index.prod],"]")
        output$show <- renderUI(h3(HTML(s)))
      }
    }
    if (input$op == "/"){
      if (index.right == 1)
        output$result <- renderUI("Cannot divide by zero!")
      if (index.right>1 && index.left == 1) output$result <- renderUI(dropdowns[1]) 
      if (index.left > 1 && index.right >1){
        #Take the difference of logarithms - number of nonzero field elements
        #Add 2 to this logarithm to get the table index
        index.div <- (index.left-index.right+p^n-1)%%(p^n-1)+2
        output$result <- renderUI(dropdowns[index.div])
        s <- paste("[",displays[index.left],"]/","[",displays[index.right],
                   "]=[",displays[index.div],"]")
        output$show <- renderUI(h3(HTML(s)))
      }
    }
    
  })

}

#Run the app
shinyApp(ui = ui, server = server)


