#Arithmetic
#August 30, 2020
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)

#The user interface
header <- dashboardHeader(title = "A trivial calculator")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(
        column(
            width = 6,
            radioButtons("multaddselecter", "Select add or multiply", choices = c("Add" ="add", "Multiply" = "multiply"))
        ),
        column(width = 6,
            sliderInput("left", "Left operand",-7, 7, 1 ),
            sliderInput("right", "Right operand",-7, 7, 1 ),
            h2(uiOutput("results"))
        )
    )
)
ui <- dashboardPage(header, sidebar, body)



#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  
   X <- reactive({input$left})
   Y <- reactive({input$right})
   
   sum <- reactive({input$left + input$right})
   product <- reactive({input$left * input$right})
   
   output$results <- renderUI({
     
     radioValue <- input$multaddselecter
     
     if(radioValue == "add"){
       withMathJax("$$x = ", X() , ",y=", Y() , ",x+y=",sum(), "$$")
     } else if(radioValue == "multiply"){
       withMathJax("$$x =", X(), ",y=", Y() , ",xy=",product(),"$$")
     }
    })
   
}

#Run the app
shinyApp(ui = ui, server = server)