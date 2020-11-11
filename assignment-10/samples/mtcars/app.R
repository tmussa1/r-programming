#Replace this line with the folder name of the app
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

#Here are the columns that are really factors
facs <- c(2,8,9,10,11)

#The user interface
header <- dashboardHeader(title = "Motor Trend car data",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=2,
           selectInput("factor1", "Factor 1", colnames(mtcars)[facs]),
           selectInput("factor2", "Factor 2", colnames(mtcars)[facs]),
           selectInput("num1", "Numeric 1", colnames(mtcars)[-facs]),
           selectInput("num2", "Numeric 2", colnames(mtcars)[-facs]),
           actionBttn("btnHist", "Histogram for Numeric 1"),
           actionBttn("btnBar", "Barplots for Factor 1"),
           actionBttn("btnBox", "Boxplot for Numeric 1 against Factor 1"),
           actionBttn("btnTable", "Table for Factor 2 against Factor 1"),
           actionBttn("btnScat", "Scatter plot for Numeric 2 against Numeric 1"),
           actionBttn("btnRegress", "Regression line for Numeric 2 against Numeric 1")
    ),
    column(width = 5,
      tableOutput("tbl")),
    column(width = 5,
           plotOutput("plot"),
           tableOutput("tbl2"))
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")

#Additional functions are OK here, but no variables
mtcars

server <- function(session, input, output) {
  output$tbl <- renderTable(mtcars)
  observeEvent(input$btnHist, {
    output$plot <- renderPlot({
      x    <- mtcars[,input$num1]
      hist(x, breaks = 25, col = "#75AADB", border = "white",
           xlab = input$num1,
           main = "", warn.unused = FALSE)
    })
  })
  observeEvent(input$btnBar,{
    output$plot <- renderPlot({
      x <- mtcars[,input$factor1]
      barplot(table(x))
    })
  })
  observeEvent(input$btnBox,{
    output$plot <- renderPlot({
      formula <- as.formula(paste(input$num1,"~",input$factor1))
      boxplot(formula, mtcars)
    })
  })
  observeEvent(input$btnTable,{
    x <- mtcars[,input$factor1]
    y <- mtcars[,input$factor2]
    tbl<- as.data.frame.matrix(table(x,y))
    output$tbl2 <- renderTable(tbl,rownames= TRUE)
  })
  observeEvent(input$btnScat,{
    x <- mtcars[,input$num1]
    y <- mtcars[,input$num2]
    output$plot <- renderPlot({plot(x,y, xlab = input$num1, ylab = input$num2)})
  })
  observeEvent(input$btnRegress,{
    x <- mtcars[,input$num1]
    y <- mtcars[,input$num2]
    formula <- as.formula(paste(input$num2,"~",input$num1))
    mtcars.lm <- lm(formula, mtcars)
    output$plot <- renderPlot({plot(x,y, xlab = input$num1, ylab = input$num2)
      abline(mtcars.lm)})
  })
}

#Run the app
shinyApp(ui = ui, server = server)