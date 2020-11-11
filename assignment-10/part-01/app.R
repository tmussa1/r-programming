library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

facs <- c(1,2)
numer <- c(3, 5, 7, 9)

header <- dashboardHeader(title = "SAT Scores in 2008",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
           column(width=2,
                  selectInput("factor1", "Factor 1", colnames(Sat2008)[facs]),
                  selectInput("factor2", "Factor 2", colnames(Sat2008)[facs]),
                  selectInput("num1", "Numeric 1", colnames(Sat2008)[numer]),
                  selectInput("num2", "Numeric 2", colnames(Sat2008)[numer]),
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
ui <- dashboardPage(header, sidebar, body, skin = "green")


server <- function(session, input, output) {

  dset <- Sat2008[, c(1,2,3,5,7,9)]
  
  output$tbl <- renderTable(dset)
  
  observeEvent(input$btnHist, {
    output$plot <- renderPlot({
      x    <- dset[,input$num1]
      hist(x, breaks = 25, col = "#75AADB", border = "white",
           xlab = input$num1,
           main = "", warn.unused = FALSE)
    })
  })
  
  observeEvent(input$btnBar,{
    output$plot <- renderPlot({
      x <- dset[,2]
      barplot(table(x))
    })
  })
  
  observeEvent(input$btnBox,{
    output$plot <- renderPlot({
      formula <- as.formula(paste(input$num1,"~",colnames(Sat2008)[facs][2]))
      boxplot(formula, dset)
    })
  })
  
  observeEvent(input$btnTable,{
    x <- dset[,input$factor1]
    y <- dset[,input$factor2]
    tbl<- as.data.frame.matrix(table(x,y))
    output$tbl2 <- renderTable(tbl,rownames= TRUE)
  })
  
  observeEvent(input$btnScat,{
    x <- dset[,input$num1]
    y <- dset[,input$num2]
    output$plot <- renderPlot({plot(x,y, xlab = input$num1, ylab = input$num2)})
  })
  
  observeEvent(input$btnRegress,{
    x <- dset[,input$num1]
    y <- dset[,input$num2]
    formula <- as.formula(paste(input$num2,"~",input$num1))
    dset.lm <- lm(formula, dset)
    output$plot <- renderPlot({plot(x,y, xlab = input$num1, ylab = input$num2)
      abline(dset.lm)})
  })
}


shinyApp(ui = ui, server = server)