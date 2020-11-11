#SocialSurvey
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)   #datasets to accompany Chihara and Hesterberg
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#Load the dataset
dset <- get("GSS2002")
#Get rid of ID column
dset$ID <- NULL
#Make a data frame with selected columns only
df <- dset[c(1,2,4,5,7,12,13,19)]

#The user interface
header <- dashboardHeader(title = "General Social Survey 2002",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 200,
  selectInput("factor1", "Factor 1 (Row)", colnames(df)),
  selectInput("factor2", "Factor 2 (Column)", colnames(df)),
  actionButton("btnGen", "Generate Table"),
  uiOutput("f1v1"),
  uiOutput("f1v2"),
  uiOutput("f2v1"),
  uiOutput("f2v2"),
  actionButton("btnGen2", "Generate 2x2 Table")
  
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 5,
           tableOutput("tbl")
    ),
    column(width = 4,
           h4("Contingency Table"),
           tableOutput("conting"),
           h4("Expected values if independent"),
           tableOutput("expect"),
           h4("Contributions to chi-square"),
           tableOutput("chisq"),
           textOutput("stat"),
           textOutput("concl"),
    ),
    column(width = 3,
           h3("Permutation Test"),
           plotOutput("hist"),
           textOutput("msg")
   )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

source("permtest.R")

#Additional functions are OK here, but no variables



server <- function(session, input, output) {
  output$tbl <- renderTable(df)
  observeEvent(input$factor1,{
    v <- df[,input$factor1]
    uniq <- unique(v)
    output$f1v1 <- renderUI(selectInput("f1val1","Factor 1 Value 1",uniq))
    output$f1v2 <- renderUI(selectInput("f1val2","Factor 1 Value 2",uniq))
  })
  observeEvent(input$factor2,{
    v <- df[,input$factor2]
    uniq <- unique(v)
    output$f2v1 <- renderUI(selectInput("f2val1","Factor 2 Value 1",uniq))
    output$f2v2 <- renderUI(selectInput("f2val2","Factor 2 Value 2",uniq))
  })


  observeEvent(input$btnGen, {
    v1 <- df[,input$factor1]
    v2 <- df[,input$factor2]
    tbl<- as.data.frame.matrix(table(v1,v2))
    output$conting <- renderTable(tbl,rownames= TRUE)
    expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl)
    output$expect <- renderTable(expected,rownames= TRUE)
    chisquare <- (tbl-expected)^2/expected
    output$chisq <- renderTable(chisquare,rownames= TRUE)
    chisqsum <- sum(chisquare)
    df <- (nrow(tbl)-1)*(ncol(tbl)-1)
    msg <- sprintf("Chi squared statistic = %5.2f with %d degrees of freedom",chisqsum,df)
    output$stat <- renderText(msg)
    pval <- pchisq(chisqsum, df, lower.tail = FALSE)
    msg2 <- sprintf("If factors are independent the probability of this large a value is roughly %9.8f",pval)
    output$concl <- renderText(msg2)
    result <- permute.test(v1,v2,10000)
    output$hist <- renderPlot({hist(c(result,chisqsum),prob = TRUE)
      abline(v = chisqsum, col = "blue")
      curve(dchisq(x,df),add = TRUE, col = "red")})
    
    output$msg <- renderText(sprintf("The fraction of permutations that led to a chi-square value greater than the observed value was %9.8f",sum(result > chisqsum)/10000))
    
  })
  observeEvent(input$btnGen2, {
    df22 <- subset(df, df[,input$factor1] %in% c(input$f1val1, input$f1val2) & 
                     df[,input$factor2] %in% c(input$f2val1, input$f2val2), select = c(input$factor1,input$factor2))
    v1 <- factor(df22[,1])
    v2 <- factor(df22[,2])
    tbl<- as.data.frame.matrix(table(v1,v2))
    output$conting <- renderTable(tbl,rownames = TRUE)
    expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl)
    output$expect <- renderTable(expected,rownames= TRUE)
    chisquare <- (tbl-expected)^2/expected
    output$chisq <- renderTable(chisquare,rownames= TRUE)
    chisqsum <- sum(chisquare)
    df <- (nrow(tbl)-1)*(ncol(tbl)-1)
    msg <- sprintf("Chi squared statistic = %5.2f with %d degrees of freedom",chisqsum,df)
    output$stat <- renderText(msg)
    pval <- pchisq(chisqsum, df, lower.tail = FALSE)
    msg2 <- sprintf("If factors are independent the probability of this large a value is roughly %9.8f",pval)
    output$concl <- renderText(msg2)
    result <- simple.test(v1,v2,10000)
    output$hist <- renderPlot({
      hist(result)  
      abline(v = tbl[1,1], col = "blue")
    })
    pVal <- min(sum(result > tbl[1,1])/10000,
                sum(result < tbl[1,1])/10000)
    output$msg <- renderText(sprintf("The fraction of permutations that led to a top left entry more extreme than the observed value was %9.8f, so the 2-sided P-value is %9.8f" , pVal,2*pVal))

  })
}

#Run the app
shinyApp(ui = ui, server = server)