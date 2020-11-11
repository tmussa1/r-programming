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

dset <- get("MathAnxiety")[, 1:5]

header <- dashboardHeader(title = "Permutation Testing for Math Anxiety among Primary and Secondary School Boys Vs Girls",
                          titleWidth = 1500)
sidebar <- dashboardSidebar(width = 200,
                            radioButtons("scoretype", "Types of Math Anxiety Scales",
                                         choices = c("AMAS","RCMAS")),
                            actionBttn("btnnew","New permuted dataset")
)

doPlot <- function(dset, type) {
  formula <- as.formula(paste(type,"~","Gender"))
  boxplot(formula, dset)
}

scramble <- function(dset){
  dset$Gender <- sample(dset$Gender)
  return(dset)
}

doPermtest <- function(dset,type,nPerm,stat){
  result <- numeric(nPerm)
  for (i in 1:nPerm){
    permset <- scramble(dset)
    men <- which(permset$Gender == "Boy")
    result[i] <- switch(stat,
                        diff =mean(permset[men,type])-mean(permset[-men,type]),
                        ratio = mean(permset[,type][men])/mean(permset[,type][-men]),
                        mdiff = median(permset[,type][men])-median(permset[,type][-men])
    )
  }
  return (result)
}

body <- dashboardBody(
  fluidRow(stylesheet,
           column(width = 3,
                  h3("The actual dataset"),
                  tableOutput("tbl")
           ),
           column(width = 3,
                  h3("A permuted dataset"),
                  tableOutput("scramtbl")
                  
           ),
           column(width = 3,
                  h3("Analysis of actual dataset"),
                  plotOutput("trueplot"),
                  uiOutput("truediff"),
                  uiOutput("trueratio"),
                  uiOutput("truemed"),
                  h3("Analysis of permuted dataset"),
                  plotOutput("scrambleplot"),
                  uiOutput("scramdiff"),
                  uiOutput("scramratio"),
                  uiOutput("scrammed")
           ),
           column(width = 3,
                  h3("Permutation test"),
                  sliderInput("nsample","Number of permuted samples",1000,20000,5000),
                  radioButtons("btnstat","Statistic",
                               choiceNames = c("Difference of means","Ratio of means","Difference of medians"),
                               choiceValues = c("diff","ratio","mdiff")),
                  actionBttn("btntest","Conduct the test"),
                  plotOutput("hist"),
                  uiOutput("pval"),
                  uiOutput("conclusion")
           )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(session, input, output) {

  type <- "AMAS"
  output$tbl <- renderTable(dset)
  permset <- scramble(dset)
  output$scramtbl <- renderTable(permset)
  
  analyze <- function(dset,type,realdata){
    if (realdata){
      output$trueplot <- renderPlot({doPlot(dset,type)})
      men <- which(dset$Gender == "Boy")
      output$truediff <- renderUI(h4(paste("Difference in means =",mean(dset[men,type])-mean(dset[-men,type]))))
      output$trueratio <- renderUI(h4(paste("Ratio of means =",
                                            round(mean(dset[men,type])/mean(dset[-men,type]),digits =2))))
      output$truemed <- renderUI(h4(paste("Difference in medians =",
                                          median(dset[men,type])-median(dset[-men,type]))))
    }
    else {
      output$scrambleplot <- renderPlot({doPlot(dset,type)})
      men <- which(dset$Gender == "Boy")
      output$scramdiff <- renderUI(h4(paste("Difference in means =",
                                            mean(dset[men,type])-mean(dset[-men,type]))))
      output$scramratio <- renderUI(h4(paste("Ratio of means =",
                                             round(mean(dset[men,type])/mean(dset[-men,type]),digits =2))))
      output$scrammed <- renderUI(h4(paste("Difference in medians =",
                                           median(dset[men,type])-median(dset[-men,type]))))
    }
  }
  
  analyze(dset,type,TRUE)
  analyze(permset,type,FALSE)
  
  observeEvent(input$scoretype,{
    type <<- input$scoretype
    analyze(dset,type,TRUE)
    analyze(permset,type,FALSE)
  })
  
  observeEvent(input$btnnew,{
    permset <<- scramble(dset)
    output$scramtbl <- renderTable(permset)
    analyze(permset,type,FALSE)
  })
  
  observeEvent(input$btntest,{
    result <- doPermtest(dset,type,input$nsample,input$btnstat)
    men <- which(dset$Gender == "Boy")
    vline <- switch(input$btnstat,
                    diff = mean(dset[men,type])-mean(dset[-men,type]),
                    ratio = mean(dset[men,type])/mean(dset[-men,type]),
                    mdiff = median(dset[men,type])-median(dset[-men,type])
    )
    output$hist <- renderPlot({
      hist(result)
      abline(v = vline, col = "blue")
    })
    pvalue <- (sum(result >= vline )+1)/(input$nsample+1)
    output$pval <- renderUI(h3(paste("Pvalue =", pvalue)))
    output$conclusion <- renderUI(h3(paste("From running the test mutiple times, 
                                           the p value is not statistically significant. We accept the null hypothesis 
                                           that boys and girls are equally likely to have Math anxiety")))
  })
}


shinyApp(ui = ui, server = server)