#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#install.packages("unifed")
library(unifed) #for Irwin-Hall
vih <- Vectorize(dirwin.hall,"x")
#source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Sampling distributions",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 200,
                            radioButtons("dist", "Distribution type:",
                                         c("Bernoulli" = "bern",
                                           "Geometric" = "geom",
                                            "Poisson"="pois",
                                           "Uniform" = "unif",
                                           "Exponential" = "expo",
                                           "Normal" = "norm",
                                           "Square of normal" = "sqnorm"
                                          
                                           )),
 
                            sliderInput("p", "Success probability(Bernoulli and geometric)",
                                   value = 0.5,
                                   min = 0.01,
                                   max = 0.99),
                            
                            sliderInput("alpha", "Expectation (Poisson)",
                                   value = 3,
                                   min = 0.1,
                                   max = 10)
                            
                           
                          
                            
                            
                            
)
body <- dashboardBody(
  fluidRow(stylesheet,
           column(width = 4,
             h3("Underlying distribution"),
             plotOutput("plot1"),
             uiOutput("msg1")
           ),
           column(width = 4,

                  sliderInput("ndisp",
                              "Samples to display",
                              value = 10000,
                              min = 2000,
                              max = 100000,
                              width = "100%"),
                  h3("Sampling distribution"),
                  plotOutput("plot2"),
                  uiOutput("msg2")

           ),
           column(width = 4,
                  sliderInput("size",
                              "Sample size",
                              value = 4,
                              min = 1,
                              max = 200,
                              width = "100%"
                  ),
                  h3("Predicted sampling distribution"),
                  plotOutput("plot3"),
                  uiOutput("msg3"),
           )
    
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  maxval <- 1

  output$plot1 <- renderPlot({
    dist <- input$dist
    p <- input$p
    alpha <- input$alpha
    switch(dist,
      bern = barplot(dbinom(0:1,1,p),names.arg = c(0,1)),
      geom = barplot(dgeom(0:10,p),names.arg = c(0:10)),
      pois = barplot(dpois(0:20,alpha),names.arg = c(0:20)),
      unif = curve(dunif, from = 0,to = 1),
      expo = curve(dexp(x, rate = 0.5),from = 0,to = 10),
      norm = curve(dnorm, from = -4,to = 4),
      sqnorm = curve(dchisq(x,df=1),from = 0,to = 10),
    )
  })
  
  output$plot2 <- renderPlot({
    dist <- input$dist
    n <- input$size
    p <- input$p
    alpha <- input$alpha
    samp <- input$rescale
    N <- input$ndisp
    results <- numeric(N)
    switch(dist,
           bern = {
             var1 <- p-p^2
             exp1 <- p
             for (i in 1:N){
               x <- sum(rbinom(1,n,p))
               results[i] <- x
             }
             hist(results, probability = TRUE, breaks = (0:(n+1))-0.5,right = FALSE)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
             curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
 
           },
           geom = {
             var1 <- (1-p)/p^2
             exp1 <- 1/p -1 
             for (i in 1:N){
               x <- sum(rgeom(n,p))
               results[i] <- x
             }
             maxval <<- max(results)
             hist(results, probability = TRUE, breaks = (-1:max(results))+0.5)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
               curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
             
           },
           pois = {
             var1 <- alpha
             exp1 <- alpha 
             for (i in 1:N){
               x <- sum(rpois(n,alpha))
               results[i] <- x
             }
             maxval <<- max(results)
             hist(results, probability = TRUE, breaks = (-1:max(results))+0.5)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
             curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
             
           },
           unif = {
             var1 <- 1/12
             exp1 <- 1/2 
             for (i in 1:N){
               x <- sum(runif(n))
               results[i] <- x
             }
             maxval <<- max(results)
             hist(results, probability = TRUE, breaks = (0:(10*maxval+1))/10)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
             curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
             
           },
           expo = {
             var1 <- 4
             exp1 <- 2 
             for (i in 1:N){
               x <- sum(rexp(n, rate = 0.5))
               results[i] <- x
             }
             maxval <<- max(results)
             hist(results, probability = TRUE, breaks = (0:(5*maxval+1))/5)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
             curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
             
           },
           norm = {
             var1 <- 1
             exp1 <- 0
             for (i in 1:N){
               x <- sum(rnorm(n))
               results[i] <- x
             }
             maxval <<- max(abs(results))
             hist(results, probability = TRUE, breaks = (-(5*maxval+1):(5*maxval+1))/5)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
               curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
             
           },
           sqnorm = {
             var1 <- 2
             exp1 <- 1
             for (i in 1:N){
               x <- sum(rchisq(n,df=1))
               results[i] <- x
             }
             maxval <<- max(results)
             hist(results, probability = TRUE, breaks = (0:(5*maxval+1))/5)
             output$msg1 <- renderUI(h3(paste("Expectation = ",round(exp1,3),"Variance = ",round(var1,3),
                                              "Standard deviation = ",round(sqrt(var1),3))))
             curve(dnorm(x, mean = n*exp1, sd = sqrt(n*var1)),  add = TRUE, col = "red")
             
           }
    )

    output$msg2 <- renderUI(h3(paste("Sample mean = ",round(mean(results),3), "Sample variance =",round(var(results),3))))

  })
  output$plot3 <- renderPlot({
    dist <- input$dist
    n <- input$size
    p <- input$p
    alpha <- input$alpha
    switch(dist,
      bern = barplot(dbinom(0:n,n,p),names.arg = 0:n),
      geom = barplot(dnbinom(0:maxval,n,p),names.arg = 0:maxval),
      pois = barplot(dpois(0:maxval,n*alpha),names.arg = 0:maxval),
      unif = curve(vih(x,n), from = 0, to = maxval),
      expo = curve(dgamma(x, shape = n, rate = 0.5), from = 0, to = maxval),
      norm = curve(dnorm(x,0,sqrt(n)), from = -maxval, to = maxval),
      sqnorm = curve(dchisq(x,df=n),from = 0,to = maxval),
    )
  })


  
  output$msg3 <- renderUI({
    alpha <- input$alpha
    n <- input$size
    p <- input$p
    h3(HTML(paste0( "Expectation =  ",
                             round(switch(input$dist,
                              bern = n*p,
                              geom = n/p-n,
                              pois = n*alpha,
                              unif = n/2,
                              expo = 2*n,
                              norm = 0,
                              sqnorm = n
                             ),3),
       " Variance = ",
       round(switch(input$dist,
              bern = n*p*(1-p),
              geom = n*(1-p)/p^2,
              pois = n*alpha,
              unif = n/12,
              expo = 4*n,
              norm = n,
              sqnorm = 2*n
            ),3),
       br(),
       "This is a",ifelse(input$dist == "unif","n "," "),
       switch(input$dist,
              bern = "binomial",
              geom = "negative binomial",
              pois = "Poisson",
              unif = "Irwin-Hall",
              expo = "gamma",
              norm = "normal",
              sqnorm = "chi-square"
       ),
       " distribution"))
    )
    
       
  })
  



  
  
}

#Run the app
shinyApp(ui = ui, server = server)