#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
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
header <- dashboardHeader(title = "Change this header",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
           column(width=4,
                  h3("Replace with your UI")
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
  
  A <- matrix(sample(0:4, 9, replace = TRUE), nrow=3, ncol = 3)
  A
  constCoef <- round(((4 * det(A)) %% 5), 2)
  constCoef
  lamCoef <- round(((4 * (-det(A[-2,-2]) - det(A[-3,-3]) - det(A[-1, -1]))) %% 5), 2)
  lamCoef
  
  lamSqCoef <- round(((4 * (A[1, 1] + A[2, 2] + A[3, 3])) %% 5), 2)
  lamSqCoef
  lamCubCoef <- round(((4 * -1) %% 5), 2)
  lamCubCoef
  
  findEigenValues <- function(){
    
    result <- c();
    
    for(x in 0:4){
      
      temp <- round( (((lamCubCoef * (x ^ 3)) + (lamSqCoef * (x ^ 2)) 
                     + (lamCoef * x) + constCoef)) %% 5, 2);
      temp
      
      if(temp == 0){
        result <- rbind(result, x);
      }
      
    }
    return (result);
  }
  
  findEigenValues();
  
  iden <- diag(3)
  iden
  
  ((lamCubCoef * (A%*%A%*%A)) + (lamSqCoef * (A%*%A)) + (lamCoef * A) + (constCoef * iden)) %% 5
}

#Run the app
shinyApp(ui = ui, server = server)