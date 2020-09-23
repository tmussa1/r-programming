#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(pracma)
# source("jaxmat.R")   #for displaying mathematics
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
  
  # A <- matrix(sample(0:4, 9, replace = TRUE), nrow=3, ncol = 3)
  A <- matrix(c(1,0,0,0,2,-0,0,0,3), byrow = T, nrow=3)
  A <- A %% 5
  A
  
  calculateCoefficients <- function(){
    
    constCoef <- round(((4 * det(A)) %% 5), 2)
    lamCoef <- round(((4 * (-det(A[-2,-2]) - det(A[-3,-3]) - det(A[-1, -1]))) %% 5), 2)
    lamSqCoef <- round(((4 * (A[1, 1] + A[2, 2] + A[3, 3])) %% 5), 2)
    lamCubCoef <- round(((4 * -1) %% 5), 2)
    
    coefs <- rbind(lamCubCoef, lamSqCoef, lamCoef, constCoef)
    
    return (coefs)
  }
  
  coeficients <- calculateCoefficients()
  
  length(calculateCoefficients())
  
  findEigenValues <- function(){
    
    result <- c();
    
    for(x in 0:4){
      
      temp <- round( (((coeficients[1] * (x ^ 3)) + (coeficients[2] * (x ^ 2)) 
                     + (coeficients[3] * x) + coeficients[4])) %% 5, 2);
      temp
      
      if(temp == 0){
        result <- rbind(result, x);
      }
      
    }
    return (result);
  }
  
  length(findEigenValues());
 
  iden <- diag(3)
  
  eigens <- findEigenValues();
  
  lamI1 <- eigens[1] * iden
  lamI1
  
  lamI2 <- eigens[2] * iden
  
  lamI2
  
  lamI3 <- eigens[3] * iden
  
  lamI3
  
  prodEigen <- (lamI2 %*% lamI3) %% 5
  
  prodEigen <- cbind(prodEigen, c(0, 0, 0))
  
  prodEigen
  
  rref(prodEigen)
  
  prodEigen2 <- (lamI1 %*% lamI3) %% 5
  
  prodEigen2
  
  rref(prodEigen2)
  
  prodEigen2 <- cbind(prodEigen2, c(0, 0, 0))
  
  rref(prodEigen2)
  
  prodEigen3 <- (lamI1 %*% lamI2) %% 5
  
  prodEigen3 <- cbind(prodEigen3, c(0, 0, 0))
  
  rref(prodEigen3)
  
  mat <- matrix(c(3,4,3,2,1,3,2,1,4, 0, 0, 0), byrow = T, nrow=3)
  mat
  
  rref(mat)
  
  ((coeficients[1] * (A%*%A%*%A)) + 
      (coeficients[2] * (A%*%A)) + (coeficients[3] * A) + (coeficients[4] * iden)) %% 5
  
  
  
}

#Run the app
shinyApp(ui = ui, server = server)