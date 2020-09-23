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
ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(session, input, output) {
  
  # A <- matrix(sample(0:4, 9, replace = TRUE), nrow=3, ncol = 3)
  A <- matrix(c(1,0,0,0,2,0,0,0,3), byrow = T, nrow=3)
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

  eigens <- findEigenValues();
  
  findVector <- function (mat){
    
    columnSum <- colSums(mat)
      
    for(ind in (1 : length(columnSum))){
      
      if(columnSum[ind] > 0){
        
        print(mat[ ,ind])
        
        return (mat[ ,ind]);
      }
    }
  }
  
  findEigenVectors <- function(){
    
    if(length(eigens) == 3){
      
      iden <<- diag(3);
      
      lamI1 <- (A - (eigens[1] * iden)) %% 5
      lamI2 <- (A - (eigens[2] * iden)) %% 5
      lamI3 <- (A - (eigens[3] * iden)) %% 5
      
      prodEigen <- (lamI2 %*% lamI3) %% 5
      eigenVeg1 <- findVector(prodEigen);
      print(prodEigen)
      print(eigenVeg1)
      
      prodEigen2 <- (lamI1 %*% lamI3) %% 5
      eigenVeg2 <- findVector(prodEigen2);
      print(prodEigen2)
      print(eigenVeg2)
      
      prodEigen3 <- (lamI1 %*% lamI2) %% 5
      eigenVeg3 <- findVector(prodEigen3);
      print(prodEigen3)
      print(eigenVeg3)
      
    } else {
      
    }
  }
  
  findEigenVectors();
  
  ((coeficients[1] * (A%*%A%*%A)) + 
      (coeficients[2] * (A%*%A)) + (coeficients[3] * A) + (coeficients[4] * iden)) %% 5
  
}

#Run the app
shinyApp(ui = ui, server = server)