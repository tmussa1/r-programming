library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(pracma)
source("jaxmat.R")

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

header <- dashboardHeader(title = "Assignment 3 - Finding Eigen Values/Vectors and Proving Cayley Hamilton",
                          titleWidth = 1500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
           column(width=4,
                  actionButton("btnpickmat", "Pick a random 3X3 Matrix in Z5", style = "color:darkblue"),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  actionButton("btncharpoly", "Calculate it's characteristic polynomial", style = "color:darkgreen"),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  actionButton("btneigenval", "Find its eigen values", style = "color:darkred"),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  actionButton("btneigenvec", "Find its eigen vectors", style = "color:darkblue"),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  actionButton("btncayley", "Calculate Cayley Hamilton's Equation", style = "color:darkgreen")
                ),
           column(width =4,
                  uiOutput("matA"), 
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  uiOutput("charpolyout"),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  uiOutput("eigvalout"),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  HTML('<br/>'),
                  uiOutput("eigvecout")
          )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(session, input, output) {
  
  A <- matrix(nrow=3, ncol=3)
    
  observeEvent(input$btnpickmat, {
    
    A <<- matrix(sample(0:4, 9, replace = TRUE), nrow=3, ncol = 3)
    
    output$matA <- renderUI(jax.matrix(A, name = "A")) 
    
    output$charpolyout <- renderUI("")
    
    output$eigvalout <- renderUI("")
    
    output$eigvecout <- renderUI("")
    
    
  })
  
  observeEvent(input$btncharpoly, {
    lam = calculateCoefficients();
    
    output$charpolyout <- renderUI(jaxD(paste("`lambda^3 *", round(lam[1], digits = 3),
                                         "+`; `lambda^2 *",round(lam[2], digits = 3),
                                         "+`; `lambda *",round(lam[3], digits = 3),
                                         " + " ,round(lam[4], digits = 3),
                                                      " = ", round(0, digits = 3))))
    
  })
  
  observeEvent(input$btneigenval, {
    
    eigenValue <<- findEigenValues();
    
    if(length(eigenValue) == 1){
      
      output$eigvalout <- renderUI(jaxD(paste("`lambda_1 =", round(eigenValue[1], digits = 3))))
    
    } else if(length(eigenValue) == 2){
     
      output$eigvalout <- renderUI(jaxD(paste("`lambda_1 =", round(eigenValue[1], digits = 3),
                                              ",`;   `lambda_2 =",round(eigenValue[2], digits = 3))))
      
      
    } else if(length(eigenValue) == 3){
      
      output$eigvalout <- renderUI(jaxD(paste("`lambda_1 =", round(eigenValue[1], digits = 3),
                                              ",`;   `lambda_2 =",round(eigenValue[2], digits = 3),
                                              ",`;   `lambda_3 =",round(eigenValue[3], digits = 3))))
      
    } else {
      output$eigvalout <- renderUI("There are no eigen values");
    }
    
  })
  
  observeEvent(input$btneigenvec, {
  
    eigenValue <<- findEigenValues();
    eigenVectors <<- findEigenVectors();
    
    if(length(eigenValue) < 3){
      output$eigvalout <- renderUI("The matrix has less than 3 distinct eigen values");
    } else {
      
      output$eigvecout <- renderUI(withTags(
        table(
          tr(
            th(jax.vector(round(eigenVectors[, 1],digits = 3), name = "v_1")),
            th(jax.vector(round(eigenVectors[, 2],digits = 3), name = "v_2")),
            th(jax.vector(round(eigenVectors[, 3],digits = 3), name = "v_3"))
          )
        )
      ));
    }
    
  })
  
  calculateCoefficients <- function(){

      constCoef <- round(((4 * det(A)) %% 5), 2)
      lamCoef <- round(((4 * (-det(A[-2,-2]) - det(A[-3,-3]) - det(A[-1, -1]))) %% 5), 2)
      lamSqCoef <- round(((4 * (A[1, 1] + A[2, 2] + A[3, 3])) %% 5), 2)
      lamCubCoef <- round(((4 * -1) %% 5), 2)

      coefs <- rbind(lamCubCoef, lamSqCoef, lamCoef, constCoef)

      return (coefs)
  }
  
  findEigenValues <- function(){

      result <- c();
      
      coeficients <<- calculateCoefficients();

      for(x in 0:4){

        temp <- round( (((coeficients[1] * (x ^ 3)) + (coeficients[2] * (x ^ 2))
                       + (coeficients[3] * x) + coeficients[4])) %% 5, 2);

        if(temp == 0){
          result <- rbind(result, x);
        }

      }
      return (result);
    }
  
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
    
    eigens <- findEigenValues();
    
    result <- c();

      if(length(eigens) == 3){

        iden <<- diag(3);

        lamI1 <- (A - (eigens[1] * iden)) %% 5
        lamI2 <- (A - (eigens[2] * iden)) %% 5
        lamI3 <- (A - (eigens[3] * iden)) %% 5

        prodEigen <- (lamI2 %*% lamI3) %% 5
        eigenVeg1 <- findVector(prodEigen);
        
        result <- cbind(result, eigenVeg1)

        prodEigen2 <- (lamI1 %*% lamI3) %% 5
        eigenVeg2 <- findVector(prodEigen2);
        
        result <- cbind(result, eigenVeg2)

        prodEigen3 <- (lamI1 %*% lamI2) %% 5
        eigenVeg3 <- findVector(prodEigen3);
        
        result <- cbind(result, eigenVeg3)

      } 
    
      return (result);
      
    }

  ((coeficients[1] * (A%*%A%*%A)) +
      (coeficients[2] * (A%*%A)) + (coeficients[3] * A) + (coeficients[4] * iden)) %% 5
  
}

#Run the app
shinyApp(ui = ui, server = server)

