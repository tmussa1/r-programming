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
  
  findInvertibleMatrix <- function (){
    
    repeat {
      
      m1 <<- matrix(sample(0:4, 9, replace = TRUE), nrow=3, ncol = 3)
      
      mtrace <<- m1[1,1] + m1[2,2] + m1[3,3]
      
      mdisc <<- (mtrace ^ 2) - (4 *  det(m1))
      
      if(det(m1) > 0 & mtrace > 0 & mdisc > 0){
        return (m1)
      }
    
    }
  }
  
  invertibleMatrix <- findInvertibleMatrix()
  mtrace <<- invertibleMatrix[1,1] + invertibleMatrix[2,2] + invertibleMatrix[3,3]
  
  eigenConst = det(invertibleMatrix) %% 5;
  eigenLam = (det(invertibleMatrix[-1, -1]) 
  + det(invertibleMatrix[-2, -2]) + det(invertibleMatrix[-3, -3])) %% 5;
  eigenLamSq = (-mtrace) %% 5;
  
}

#Run the app
shinyApp(ui = ui, server = server)