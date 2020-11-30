#ImgClick
#This is an R version of an example from W3Schools.com
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

#The user interface
header <- dashboardHeader(title = "Clicking on an image")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    useShinyjs(),  #enable onclick 
    tags$iframe(name = "picture", width = "200", height = "200"),
    fluidRow(
        column(width = 6,
          h1("The map and area elements"),
          p("Click on the sun or on one of the planets to watch it closer"),
          img (id = "planet", src="planets.gif", alt="Planets", usemap="#planetmap", width="145", height="126"),
            
          tags$map( name="planetmap",
    
  tags$area(id = "sun", target = "picture", shape="rect" ,coords="0,0,82,126" ,alt="Sun", href = "sun.gif"),
  tags$area(id = "merc", target = "picture",  shape="circle", coords="90,58,3", alt="Mercury" ,href="merglobe.gif"),
      tags$area(id = "venus", target = "picture",  shape="circle", coords="124,58,8", alt="Venus", href="venglobe.gif")),
       textOutput("astro"),
  uiOutput("show")


               
               
        )
    )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")

#Variables that are shared among server functions

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  output$astro <- renderText("message here")
  onclick("sun",{output$astro <- renderText("You clicked on the sun.")})
  onclick("merc",{output$astro <- renderText("You clicked on Mercury.")})
  onclick("venus",{output$astro <- renderText("You clicked on Venus.")})
  onclick("planet",{output$astro <- renderText("You clicked on the picture.")})
    #Initialization
    
    #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)