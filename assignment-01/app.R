#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    title{
      color: blue;
    }
    h2{
      color: aquamarine;
      font-family: "Times New Roman", Times, serif;
    }
    h3{
      color: teal;
      font-family: Arial, Helvetica, sans-serif;
    }
    h4{
     color: CadetBlue;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "My Blog",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
             column(
               width=6,
               h1("My Biography"),
               h3("I was born in Addis Ababa, Ethiopia and moved to Minnesota when I was young"),
               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/S1T8NU8RYOk" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
             ),
             column(
               width=3,
               h2("My background"),
               h4("I completed my undergraduate degree at ", a(href= "https://www.udc.edu/", " a school in Washington, DC")),
               h4("I have a Bachleor's degree and pursuing ALM in Software Engineering
                       at Harvard Extension School"),
               h4("I have not had a math class in a while but loved math growing up 
                  and I am excited about this course"),
               h5("I solved this minimization problems to figure out if I have calculus fresh in my memory"),
               h6(jaxI("x^7 + 16x^5 + 16 = 0")),
               h5("And I did the integral of "),
               h6(jaxI("log(x^2 + 25)")),
               h5("And a little bit of converting decimal to binary"),
               h6(jaxI("1110 = 2^3(1) + 2^2(1) + 2^1(1) + 2^0(0)")),
               withTags(table(
                 caption("I have completed around 9 courses towards my Master's degree. 
                         Some of the courses I took have served me well professionally"),
                 tr(th("My favorite courses thus far || "), th("when I took them")),
                 tr(td("CSCI E-97"), td("Fall 2019")),
                 tr(td("CSCI E-66"), td("Spring 2020")),
                 tr(td("DGMD E-28"), td("Spring 2020")),
                 tr(td("CSCI E-57"), td("Spring 2019"))
               ))
             ),
             column(
               width=3,
               h3("Hobbies"),
               h5("I like watching all sorts of news and political commentaries"),
               h5("I once campaigned for Senator Bernie Sanders"),
               h5("I like playing simple games when I am not working or watching TV like"),
               withTags(ol(
                 li("Sudoku"),
                 li("Cross word puzzles"),
                 li("Chess")
               )),
               img(src ="www/sudoku.png", width="100%")
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
    #Variables that are shared among server functions (use <<-)
    
    #Initialization
    
    #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)