library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(pacman)


stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

header <- dashboardHeader(title = "Short Test",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),
  tags$iframe(name = "picture", width = "700", height = "400"),
  fluidRow(
    column(width = 5,
           h3("Prove that you are not a robot by clicking on buses"),
           img (id = "bus", src="bus.jpg", alt="Bus", usemap="#busmap"),
           tags$map( name="busmap",
                     
                     tags$area(id = "sun", target = "picture", shape="rect" ,coords="0,390,135,520" ,alt="Sun", href = "schoolbus1.jpg"),
                     tags$area(id = "merc", target = "picture",  shape="rect", coords="135,390,270,520", alt="Mercury" ,href="schoolbus2.jpg"),
                     tags$area(id = "venus", target = "picture",  shape="rect", coords="135,250,270,390", alt="Venus", href="busiframe.jpg")),
           br(),
           br(),
           br(),
           textOutput("astro"),
           uiOutput("show")
           
    ),
    column(width = 4,
           plotOutput("plot1", click ="plot_click", brush = "plot_brush", height = 600),
           br(),
           br(),
           uiOutput("info")
    )
    ,
     column(width = 3,
            p(id = "elementp", "Scrambled I word am. Me change clicking by"),
            verbatimTextOutput("elementp")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green")


server <- function(session, input, output) {

  onclick("sun",{output$astro <- renderText("You clicked on a large school bus.")})
  onclick("merc",{output$astro <- renderText("You clicked on a small school bus.")})
  onclick("venus",{output$astro <- renderText("You clicked on a normal bus.")})
  onclick("bus",{output$astro <- renderText("Prove that you are not a robot.")})
  
  onclick("elementp", {output$elementp <- renderPrint("I am a scrambled word. Change me by clicking.")})
  
  x <- mtcars[,4]
  y <- mtcars[,5]
  N.out <- 50
  
  output$plot1 <- renderPlot({plot(x,y, xlab = 4, ylab = 5)})
  
  observeEvent(input$plot_click,{
    output$info <- renderUI(paste("x = ", input$plot_click$x, "y = ",
                                    input$plot_click$y))
  })

}


shinyApp(ui = ui, server = server)