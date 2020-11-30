
#EulerWalk
library(shiny)
library(shinydashboard)
library(shinyWidgets)

#Splitting up the ui makes it easier to read

header <- dashboardHeader(title = "Euler Walks")

sidebar <- dashboardSidebar(
  
  sliderInput("vertex", "Number of Vertices",6,12,8),
  actionButton("generate", "Generate New Graph")
)

body <- dashboardBody(
    fluidRow(
      column(
        width = 12,
        h3("This graph permits an Euler walk"),
        box(
          width = NULL,
          height = 500,
          plotOutput("graph",click="plot_click")
        ), #box
        uiOutput("message")
      ) #column
    ) #fluid
) #body

ui <- dashboardPage(header, sidebar, body)

source("eulergraph.R")


server <- function(input, output, session){
  #Global variables used by the server code can be defined and initialized here.
  #Matrix with one row for each edge
  edges <- matrix(nrow = 0, ncol = 2)
  #Vector of edge colors
  edgecolors <- character(0)
  #Data frame with positions of vertices
  verticesDF <- data.frame(x=numeric(0),y=numeric(0))
  #vector with sequence of vertices in the walk
  walkList <- numeric(0)

#Generate a graph with the specified number of vertices
  observeEvent(input$generate,{
    edges <<- Euler.chooseEdges(input$vertex)
    edgecolors <<- rep("blue",nrow(edges))   #all blue to start
    verticesDF <<- Euler.placeVertices(input$vertex)
    output$graph <- renderPlot(Euler.displayGraph(verticesDF,edges,edgecolors))
    walkList <<-numeric(0)
    output$message <- renderUI(h3("Click on vertices to do your Euler walk. If there are red vertices of odd degree, choose one as the starting vertex"))
  })

#React to a click by adding the nearest vertex to the walk if possible
  observeEvent(input$plot_click,{
    v <- Euler.findClosestVertex(verticesDF,input$plot_click$x,input$plot_click$y)

    #If walk list is empty, start it with this vertex
    if (length(walkList) == 0) {  
      walkList <<- v
      output$message <- renderUI(h3(v))
      return()
    }
    #Make a candidate edge from new vertex and tail of existing walk (increasing order)
    edge <- sort(c(v,tail(walkList,1)))
    k <- row.match(edge,edges,nomatch=0)
    #If there's no edge for that par of vertices, wait for another click
    if (k==0)
      return()
    #If that edge was already in the walk, return
    if (edgecolors[k] == "red")
      return()
    #Add the vertex to the walk list
    walkList <<- c(walkList,v)
  #Color the new edge red
    edgecolors[k] <<- "red"
  #Redisplay the graph
        output$graph <- renderPlot(Euler.displayGraph(verticesDF,edges,edgecolors))
  #Make a comma-separated string from the walk list
    s <- paste(walkList,sep=",",collapse = ",")
  #Display the updated walk list
    output$message <- renderUI(h3(s))
  })
}  


shinyApp(ui = ui, server = server) 
