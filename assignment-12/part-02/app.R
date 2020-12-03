#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("eulergraph.R")
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
header <- dashboardHeader(title = "Petersen's graph",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=3,
      tableOutput("vertex"),
      tableOutput("edge")
    ),
    column(width=3,
           radioButtons("choosecolorradio", "Pick a color for higlighting to have no two edges have the same color",
                        choiceNames = c("Gray", "Beige", "Pink"),
                        choiceValues = c("gray", "beige", "pink")),
           actionBttn("choosecolorbtn","Pick a Color for Highlighting"),
           br(),
           br(),
           br(),
           actionBttn("hamiltonianbtn","Perform Hamiltonian Walk"),
           br(),
           br(),
           br(),
           actionBttn("eulerianbtn","Perform Eulerian Walk"),
           br(),
           br(),
           br(),
           actionBttn("resetbtn","Clear"),
    ),
    column(width = 6,
      plotOutput("plot1", height = 700, click="plot_click")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other 

makeVertexDF <- function() {
  DF <- data.frame(V = character(10), x = numeric (10), y = numeric(10), bg = character(10))
  DF$V <- c("RGP","RYB","YGP","RGB","YBP","YGB","BGP","RBP","RYP","RYG")
  DF$x[1:5] <- 2*sin((0:4)*2*pi/5)
  DF$y[1:5] <- 2*cos((0:4)*2*pi/5)
  DF$x[6:10] <- sin((0:4)*2*pi/5)
  DF$y[6:10] <- cos((0:4)*2*pi/5)
  DF$bg <- rep("white",10)
  return(DF)
}




makeEdgeDF <- function() {
  DF <- data.frame(V1 = character(20), V2 = character(20), color = character(20))
  DF$V1 <- c("RGP","RYB","YGP","RGB","YBP","RGP","RYB","YGP","RGB","YBP","RYP","RYG","YGB","BGP","RBP", "RGP", "YBP", "RGB", "YGP", "RYB")
  DF$V2 <- c("RYB","YGP","RGB","YBP","RGP","YGB","BGP","RBP","RYP","RYG","YGB","BGP","RBP","RYP","RYG", "RYG", "RYP", "RBP", "BGP", "YGB")
  DF$color <- c("red","orange","green","blue","purple","green","blue","purple","red","orange","orange","green",
                "blue","purple","red", "black", "black", "black", "black", "black")
  return(DF)
}

server <- function(session, input, output) {
  plotVertices <- function(DF) {
    par (mar = c(0,0,0,0))
    plot(DF$x,DF$y, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), asp = 1, pch = 21, cex = 10, bg = DF$bg )
    text(DF$x,DF$y,DF$V, cex = 1.5)
  }
  plotEdges <- function(vDF,eDF) {
    for (i in 1:20){
      v1 <- eDF[i,1]
      v2 <- eDF[i,2]
      color <- eDF[i,3]
      x1 <- vDF[which.max(vDF$V == v1),2]
      y1 <- vDF[which.max(vDF$V == v1),3]
      x2 <- vDF[which.max(vDF$V == v2),2]
      y2 <- vDF[which.max(vDF$V == v2),3]
      segments(x1,y1,x2,y2,col = color, lwd = 2)
    }
  }
  
  plotEdgesNew <- function(vDF,eDF, k) {
    for (i in 1:20){
      v1 <- eDF[i,1]
      v2 <- eDF[i,2]
      color <- eDF[i,3]
      x1 <- vDF[which.max(vDF$V == v1),2]
      y1 <- vDF[which.max(vDF$V == v1),3]
      x2 <- vDF[which.max(vDF$V == v2),2]
      y2 <- vDF[which.max(vDF$V == v2),3]
      
      if(i %in% k){
        segments(x1,y1,x2,y2,col = color, lwd = 2, lty = 2)
      } else {
        segments(x1,y1,x2,y2,col = color, lwd = 2)
      }
    }
  }
  
  PeteDF <- makeVertexDF()
  edgeDF <- makeEdgeDF()
  choosenColor <- c()
  edgecolors <- rep("not seen", 20)
  performHamiltonian <- FALSE
  performEulerian <- FALSE
  walkList <- numeric(0)
  seen <- c()
  
  output$vertex <- renderTable(PeteDF)
  output$edge <- renderTable(edgeDF)
  
  output$plot1 <- renderPlot({
    plotVertices(PeteDF)
    plotEdges(PeteDF,edgeDF)
  })
  
  observeEvent(input$choosecolorbtn, {
    
    if(input$choosecolorradio == "gray"){
      
      choosenColor <<- "#808080"
      
    } else if(input$choosecolorradio == "pink"){
      
      choosenColor <<- "#eda6c4"
      
    } else if(input$choosecolorradio == "beige"){
      
      choosenColor <<- "#F5F5DC"
    }
    
  })
  
  observeEvent(input$plot_click,{
    
    v <- Euler.findClosestVertexNew(PeteDF,input$plot_click$x,input$plot_click$y)
    
    if(performHamiltonian == TRUE){
  
      PeteDF[v, 4] <<- "red"
      
      output$plot1 <- renderPlot({
        plotVertices(PeteDF)
        plotEdges(PeteDF,edgeDF)
      })
      
      
    } else if(performEulerian == TRUE){
      
      if (length(walkList) == 0) {  
        walkList <<- v
        return()
      }
 
      
      edge <- sort(c(v,tail(walkList,1)))
      
      len <- length(edgeDF$V1)
      
      k <<- 0
      
      for(e in (1:len)){

        if((edgeDF$V1[e] == PeteDF$V[v] && edgeDF$V2[e] == PeteDF$V[tail(walkList,1)]) ||
           (edgeDF$V2[e] == PeteDF$V[v] && edgeDF$V1[e] == PeteDF$V[tail(walkList,1)])){
          k <<- e
        }
        
      }


      if (k==0)
        return()
      

      if (edgecolors[k] == "seen")
        return()

      walkList <<- c(walkList,v)

      edgecolors[k] <<- "seen"
      
      seen <<- c(seen, k)
      
      output$plot1 <- renderPlot({
        plotVertices(PeteDF)
        plotEdgesNew(PeteDF,edgeDF, seen)
      })
    
    } else {
      
      PeteDF[v, 4] <<- choosenColor
      
      output$plot1 <- renderPlot({
        plotVertices(PeteDF)
        plotEdges(PeteDF,edgeDF)
      })
      
    }
    

  })
  
  observeEvent(input$hamiltonianbtn, {
    performHamiltonian <<- TRUE
  })
  
  
  observeEvent(input$eulerianbtn, {
    performEulerian <<- TRUE
  })
  
  observeEvent(input$resetbtn, {
    
    PeteDF$bg <<- "white"
    
    performHamiltonian <<- FALSE
    performEulerian <<- FALSE
    
    output$plot1 <- renderPlot({
      plotVertices(PeteDF)
      plotEdges(PeteDF,edgeDF)
    })
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)