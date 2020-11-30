#SpanningTree
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#The user interface
header <- dashboardHeader(title = "Spanning Trees")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(
        column(width = 4,
            h2("Prim's algorithm"),
            actionBttn("btnprstart","Initialize the map"),
            actionBttn("btnprfirst","Choose a starting city"),
            h3(uiOutput("prmiles")),
            h3(uiOutput("prmsg")),
            actionBttn("btnpradd","Choose a road from red to blue"),
            h3(uiOutput("prsuccess")),
            h2("Kruskal's Algorithm"),
            actionBttn("btnkrstart","Initialize the map"),
            h3(uiOutput("krchoice")),
            actionBttn("btnkryes","Include this road"),
            actionBttn("btnkrno","Reject this road"),
            h3(uiOutput("krtotal")),
            h3(uiOutput("krmsg")),
            actionBttn("btnrlist","Show road list"),
            tags$ul(
              uiOutput("roadlist")
            )
            
        ),
        column(width = 8,
          plotOutput("map",click = "plot_click"),
          h3(uiOutput("mincost")),
          actionBttn("btnmin", "Show minimum spanning tree"),
          actionBttn("btnrand", "Create random spanning tree"),
          h3(uiOutput("randcost")),
          actionBttn("btnimp", "Improve the spanning tree"),
          h3(uiOutput("msg")),
          h3(uiOutput("road"))
          
        )
    )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
#This file must go into the same directory as app.R
source("spancalc.R")




#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  NEDF <- span.NEcities()
  roadsDF <- span.NEroads()
  select <- 0
  end1 <- 0
  end2 <- 0
  randDF <- span.NEroads()
  prim <- rep(FALSE,nrow(NEDF))
  redcity <- 0
  bluecity <- 0
  minmiles <- 0
  krindex <- 0
  krDF <- span.NEroads()
  krcount <- 0
  krmiles <- 0
  kruskal <- rep(FALSE,nrow(NEDF))
    #Initialization
  output$map <- renderPlot(span.plotmap(NEDF, roadsDF))
  minmiles <- span.treecost(span.mintree(roadsDF))
  output$mincost <- renderUI(paste0("Minimum spanning tree is ",minmiles," miles",collapse=""))

#Handle a mouse click
#Do something, display a message, and set the appropriate value of select
  observeEvent(input$plot_click,{
      if (select == 7){   #Prim - select blue city
          city <- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)
          if (prim[city] == TRUE) {
            output$prmsg <- renderUI("Choose a blue city!")
            return()
          }
          if (span.checkRoad(randDF,redcity,city) == 0) {
            output$prmsg <- renderUI("Choose a blue city connected to the red city")
            return()
          }
          bluecity <<- city
          output$prRoad <- renderUI(paste0("From ",NEDF[redcity,1],
                                             " to ",NEDF[bluecity,1],collapse=""))
          prim[bluecity] <<- TRUE
          randDF <<- span.addRoad(randDF,redcity,bluecity)
          output$map <- renderPlot(span.plotmap(NEDF,randDF,prim))
          ncity <- sum(prim)
          output$prmiles <- renderUI(paste0(ncity, " cities, ",
                                        span.treecost(randDF), " miles",collapse=""))
          if(span.treecost(randDF)==minmiles){
            output$prmsg <- renderUI("You have built the minimal spanning tree.")
            select <<- 0
            return()
          }
          if(sum(prim)==nrow(NEDF)){
            output$prmsg <- renderUI("You have built a non-minimal spanning tree.")
            select <<- 0
           return()
          }
          output$prmsg <- renderUI("You need to add more roads.")
          select <<- 0
      }
      if (select == 6){   #Prim - select red city
          city <- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)
          if (prim[city] == TRUE) {
            redcity <<- city
            output$prRoad <- renderUI(paste0("From ",NEDF[redcity,1], collapse=""))
            output$prmsg <- renderUI("Click on the blue city")
            select <<- 7
          }
          else {
            output$prmsg <- renderUI("Choose a red city!")
          }
        
        
      }
    if (select == 5){   #Prim - select starting city
      city <- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)
      prim[city] <<- TRUE
      output$map <- renderPlot(span.plotmap(NEDF, roadsDF, prim))
      output$prmsg <- renderUI("")
      select <<- 0
    }
      if (select == 4){   #remove road - second city
          end2 <<- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)
          if (span.checkRoad(randDF,end1,end2) == 0) {
            output$prmsg <- renderUI("There must be a road between the cities!")
            return()
          }
          if (span.checkRoad(randDF,end1,end2) == 1) {
            output$prmsg <- renderUI("That road is not in the tree!")
            return()
          }
          output$road <- renderUI(paste0("Removing road from ",
                                         NEDF[end1,1]," to ",NEDF[end2,1],collapse=""))
          randDF <<- span.removeRoad(randDF,end1,end2)
          output$map <- renderPlot(span.plotmap(NEDF,randDF))
          output$road <- renderUI("")
          output$msg <- renderUI("")
          output$randcost <- renderUI(paste0("Spanning tree is now ",span.treecost(randDF)," miles",collapse=""))
          select <<- 0
      }
      if (select == 3){   #remove road - first city
          end1 <<- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)
          output$road <- renderUI(paste0("Removing road from ",
                                         NEDF[end1,1],collapse=""))
          output$msg <-renderUI("Click the other end of a road to remove.")
          select <<- 4
      }
      if (select == 2){   #add road - second city
          end2 <<- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)

          if (span.checkRoad(randDF,end1,end2) == 0) {
            output$road <- renderUI(paste0("Choose a city connected to ",NEDF[end1,1],collapse=""))
            return()
          }   
           if (span.checkRoad(randDF,end1,end2) == 2) {  
            output$road <- renderUI(paste0("Choose a city connected to ",NEDF[end1,1]," not in the tree",collapse=""))
            return()
          }
          output$road <- renderUI(paste0("Adding road from ",
                                         NEDF[end1,1]," to ",NEDF[end2,1],collapse=""))
          randDF <<- span.addRoad(randDF,end1,end2)
          output$map <- renderPlot(span.plotmap(NEDF,randDF))
          output$road <- renderUI("")
          output$msg <-renderUI("Click one end of a road to remove from a cycle.")
          select <<- 3
      }
      if (select == 1){   #add road - first city
         end1 <<- span.findClosestCity(NEDF,input$plot_click$x,input$plot_click$y)
         
         output$road <- renderUI(paste0("Adding road from ",
                                        NEDF[end1,1],collapse=""))
         output$msg <-renderUI("Click the other end of a road to add.")
         select <<- 2
      }

      
  })
  
  #Functions that respond to button clicks
  #Display the minimal spanning tree
  observeEvent(input$btnmin,{
    minDF <- span.mintree(roadsDF)
    output$map <- renderPlot(span.plotmap(NEDF,minDF))
  })
  #Generate and display a random spanning tree to be improved
  observeEvent(input$btnrand,{
    randDF <<- span.randomtree(roadsDF)
    output$map <- renderPlot(span.plotmap(NEDF,randDF))
    output$randcost <- renderUI(paste0("Random spanning tree is ",span.treecost(randDF)," miles",collapse=""))
  })
  #Initiate one step of tree improvement
  observeEvent(input$btnimp,{
    output$msg <-renderUI("Click one end of a road to add.")
    select <<- 1
  })
  
#Buttons for Prim's algorithm'
  observeEvent(input$btnprstart, {
    select <<- 0
    prim <<- rep(FALSE,nrow(NEDF))     #no cities connected
    output$map <- renderPlot(span.plotmap(NEDF, roadsDF, prim))
    randDF <<- roadsDF
  })
  observeEvent(input$btnprfirst, {
    output$prmsg <- renderUI("Click on the city")
    select <<- 5
  })
  observeEvent(input$btnpradd, {
    output$prmsg <- renderUI("Click on the red city")
    select <<- 6
  })
  
#Buttons for Kruskal's algorithm (needs no mouse events)
  observeEvent(input$btnkrstart, {
    select <<- 0
    krcount <<- 0
    krmiles <<- 0
    krindex <<- 1
    krDF <<- span.NEroads()
    output$map <- renderPlot(span.plotmap(NEDF, krDF))
    end1 <- krDF[krindex,1]
    end2 <- krDF[krindex,2]
    output$krchoice <- renderUI(paste0("From ",
                                       NEDF[end1,1]," to ",NEDF[end2,1],collapse=""))
  })
  observeEvent(input$btnkryes, {
    krDF[krindex,4] <<- TRUE
    krcount <<- krcount + 1
    krmiles <<- krmiles + krDF[krindex,3]
    output$krtotal <- renderUI(paste0(krcount," roads, ",krmiles," miles.",collapse = ""))
    output$map <- renderPlot(span.plotmap(NEDF, krDF))
    if ((krmiles == minmiles)&&(krcount == nrow(NEDF)-1)) {
      output$krmsg <- renderUI("You have built the minimal spanning tree.")
      return()
    }
    krindex <<- krindex+1
    end1 <- krDF[krindex,1]
    end2 <- krDF[krindex,2]
    output$krchoice <- renderUI(paste0("From ",
                                       NEDF[end1,1]," to ",NEDF[end2,1],collapse=""))
  })
  observeEvent(input$btnkrno, {
    krindex <<- krindex+1
    end1 <- krDF[krindex,1]
    end2 <- krDF[krindex,2]
    output$krchoice <- renderUI(paste0("From ",
                                       NEDF[end1,1]," to ",NEDF[end2,1],collapse=""))
  })
  observeEvent(input$btnrlist, {
    output$roadlist <- renderUI(apply(krDF, 1,
                                      function(x) tags$li(NEDF[x['start'],1], NEDF[x['end'],1], x['dist'])))
  })
    
}

#Run the app
shinyApp(ui = ui, server = server)