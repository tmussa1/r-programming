#Fourier
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(MASS)   #datasets to accompany Chihara and Hesterberg
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))


#The user interface
header <- dashboardHeader(title = "Fourier Analysis of Time Series ",
                          titleWidth = 500)
sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(id="menu",
                                        menuItem("Accidental deaths", tabName = "accident"),
                                        menuItem("Deaths from Lung Diseases", tabName = "lung"),
                                        menuItem("Deaths of Car Drivers", tabName = "car")
                            ),
                            sliderInput("nbasis","Basis Functions to Display", min = 0, max = 9, value = 0),
                            sliderInput("ndisplay","Coefficients to Display", min = 20, max = 100, value = 40),
                            actionButton("btncalc","Calculate Fourier Coefficients"),
                            sliderInput("nrecon","Components for reconstruction", min = 1, max = 100, value = 5)
                           
  
)
body <- dashboardBody(
  tabItems(

    tabItem(tabName = "accident",
            h2("Accidental deaths in the US between 1973-1978 (72 months)")
    ),
    tabItem(tabName = "lung",
            h2("Monthly Deaths from Lung Diseases in the UK between 1974-1979 (72 Months)")
    ),
    tabItem(tabName = "car",
            h2("Deaths of Car Drivers in Great Britain 1969-84 (192 months)"),
    )
  ),
  fluidRow(plotOutput("dataplot")),
  fluidRow(plotOutput("coefplot")),
  fluidRow(stylesheet,
    column(width = 6,
           tableOutput("tbl")  #display the data set
    ),
    column(width = 6,
          
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

source("fcalc.R")

#Additional functions are OK here, but no variables

cer <- get("accdeaths")
cer

server <- function(session, input, output) {
  havecoeff <- FALSE
  colors = c("red", "blue", "green", "orange", "purple")
  dset <- numeric(0)
  N <- 0
  coefA <- numeric(0)
  coefB <- numeric(0)
  coefabs <- numeric(0)
  datamean <- numeric(0)
  showData <- function() plot(1:N,dset,type = "l")
  observeEvent(input$menu, {
    if(input$menu == "lung") {
      dset <<- as.numeric(deaths)  #we just want a vector
      N <<- length(dset)

    }
    if(input$menu == "accident") {
      dset <<- as.numeric(accdeaths)  #we just want a vector
      N <<- length(dset)               
    }
    if(input$menu == "car"){
      dset <<- as.numeric(drivers)  #we just want a vector
      N <<- length(dset) 
    }
    havecoeff <<- FALSE
    output$coefplot  <<- NULL
    output$dataplot <- renderPlot({showData()})
  })
  observeEvent(input$btncalc,{
    f <- fa.makeCoefficients(dset)
    datamean <<- as.numeric(f$mean)
    coefA <<- f$cos
    coefB <<- f$sin
    coefabs <<- f$abs
    havecoeff <<- TRUE
    output$coefplot <- renderPlot(barplot(f$abs[1:input$ndisplay],names.arg = 1:input$ndisplay ))
  })

  observeEvent(input$nrecon,{
    if (havecoeff == FALSE) return()
    recon <- fa.reconstruct(datamean, coefA, coefB, input$nrecon)
    output$dataplot <- renderPlot({showData()
      points(1:N,recon, type = "l", col = "red")})
  })
  observeEvent(input$nbasis,{
    output$dataplot <- renderPlot({
      showData()
      n <- input$nbasis
      if (n == 0) return()
      dmean <- mean(dset)
      points(1:N,rep(dmean,N), type = "l")
      if (n == 1) return()
      for (i in 1:floor(n/2)){
        points(1:N,dmean+(dmean-min(dset))*myCos(i,N), type = "l", col = colors[i], lwd = 2)
      }
      if (n == 2) return()
      for (i in 1:floor((n-1)/2)){
        points(1:N,dmean+(dmean-min(dset))*mySin(i,N), type = "l", col = colors[i], lwd = 2, lty = 2)
      }
    })
  })
}

#Run the app
shinyApp(ui = ui, server = server)