#JaxGuide
#August 26, 2020
#Can be used as a prototype for other XXXGuide apps
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)     #for onclick()

#source("jaxmat.R")   #for displaying mathematics


stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    pre{
      font-size:125%;
    }
  ')
))
snippets <- c(
  'output$info <- renderText({  paste0("x=", input$plot_click$x,  "\n y=", input$plot_click$y)}) ',
  
 'output$info <- renderText({    paste0("x=", input$plot_dblclick$x,"\n y=", input$plot_dblclick$y) })',
 'output$info <- renderText({   paste0("x=", input$plot_hover$x,           "\n y=", input$plot_hover$y)})',
 'observeEvent(input$plot_click,
               {vals$mtc <- rbind(vals$mtc,data.frame(wt = input$plot_click$x, mpg = input$plot_click$y))
   })',
 'output$info <- renderText({paste0("xmin=", input$plot_brush$xmin,"   xmax=", input$plot_brush$xmax,
           "\n ymin=", input$plot_brush$ymin, "   ymax=", input$plot_brush$ymax)  })    ',

 'output$info <- renderPrint({
   rows <- brushedPoints(mtcars, input$plot_brush, xvar = "wt", yvar = "mpg")
    cat("Points within the brush:\n");  print(rows)  }) ',
 'output$info <- renderPrint({
    rows <- nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg",
       threshold = 30, maxpoints = 3)
    cat("Nearest 3 points within 30 pixels:\n")
    print(rows)
  }) '


  
)

#The user interface
header <- dashboardHeader(title = "Click Guide based on Winston Chang",
                          titleWidth = 600)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    useShinyjs(),  #needed for onclick()
    fluidRow(stylesheet,
             column(width=7,
                    pre('plotOutput("plot1", click = "plot_click", 
                        dblclick = dblclickOpts(id = "plot_dblclick", delay = 200),
                        hover = hoverOpts(id = "plot_hover", delay = 500),
                        brush = brushOpts(id = "plot_brush"),
                        width = 600)',style = "background-color:white"),
                    pre('  vals <- reactiveValues(mtc = mtcars[,c("wt","mpg")])
  output$plot1 <- renderPlot({plot(vals$mtc$wt, vals$mtc$mpg)})
  output$zoom <- renderPlot({plot(vals$mtc$wt, vals$mtc$mpg,xlim = c(input$plot_brush$xmin,input$plot_brush$xmax),
         ylim = c(input$plot_brush$ymin,input$plot_brush$ymax))})',style = "background-color:white"),
                    pre(snippets[1],id = "snip1"),
                    pre(snippets[2],id = "snip2"),
                    pre(snippets[3],id = "snip3"),
                    pre(snippets[4],id = "snip4"),
                    pre(snippets[5],id = "snip5"),
                    pre(snippets[6],id = "snip6"),
                    pre(snippets[7],id = "snip7")

             ),
             column(width=5,
                box(width = NULL,
                  plotOutput("plot1", click = "plot_click", 
                             dblclick = dblclickOpts(id = "plot_dblclick", delay = 200),
                             hover = hoverOpts(id = "plot_hover", delay = 500),
                             brush = brushOpts(id = "plot_brush"),
                             width = 600)
                ),
                verbatimTextOutput("info"),
                plotOutput("zoom",width = 600)
            
                  
             )
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


server <- function(session, input, output) {
  vals <- reactiveValues(mtc = mtcars[,c("wt","mpg")])
  output$plot1 <- renderPlot({plot(vals$mtc$wt, vals$mtc$mpg)})
  output$zoom <- renderPlot({plot(vals$mtc$wt, vals$mtc$mpg,xlim = c(input$plot_brush$xmin,input$plot_brush$xmax),
                                 ylim = c(input$plot_brush$ymin,input$plot_brush$ymax))})
 


    onclick("snip1",{
        eval(parse(text=snippets[1]))
    })
    onclick("snip2",{
        eval(parse(text=snippets[2]))
        
    })
    onclick("snip3",{
      eval(parse(text=snippets[3]))
        
    })
    onclick("snip4",{
      eval(parse(text=snippets[4]))
      
    })
    onclick("snip5",{
      eval(parse(text=snippets[5]))
      
    })
    onclick("snip6",{
      eval(parse(text=snippets[6]))
      
    })
    onclick("snip7",{
      eval(parse(text=snippets[7]))
      
    })
    onclick("snip8",{
      eval(parse(text=snippets[8]))
      
    })
    onclick("snip9",{
      eval(parse(text=snippets[9]))
      
    })
    onclick("snip10",{
      output$result <- renderUI(eval(parse(text=snippets[10])))
      
    })
    onclick("snip11",{
      output$result <- renderUI(eval(parse(text=snippets[11])))
      
    })
    onclick("snip12",{
      output$result <- renderUI(eval(parse(text=snippets[12])))
    })
}

#Run the app
shinyApp(ui = ui, server = server)