#StatsGuide
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)     #data for Chihara and Hesterberg
library(shinyjs)     #for onclick()

#source("jaxmat.R")   #for displaying mathematics
#Static data to display
dset <- subset(Girls2004, State == "WY", select = c("Weight","Gestation"))



stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    pre {
      font-size: 130%
    }
  ')
))
snippets <- c(
   'mean(dset$Weight)',
  'median(dset$Weight)',
  'mean((dset$Weight-mean(dset$Weight))^2)  #sample variance (definition)',
  'mean(dset$Weight^2)-(mean(dset$Weight))^2  #sample variance (calculation)',
  'var(dset$Weight)   #population variance',
  'sd(dset$Weight)   #opoulation standard deviation',
  'mean(dset$Weight > median(dset$Weight))  #fraction above median ',
  'mean(abs(dset$Weight-mean(dset$Weight)) < sd(dset$Weight))  #usually about 2/3 ',
  'summary(dset$Weight) #includes the mean',
  'quantile(dset$Weight)  #without the mean',
  'min(dset$Weight)   #minimum',
  'sum(dset$Gestation < 40)   #how many in a subset ',
  'mean(dset$Weight[which(dset$Gestation < 40)])  #mean for a subset',
  'mean(dset$Weight[-which(dset$Gestation < 40)]) #mean outside a subset',

  'max(dset$Weight[which(dset$Gestation == 40)])  #maximum of a subset ',
  'dset$Gestation[which.max(dset$Weight)]   #property of a maximum row '
)

#The user interface
header <- dashboardHeader(title = "Statistics Guide",
                          titleWidth = 600)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    useShinyjs(),  #needed for onclick()
    fluidRow(stylesheet,
      column(width = 2,
        tableOutput("tbl")
      ),
             column(width=6,
                    pre(snippets[1],id = "snip1"),
                    pre(snippets[2],id = "snip2"),
                    pre(snippets[3],id = "snip3"),
                    pre(snippets[4],id = "snip4"),
                    pre(snippets[5],id = "snip5"),
                    pre(snippets[6],id = "snip6"),
                    pre(snippets[7],id = "snip7"),
                    pre(snippets[8],id = "snip8"),
                    pre(snippets[9],id = "snip9"),
                    pre(snippets[10],id = "snip10"),
                    pre(snippets[11],id = "snip11"),
                    pre(snippets[12],id = "snip12"),
                    pre(snippets[13],id = "snip13"),
                    pre(snippets[14],id = "snip14"),
                    pre(snippets[15],id = "snip15"),
                    pre(snippets[16],id = "snip16")
             ),
             column(width=4,
                box(width = NULL,
                  uiOutput("result")
                ),
                plotOutput("plot"),
                plotOutput("barplot")
                  
             )
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

eval(parse(text=snippets[3]))

server <- function(session, input, output) {
    output$tbl <- renderTable(dset)
    output$plot <- renderPlot(hist(dset$Weight))
    output$barplot <- renderPlot(barplot(table(dset$Gestation)))

    onclick("snip1",{
        output$result <- renderUI(as.character(eval(parse(text=snippets[1]))))
    })
    onclick("snip2",{
            output$result <- renderUI(as.character(eval(parse(text=snippets[2]))))
        
    })
    onclick("snip3",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[3]))))
        
    })
    onclick("snip4",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[4]))))
      
    })
    onclick("snip5",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[5]))))
      
    })
    onclick("snip6",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[6]))))
      
    })
    onclick("snip7",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[7]))))
      
    })
    onclick("snip8",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[8]))))
      
    })
    onclick("snip9",{
      output$result <- renderUI(paste(eval(parse(text=snippets[9])),collapse= ","))
      
    })
    onclick("snip10",{
      output$result <- renderUI(paste(eval(parse(text=snippets[10])),collapse= ","))
      
    })
    onclick("snip11",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[11]))))
      
    })
    onclick("snip12",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[12]))))
    })
    onclick("snip13",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[13]))))
      
    })
    onclick("snip14",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[14]))))
      
    })
    onclick("snip15",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[15]))))
      
    })
    onclick("snip16",{
      output$result <- renderUI(as.character(eval(parse(text=snippets[16]))))
    })
}

#Run the app
shinyApp(ui = ui, server = server)