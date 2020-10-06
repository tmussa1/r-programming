#GroupS3
source("buttonrows.R")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
ui <- dashboardPage(
    dashboardHeader(title = "Group S3"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            column(width=8,
                   box(
                       width = NULL,
                       height = 500,
                       h3 ("Elements of the group"),
                       h4("The identity"),
                       controlRow1(
                           "ctrlI"
                       ), 
                       h4("Order 2 elements (rotations and flips)"),
                       controlRow7(
                         c("ctrlrcubed", "ctrlfix2and5", 
                           "ctrlfix1and4", "ctrlfix3and6", 
                           "ctrlflipy", "ctrlflipz", "ctrlflipoppositez")
                       ), 
                       h4("Order 3 elements (rotations)"),
                       controlRow2(
                           c("ctrlrsquared", "ctrlrtothe4th")
                       ), 
                       h4("Order 6 elements (rotations)"),
                       controlRow2(
                         c("ctrlr", "ctrlrtothe5th")
                       )
                       
                   ),#box
                   box(
                       width = NULL,
                       height = 100,
                       title = "Subgroups",
                       buttonRow5(
                           inputIds = c("btnC3", "btnd3", "btnd32", "showv4", "showd6"),
                           labels = c("Show C3", "Show D3 1st", "Show D3 2nd", "Show V4", "Show D6"),
                           btnStyle = "padding:4px;font-size:150%"
                       )  #agb
                   )
            ),  #col
            column(
                width = 4,
                box(
                    width = NULL,
                    h3("Inputs and Products"),
                    height = 350,
                    htmlOutput("results"),
                    tags$head(tags$style("#results{color:red; font-size:20px;
                    font-style:italic; overflow-y:scroll;
                    max-height: 300px; background: ghostwhite;}")),
                ),
                box(width = NULL, height = 60,actionBttn("reset", "Clear Inputs and Products") ),
            )
        ),  #fluid
        fluidRow(
          column(width=10,
                 box(width = NULL,
                     height = 500,
                     h3("Multiplication Table"),
                     tableOutput("multable")
                 )
          )
        )
    ), #body
)

source("permutecalc.R")
source("d6calc.R")
#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
  result <- "I"
  for (i in 1:length(v)){
    result <- Perm.multiply(result,ifelse(v[i]=="a",a,b))
  }
  return (result)
}
#evaluate(c("a","b"),"(123)","(12)")



#Everything that follows involves something in the UI
server <- function(input, output, session) {
  #Global variables accessible to server()
  N <- 12
  neutral <- "gray90"
  S3DF <- makeS3data(neutral)
  #Elements in the chosen subgroup
  subgroup <- numeric(0)
  #Color for subgroup buttons
  subcolor <- "yellow"
  #Output to display in the text box
  result.list <- ""
  #Result of all multiplications so far
  product <- "I"
  
  a <-"I"

  displayButton = function(i) {
      renderUI({actionButton(S3DF[i,1],S3DF[i,2],
                 style=paste("padding:4px;
                 font-size:180%;background:",S3DF[i,3]))}) 
  }
    #show all the buttons
  showButtons <- function() {
    output$ctrlI <- displayButton(1)
    output$ctrlr<- displayButton(2)                                     
    output$ctrlrsquared <- displayButton(3)
    output$ctrlrcubed <- displayButton(4)
    output$ctrlrtothe4th <- displayButton(5)
    output$ctrlrtothe5th <- displayButton(6)
    output$ctrlfix2and5 <- displayButton(7)
    output$ctrlfix1and4 <- displayButton(8)                                     
    output$ctrlfix3and6 <- displayButton(9)
    output$ctrlflipy <- displayButton(10)
    output$ctrlflipz <- displayButton(11)
    output$ctrlflipoppositez<- displayButton(12)
  }
  showButtons()
    #Display the multiplication table
    tbl <- outer(S3DF[,2],S3DF[,2],Vectorize(Perm.multiply,c("a","b")))
    colnames(tbl) <- S3DF[,2]
    rownames(tbl) <- S3DF[,2] 
    output$multable <- renderTable(tbl,rownames = TRUE)
    #Multiplies by a specified permutation and displays all calculations so far
    compute.and.show <- function(perm){
        
        product <<- Perm.multiply(perm,product)
        line.out <- paste(perm,product,sep = "&emsp;")
        result.list <<- paste(result.list, line.out, sep = "<br/>")
        output$results<-renderUI(HTML(result.list))
    }
    #Marks all elements in a subgroup with a color
    mark.subgroup <- function() {
        for (i in 1:N){
            S3DF[i,3] <<- ifelse(i %in% subgroup,subcolor,neutral)
        }

    }
    #Event handlers for all the element buttons 
    observeEvent(input$btnI,{compute.and.show("I")})
    observeEvent(input$btnr,{compute.and.show("(123456)")})
    observeEvent(input$btnrsquared,{compute.and.show("(135)(246)")})
    observeEvent(input$btnrcubed,{compute.and.show("(14)(25)(36)")})
    observeEvent(input$btnrtothe4th,{compute.and.show("(153)(264)")})
    observeEvent(input$btnrtothe5th,{compute.and.show("(165432)")})
    observeEvent(input$btnfix2and5,{compute.and.show("(13)(46)")})
    observeEvent(input$btnfix1and4,{compute.and.show("(26)(35)")})
    observeEvent(input$btnfix3and6,{compute.and.show("(15)(24)")})
    observeEvent(input$btnflipy,{compute.and.show("(12)(36)(45)")})
    observeEvent(input$btnflipz,{compute.and.show("(14)(23)(56)")})
    observeEvent(input$btnflipoppositez,{compute.and.show("(16)(25)(34)")})
    
    #The reset button clears the output and reinitializes the product
    observeEvent(input$reset,{
        result.list <<- ""
        product <<- "I"
        output$results<-renderUI(HTML(result.list))
    })
    observeEvent(input$btnC3,{
      subgroup <<- c(1,3,5)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$showv4,{
      subgroup <<- c(1,10,11,12)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$showd6,{
      subgroup <<- c(1,2,3,4,5,6)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnd3,{
      subgroup <<- c(1,3,5,7,8,9)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnd32,{
      subgroup <<- c(1,3,5,10,11,12)
      mark.subgroup()
      showButtons()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
