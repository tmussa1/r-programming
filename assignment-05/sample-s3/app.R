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
            column(width=6,
                   box(
                       width = NULL,
                       height = 350,
                       h3 ("Elements of the group"),
                       h4("The identity"),
                       controlRow1(
                           "ctrlI"
                       ),  #agb
                       h4("Order 3 elements (rotations)"),
                       controlRow2(
                           c("ctrl123", "ctrl132")
                       ),  #agb
                       h4("Order 2 elements (flips)"),
                       controlRow3(
                           c("ctrl23", "ctrl13", "ctrl12")
                       )  #agb
                       
                   ),#box
                   box(
                       width = NULL,
                       height = 100,
                       title = "Subgroups",
                       buttonRow2(
                           inputIds = c("btnC2", "btnC3"),
                           labels = c("Show C2", "Show C3"),
                           btnStyle = "padding:4px;font-size:150%"
                       )  #agb
                   ),#box
                   box(
                       width = NULL,
                       height = 100,
                       title = "Cosets",
                       buttonRow2(
                           inputIds = c("btnLC", "btnRC"),
                           labels = list("Left Cosets", "Right Cosets"),
                           btnStyle = "padding:4px;font-size:120%"
                       )  #agb
                   ),
                   box(
                     width = NULL,
                     height = 120,
                     title = "Conjugate Subgroup",
                     buttonRow2(
                       inputIds = c("btnmark", "btnconj"),
                       labels = list("Select a", "Generate Subgroup"),
                       btnStyle = "padding:4px;font-size:120%"
                     ),  
                     h4(uiOutput("conjmsg"))
                   ),#box
                   box(
                     width = NULL,
                     height = 120,
                     title = "Generate a Subgroup",
                     buttonRow4(
                       inputIds = c("btnmarkgena", "btnmarkgenb", "btngen", "btnclear"),
                       labels = list("Generator a", "Generator b","Generate","Clear"),
                       btnStyle = "padding:4px;font-size:120%"
                     ),  
                     h4(uiOutput("genmsg"))
                   )#box
            ),  #col
            column(
                width = 6,
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

                box(width = NULL,
                    height = 350,
                    h3("Multiplication Table"),
                    tableOutput("multable")
                )
            )
        )  #fluid
    ), #body
)

source("permutecalc.R")
source("s3calc.R")
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
  N <- 6
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
  
  #Variables for cosets and conjugate subgroups
  conjugating <- FALSE
  generating <- 0
  a <-"I"
  gena <- "I"
  genb <- "I"

  color.list <- c("pink", "lightblue")   #colors for cosets

  

    displayButton = function(i) {
        renderUI({actionButton(S3DF[i,1],S3DF[i,2],
                   style=paste("padding:4px;
                   font-size:180%;background:",S3DF[i,3]))}) 
    }
    #show all the buttons
    showButtons <- function() {
      output$ctrlI <- displayButton(1)
      output$ctrl123<- displayButton(2)                                     
      output$ctrl132<- displayButton(3)
      output$ctrl23<- displayButton(4)
      output$ctrl13<- displayButton(5)
      output$ctrl12<- displayButton(6)
    }
    showButtons()
    #Display the multiplication table
    tbl <- outer(S3DF[,2],S3DF[,2],Vectorize(Perm.multiply,c("a","b")))
    colnames(tbl) <- S3DF[,2]
    rownames(tbl) <- S3DF[,2] 
    output$multable <- renderTable(tbl,rownames = TRUE)
    #Multiplies by a specified permutation and displays all calculations so far
    compute.and.show <- function(perm){
        if (conjugating) {
          a <<- perm
          output$conjmsg <- renderUI(paste0("Conjugating by element ",perm,collapse=""))
          conjugating <<- FALSE
          return()
        }
        if (generating==1) {
          gena <<- perm
          output$genmsg <- renderUI(paste0("Generating with element ",gena,collapse=""))
         return()
        }
        if (generating==2) {
          genb <<- perm
          output$genmsg <- 
            renderUI(paste0("Generating with elements ",gena," and ", genb,collapse=""))
          return()
        }
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
    observeEvent(input$btn123,{compute.and.show("(123)")})
    observeEvent(input$btn132,{compute.and.show("(132)")})
    observeEvent(input$btn12,{compute.and.show("(12)")})
    observeEvent(input$btn13,{compute.and.show("(13)")})
    observeEvent(input$btn23,{compute.and.show("(23)")})
    observeEvent(input$btnI,{compute.and.show("I")})
    #The reset button clears the output and reinitializes the product
    observeEvent(input$reset,{
        result.list <<- ""
        product <<- "I"
        output$results<-renderUI(HTML(result.list))
    })
    #Event handlers for the subgroup buttons
    observeEvent(input$btnC2,{
      subgroup <<- c(1,6)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnC3,{
      subgroup <<- c(1,2,3)
      mark.subgroup()
      showButtons()
    })
    #Event handler for left cosets
    observeEvent(input$btnLC,{
        mark.subgroup()
        idx = 1   #index into the color list -- one for each coset
        #Keep creating cosets as long as there are elements that are still gray
        while(length(which(S3DF$color == neutral) >0)){
            #Find the first unassigned group element
            in.coset <- which(S3DF$color == neutral)[1]
            #Generate its left coset and put a new color on the buttons
            for (j in 1:N) {
              if(j %in% subgroup) {
                element <- Perm.multiply(S3DF[in.coset,2],S3DF[j,2])
                k <- which(S3DF[,2] == element)[1]
                S3DF[k,3] <<- color.list[idx]
              }
            }
            idx <- idx + 1
        }
        showButtons()
    })
    #Right cosets work the same way
    observeEvent(input$btnRC,{
        mark.subgroup()
        idx = 1   #index into the color list -- one for each coset
        #Keep creating cosets as long as there are elements that are still gray
        while(length(which(S3DF$color == neutral) >0)){
            #Find the first unassigned group element
            in.coset <- which(S3DF$color == neutral)[1]
            #Generate its left coset and put a new color on the buttons
            for (j in 1:N) {
              if(j %in% subgroup) {
                  element <- Perm.multiply(S3DF[j,2],S3DF[in.coset,2])
                  k <- which(S3DF[,2] == element)[1]
                  S3DF[k,3] <<- color.list[idx]
                }
            }
            idx <- idx + 1
        }
        showButtons()
    })
    observeEvent(input$btnmark,{
      conjugating <<- TRUE
      output$conjmsg <- renderUI("Click the button for the desired element a")
    })
    observeEvent(input$btnmarkgena,{
      generating <<- 1
      S3DF[,3] <<- rep(neutral,N)
      showButtons()
      output$genmsg <- renderUI("Click the button for generator a")
    })
    observeEvent(input$btnmarkgenb,{
      generating <<- 2
      S3DF[,3] <<- rep(neutral,N)
      showButtons()
      output$genmsg <- renderUI("Click the button for generator b")
    })
#Generate random sequences of generators.
#If we generate more than half the group, it's the entire group
#This algorithm could turn out to be inefficient,and in principle it can fail
    observeEvent(input$btngen,{
      subgroup <<-  numeric(0)
      for (j in 1:(4*N)) {
        v <- sample(c("a","b"),sample(7:10,1),replace = TRUE)
        element <- evaluate(v,gena,genb)
        k <- which(S3DF[,2] == element)[1]
        if(!(k %in% subgroup)){
          subgroup <<- c(subgroup,k)
          S3DF[k,3] <<- subcolor
        }
        #If subgroup has more than N/2 elements, it's the entire group
        if (length(subgroup) > N/2){
          subgroup <<- 1:N
          break
        } 
      }  
      mark.subgroup()
      showButtons()
      output$genmsg <- 
        renderUI(paste0("The subgroup generated by ",gena," and ", genb," is now yellow"))
    })
    observeEvent(input$btnclear,{
      subgroup <<- numeric(0)
      generating <<- 0
      gena <<- "I"
      genb <<- "I"
      mark.subgroup()
      showButtons()
      output$genmsg <- renderUI("")
    })
    observeEvent(input$btnconj,{
      aInv <- Perm.inverse(a)
      S3DF[,3] <<- rep(neutral,N)
      for (j in 1:N) {
        if (j %in% subgroup){
          element <- Perm.conjugate(a,S3DF[j,2])
          k <- which(S3DF[,2] == element)[1]
          S3DF[k,3] <<- "pink"
        }
      }
      showButtons()
      output$conjmsg <- renderUI(paste0("The subgroup ",a,"H",aInv," is now pink"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
