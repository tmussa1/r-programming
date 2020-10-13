#GroupA4


library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("buttonrows.R")

ui <- dashboardPage(
    dashboardHeader(title = "Group A4, rotational symmetries of the tetrahedron",
                    titleWidth = 500),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            column(width=4,
                   box(
                       width = NULL,
                       height = 310,
                       h3 ("Elements of the group"),
                       h4("The identity"),
                       controlRow1("ctrlI"),
                       h4("Order 3 elements (face-vertex rotations through 120 degrees)"),
                       controlRow4(
                           c("ctrl123", "ctrl124","ctrl132","ctrl134")
                       ),   
                       controlRow4(
                           c("ctrl142", "ctrl143","ctrl234","ctrl243")
                       ),   
                       h4("Order 2 elements (edge-edge rotations through 180 degrees)"),
                       controlRow3(
                           c("ctrl3412", "ctrl2413", "ctrl2314")
                       ),
                   ),
                       box(
                         width = NULL,
                         height = 100,
                         title = "Subgroups",
                         buttonRow3(
                           inputIds = c("btnC2", "btnC3", "btnV4"),
                           labels = list("Show C2", "ShowC3", "ShowV4"),
                           btnStyle = "padding:4px;font-size:120%"
                         )   
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
                       ),#box
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
                    

            ),
            #col
            column(
                width = 8,
                box(
                  width = NULL,
                  height = 380,
                  fluidRow(
                    column(
                      width = 8,
                      h3("Inputs and Products"),

                      htmlOutput("results"),
                      tags$head(tags$style("#results{color:red; font-size:20px; font-style:italic; 
overflow-y:scroll; max-height: 300px; background: ghostwhite;}"))
                    ),
                    column(
                      width = 4,
                      actionBttn("reset", "Clear Inputs and Products")
                    )

                  )
                ),
                box(width = NULL,
                    height = 430,
                    tableOutput("multable")
                )
                
                
            )
        )  #fluid
    )  
)


source("a4calc.R")
source("permutecalc.R")




#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
  result <- "I"
  for (i in 1:length(v)){
    result <- Perm.multiply(result,ifelse(v[i]=="a",a,b))
  }
  return (result)
}


server <- function(input, output, session) {
  #Shared variables accessible to server()
  N <- 12
  neutral <- "gray90"
  A4DF <- makeA4data(neutral)
  #Elements in the chosen subgroup
  subgroup <- numeric(0)
  #Color for subgroup buttons
  subcolor <- "yellow"
  #Output to display in the text box
  result.list <- ""
  #Result of all multiplications so far
  product <- "I"
  
  #colors for cosets
  color.list <- c("pink","aquamarine","beige","hotpink", "violet")
  conjugating <- FALSE
  generating <- 0
  a <-"I"
  gena <- "I"
  genb <- "I"

  displayButton <- function(i) {
    renderUI({actionButton(A4DF[i,1],A4DF[i,2],
                           style=paste("padding:4px;
                   font-size:120%;background:",A4DF[i,3]))}) 
  }
  #show all the buttons
  showButtons <- function() {
    output$ctrl2413 <- displayButton(1)
    output$ctrl2314<- displayButton(2)                                     
    output$ctrl3412<- displayButton(3)
    output$ctrl234<- displayButton(4)
    output$ctrl243 <- displayButton(5)
    output$ctrl134<- displayButton(6)                                     
    output$ctrl143<- displayButton(7)
    output$ctrl124<- displayButton(8)
    output$ctrl142 <- displayButton(9)
    output$ctrl123<- displayButton(10)                                     
    output$ctrl132<- displayButton(11)
    output$ctrlI<- displayButton(12)
  }
  showButtons()
  #Display the multiplication table
  tbl <- outer(A4DF[,2],A4DF[,2],Vectorize(Perm.multiply,c("a","b")))
  colnames(tbl) <- A4DF[,2]
  rownames(tbl) <- A4DF[,2] 
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
      result.list <<- paste(result.list, line.out, "<br/>")
      output$results<-renderUI(HTML(result.list))
    }
    #Marks all elements in a subgroup with a color
    mark.subgroup <- function() {
      for (i in 1:N){
        A4DF[i,3] <<- ifelse(i  %in% subgroup,"yellow",neutral)
      }
    }
#Event handlers for all the element buttons 
    observeEvent(input$btnI,{
      compute.and.show("I")
    })
    observeEvent(input$btn123,{
       compute.and.show("(123)")
    })
    observeEvent(input$btn124,{
      compute.and.show("(124)")
    })
    observeEvent(input$btn132,{
      compute.and.show("(132)")
    })
    observeEvent(input$btn134,{
      compute.and.show("(134)")
    })
    observeEvent(input$btn142,{
      compute.and.show("(142)")
    })
    observeEvent(input$btn143,{
      compute.and.show("(143)")
    })
    observeEvent(input$btn234,{
      compute.and.show("(234)")
    })
    observeEvent(input$btn243,{
      compute.and.show("(243)")
    })
    observeEvent(input$btn3412,{
        compute.and.show("(12)(34)")
    })
    observeEvent(input$btn2413,{
      compute.and.show("(13)(24)")
    })
    observeEvent(input$btn2314,{
      compute.and.show("(14)(23)")
    })
#The reset button clears the output and reinitializes the product
    observeEvent(input$reset,{
        result.list <<- ""
        product <<- "I"
        output$results<-renderUI(HTML(result.list))
    })
#Event handlers for the subgroup buttons
    observeEvent(input$btnC2,{
      subgroup <<- c(1,12)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnC3,{
      subgroup <<- c(4,5,12)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnV4,{
      subgroup <<- c(1,2,3,12)
      mark.subgroup()
      showButtons()
    })
    #Event handler for left cosets
    observeEvent(input$btnLC,{
      mark.subgroup()
      idx = 1   #index into the color list -- one for each coset
      #Keep creating cosets as long as there are elements that are still gray
      while(length(which(A4DF$color == neutral) >0)){
        #Find the first unassigned group element
        in.coset <- which(A4DF$color == neutral)[1]
        #Generate its left coset and put a new color on the buttons
        for (j in 1:N) {
          if(j %in% subgroup) {
            element <- Perm.multiply(A4DF[in.coset,2],A4DF[j,2])
            k <- which(A4DF[,2] == element)
            A4DF[k,3] <<- color.list[idx]
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
      while(length(which(A4DF$color == neutral) >0)){
        #Find the first unassigned group element
        in.coset <- which(A4DF$color == neutral)[1]
        #Generate its left coset and put a new color on the buttons
        for (j in 1:N) {
          if(j %in% subgroup) {
            element <- Perm.multiply(A4DF[j,2],A4DF[in.coset,2])
            k <- which(A4DF[,2] == element)
            A4DF[k,3] <<- color.list[idx]
          }
        }
        idx <- idx + 1
      }
      showButtons()
    })
    
#Event handlers for buttons that change state variables
    observeEvent(input$btnclear,{
      subgroup <<- numeric(0)
      generating <<- 0
      gena <<- "I"
      genb <<- "I"
      mark.subgroup()
      showButtons()
      output$genmsg <- renderUI("")
    })
    observeEvent(input$btnmark,{
      conjugating <<- TRUE
      output$conjmsg <- renderUI("Click the button for the desired element a")
    })
    observeEvent(input$btnmarkgena,{
      generating <<- 1
      A4DF[,3] <<- rep(neutral,N)
      showButtons()
      output$genmsg <- renderUI("Click the button for generator a")
    })
    observeEvent(input$btnmarkgenb,{
      generating <<- 2
      A4DF[,3] <<- rep(neutral,N)
      showButtons()
      output$genmsg <- renderUI("Click the button for generator b")
    })
    
#Event handlers for conjugation and generation buttons
    observeEvent(input$btnconj,{
      aInv <- Perm.inverse(a)
      A4DF[,3] <<- rep(neutral,N)
      for (j in 1:N) {
        if (j %in% subgroup){
          element <- Perm.conjugate(a,A4DF[j,2])
          k <- which(A4DF[,2] == element)[1]
          A4DF[k,3] <<- "pink"
        }
      }  
      showButtons()
      output$conjmsg <- renderUI(paste0("The subgroup ",a,"H",aInv," is now pink"))
    })
    #Generate random sequences of generators.
    #If we generate more than half the group, it's the entire group
    #This algorithm could turn out to be inefficient,and in principle it can fail
    observeEvent(input$btngen,{
      subgroup <<- numeric(0)
      output$genmsg <- renderUI("Generating")
      for (j in 1:(4*N)) {
        v <- sample(c("a","b"),sample(7:10,1),replace = TRUE)
        element <- evaluate(v,gena,genb)
        k <- which(A4DF[,2] == element)[1]
        if(!(k %in% subgroup)){
          subgroup <<- c(subgroup,k)
          A4DF[k,3] <<- "yellow"
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
}



# Run the application 
shinyApp(ui = ui, server = server)



