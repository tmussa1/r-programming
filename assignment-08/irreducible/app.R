#app.R for irreducible polynomials
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#The user interface
header <- dashboardHeader(title = "Searching for Irreducible Polynomials",titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(
        column(
            width = 4,
            prettyRadioButtons("prime","p (coefficients are modulo p)",c(2,3,5,7,11)),
            prettyRadioButtons("exp","n (degree of irreducible polynomial)",c(2,3)),
            actionBttn("btnfind","List Irreducible Polynomials"),
            h3(uiOutput("tally"))
        ),
        column(
            width = 4,
            h3("Irreducible polynomials"),
            h3(uiOutput("list"))
        ),
        column(
            width = 4,
            h3("Reducibility"),
            sliderInput("sqr","Coefficient of x^2",min=1,max=1,value=1,step = 1),
            sliderInput("lin","Coefficient of x",min=0,max=1,value=1,step = 1),
            sliderInput("const","Constant",min=0,max=1,value=1,step = 1),
            h3(uiOutput("poly")),
            actionBttn("btntest","Test Reducibility"),
            h3(uiOutput("fval")),
        )
        
    )
)
ui <- dashboardPage(header, sidebar, body)

#Given a polynomial as a vector of coefficients, converts it to a printable string
convertPoly <- function(coeff, carats = FALSE) {
    v <- character(0)
    n <- length(coeff)
    while(n > 0) {
        if (n > 2 && coeff[n] > 0){
            if (coeff[n] > 1) v <- c(v, coeff[n])
            v <- c(v,"x",ifelse(carats,"^","<sup>"),n-1,ifelse(carats,"","</sup>"),"+")
        }
        
        if (n== 2 && coeff[2] > 0) {
            if (coeff[2] > 1) v <- c(v, coeff[2])
            v <- c(v,"x","+")
        }
        if ((n == 1) && (length(v) == 0 || coeff[1]>0)) v <- c(v, coeff[1])
        n <- n-1
    }
    if (tail(v,1)=="+") v <- head(v,-1)
    return(paste(v,sep="",collapse=""))
}

quadPoly <- function(x, a, b, p) {
    (x^2 + a*x + b)%%p
}

cubePoly <- function(x, a, b, c, p) {
    (x^3 + a*x^2 + b*x + c)%%p
}




#Functions that read the input and modify the output and input
server <- function(session, input, output) {
    output$tally <- renderText({
        n <- as.numeric(input$exp)
        p <- as.numeric(input$prime)
        if (n==2) {
          msg <- HTML(paste0("There are ",p^2, " quadratic polynomials.<br/>",
            p," of them are squares.<br/>",
            p*(p-1)/2," of them are products.<br/>",
            "This leaves ",p*(p-1)/2," irreducible,",sep=""))
        }
        if (n==3) {
            msg <- HTML(paste0("There are ",p^3, " cubic polynomials.<br/>",
                        p," of them are cubes.<br/>",
                        p*(p-1)," of them are square times linear.<br/>",
                        p^2*(p-1)/2," of them are irreducible times linear.<br/>",
                        p*(p-1)*(p-2)/6," of them are products of three linear.<br/>",
                        "This leaves ",p*(p^2-1)/3," irreducible.",sep=""))
        }
        msg
 

        
    })
    output$poly <- renderText({
      n <- as.numeric(input$exp)
      if (n==2) 
        v <- c(input$const,input$lin,input$sqr)
      if (n==3) 
          v <- c(input$const,input$lin,input$sqr,1)
      convertPoly(v)
 

    })
    observeEvent(input$btnfind,{
        p <- as.numeric(input$prime)
        n <- as.numeric(input$exp)
        result <- character(0)
        if (n == 2) {
            for (a in 0:(p-1)){
                for (b in 0:(p-1))
                    for (x in 0:(p-1)){
                        if(quadPoly(x,a,b,p)==0) break
                        if(x == (p-1)){
                            polystr <- convertPoly(c(b,a,1))
                            result <- paste(result,polystr,sep="<br/>")
                        }
                        
                    }
                
            } 
            output$list <- renderUI(h3(HTML(result)))
         }
        if (n == 3) {
            for (a in 0:(p-1)){
                for (b in 0:(p-1))
                    for (c in 0:(p-1))
                      for (x in 0:(p-1)){
                        if(cubePoly(x,a,b,c,p)==0) break
                        if(x == (p-1)){
                            polystr <- convertPoly(c(c,b,a,1))
                            result <- paste(result,polystr,sep="<br/>")
                        }
                        
                    }
                
            } 
            output$list <- renderUI(h3(HTML(result)))

        }
        updateSliderInput(session,"sqr",min = 0, max = p-1)
        updateSliderInput(session,"lin",max = p-1)
        updateSliderInput(session,"const",max = p-1)
    })
    observeEvent(input$btntest,{
      p <- as.numeric(input$prime)
      n <- as.numeric(input$exp)  
      if (n == 2){
        msg <- character(0)
        for (x in 0:(p-1))
          msg <- paste0(msg,"p(",x,")=",quadPoly(x,input$lin,input$const,p),"<br/>",sep="")
      }
      if (n == 3){
          msg <- character(0)
          for (x in 0:(p-1))
              msg <- paste0(msg,"p(",x,")=",cubePoly(x,input$sqr,input$lin,input$const,p),"<br/>",sep="")
      }
      output$fval <- renderUI(HTML(msg))
        
    })
}

#Run the app
shinyApp(ui = ui, server = server)