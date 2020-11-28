library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")

if (!require("pacman")) install.packages("pacman")

pacman::p_load(corrplot, magrittr, pacman, rio, tidyverse)

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

header <- dashboardHeader(title = "Data Analysis with Few Datasets and Techniques",
                          titleWidth = 500)

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(id="menu",
                                        menuItem("Personality Profiles in US States", tabName = "profile"),
                                        menuItem("Deaths from Lung Diseases", tabName = "lung"),
                                        menuItem("Deaths of Car Drivers", tabName = "car")
                            )
                            
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "profile",
            h2("Data Collected from 48 US States"),
            tags$ul(
              tags$li(HTML(paste0("<b>", "Geographical Data" , "</b>", "- common name of the states and their regions"))),
              tags$li(HTML(paste0("<b>", "Politcal Data" , "</b>", "- whether the state's governor is a Republican or a Democrat"))),
              tags$li(HTML(paste0("<b>", "Personality Data" , "</b>", "- whether the respondents are Friendly and Conventional, Relaxed and Creative or Temperamental and Uninhibited"))),
              tags$li(HTML(paste0("<b>", "Google Search Data" , "</b>", "- the frequency of some select search words on Google that we will use to predict personality traits")))
            ),
            fluidRow(stylesheet,
                     column(width = 3,
                            radioButtons("profileanalysis", "Types of Analysis",
                                         choiceNames = c("Contingency Table between US Regions and Personality Type",
                                                         "Correlation Graph between Different Google Search Words",
                                                         "Boxplot for Entrepreneurship",
                                                         "Chi-Square Test between US Regions and Personality Type"),
                                         choiceValues = c("profilecont", "profilecorr", "profileboxplot", "profilechi")),
                            actionBttn("profileanalyze","Analyze"),
                            br(),
                            br(),
                            br(),
                            selectInput("selectprofile1","Pick a Predictor to Run Correlation t Test between Search Words",
                            choices = c("instagram","facebook","retweet","entrepreneur","gdpr","privacy",
                                        "university", "mortgage", "volunteering", "museum", "scrapbook"),
                            ),
                            selectInput("selectprofile2", "Pick a Second Predictor",
                                        choices = c("instagram","facebook","retweet","entrepreneur","gdpr","privacy",
                                                    "university", "mortgage", "volunteering", "museum", "scrapbook"),
                            ),
                            actionBttn("profilecorranalyze","Run a t Test between Your Picks")
                     ),
                     column(width = 9,
                            plotOutput("profilegraph"),
                            br(),
                            br(),
                            verbatimTextOutput("profiletext")
                     )
                     
            ), 
            
            fluidRow(stylesheet,
                        column(width = 4,
                               br(),
                               br(),
                               htmlOutput("profilechiobservedtext"),
                               tableOutput("profileconttable")
                        ),
                        column(width = 4,
                               br(),
                               br(),
                               htmlOutput("profilechiexpectext"),
                               tableOutput("profilechiexpec")
                        ),
                        column(width = 4,
                               br(),
                               br(),
                               htmlOutput("profilechiresidtext"),
                               tableOutput("profilechiresid")
                        )
            )
            
    ),
    tabItem(tabName = "lung",
            h2("Monthly Deaths from Lung Diseases in the UK between 1974-1979 (72 Months)")
    ),
    tabItem(tabName = "car",
            h2("Deaths of Car Drivers in Great Britain 1969-84 (192 months)"),
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available




server <- function(session, input, output) {
 
  ########################################################### - profile Correlation 
  profileType <- "profilecont"
  factor1 <- 0
  factor2 <- 0
  
  profileDf <- import("data/StateData.xlsx") %>%
    as_tibble()
  
  profileDfSubset <- profileDf[, 11:22]
  
  observeEvent(input$profileanalysis,{
    profileType <<- input$profileanalysis
  })
  
  observeEvent(input$profilecorranalyze,{
   
    factor1 <<- input$selectprofile1
    factor2 <<- input$selectprofile2
    
    factor1 <<- profileDf %>% pull(factor1)
    factor2 <<- profileDf %>% pull(factor2)
    
    profilecorr <<- cor.test(factor1,factor2)
    
    
    output$profiletext <- renderText({paste(profilecorr, sep="\n")})
  })
  
  clearProfile <- function() {
    
    output$profileconttable <- NULL
    
    output$profiletext <- NULL
    
    output$profilegraph <- NULL
    
    output$profilechiexpec <- NULL
    
    output$profilechiresid <- NULL
    
    output$profilechiobservedtext <- NULL
    
    output$profilechiexpectext <- NULL
    
    output$profilechiresidtext <- NULL
  }
  
  ################################################## - profile COntingency
  
  profileDfContingent <- profileDf[, c(2, 3, 5)]
  
  profileDfContingent <- profileDfContingent %>%
  mutate(
    psychRegions = as.factor(psychRegions),
    psychRegions = fct_recode(psychRegions,
                              "Friendly" = "Friendly and Conventional",
                              "Relaxed" = "Relaxed and Creative",
                              "Temperamental" = "Temperamental and Uninhibited"
    )
  )
  
  profileDfContingent <- profileDfContingent %>%
    dplyr::select(region, psychRegions) %>%
    table() %>% prop.table(1) %>% 
    round(2) %>%    
    `*`(100) 
  
  observeEvent(input$profileanalyze,{
    
    if(profileType == "profilecorr"){
      
      clearProfile()
      
      output$profilegraph <- renderPlot(profileDfSubset %>%
                                          cor() %>%
                                          corrplot(
                                            type   = "upper",     
                                            diag   = F,         
                                            order  = "original",
                                            tl.col = "black",   
                                            tl.srt = 45   
                                          ))
    } else if(profileType == "profilecont"){
      
      clearProfile()
        
      output$profileconttable <- renderTable({as.data.frame.matrix(profileDfContingent)}, 
                                             include.rownames=TRUE)
      
    } else if(profileType == "profileboxplot"){
      
      clearProfile()
      
      output$profilegraph <- renderPlot(profileDf %>%
                                          dplyr::select(entrepreneur) %>%
                                          ggplot(., aes(y = entrepreneur, x = 1)) +
                                          geom_boxplot(notch = TRUE) +  # Boxplot with CI
                                          coord_flip())
    } else if(profileType == "profilechi"){
      
      profileSubset <- profileDf %>% dplyr::select(state_code, region, psychRegions) %>%
        mutate(
          psychRegions = as.factor(psychRegions),
          psychRegions = fct_recode(psychRegions,
                                    "Friendly" = "Friendly and Conventional",
                                    "Relaxed" = "Relaxed and Creative",
                                    "Temperamental" = "Temperamental and Uninhibited"
          )
        ) 
      
      profilechi <- profileSubset %>% dplyr::select(region, psychRegions) %>%
        table() %>% chisq.test()
      
      clearProfile()
      
      output$profileconttable <- renderTable({as.data.frame.matrix(profilechi$observed)},
                                             include.rownames=TRUE)
      output$profilechiexpec <- renderTable({as.data.frame.matrix(profilechi$expected)},
                                             include.rownames=TRUE)
      output$profilechiresid <- renderTable({as.data.frame.matrix(profilechi$residuals)},
                                             include.rownames=TRUE)

      output$profilechiobservedtext <- renderText({paste("<b>", "Observed", "</b>")})
      output$profilechiexpectext <- renderText({paste("<b>", "Expected", "</b>")})
      output$profilechiresidtext <- renderText({paste("<b>", "Residuals", "</b>")})
      
      output$profiletext <- renderText({paste(profilechi, sep="\n")})
    }
  })
}


shinyApp(ui = ui, server = server)