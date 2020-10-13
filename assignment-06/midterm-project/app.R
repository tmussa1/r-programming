library(shiny)
library(shinydashboard)
library(shinyWidgets)
library("DT")
library("tidyverse")
library("readxl")

header <- dashboardHeader(title = "Covid Data Analysis",
                          titleWidth = 500)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  navbarPage(
  "Select a Continent/Country",
  tabPanel(
    fluidPage(fluidRow(
      column(
        selectInput("selected_continent",
                    "Select a continent",
                    choices = NULL),
        width = 6
      ),
      column(
        selectInput("selected_country",
                    "Select a country",
                    choices = NULL),
        width = 6
      )
    )),
    plotOutput("covid_chart")
  ),
  collapsible = TRUE
))
ui <- dashboardPage(header, sidebar, body, skin = "blue")


server <- function(session, input, output) {
 
  
  covid_data <- as_tibble(read_excel("data/covid_data.xlsx"))
  countries <- unique(covid_data %>% pull(countriesAndTerritories))
  continents <- unique(covid_data %>% pull(continentExp))
  
  updateSelectInput(session,
                    "selected_continent",
                    choices = continents)
  
  observeEvent(input$selected_continent,
               {
                 countries_in_continent <- covid_data %>%
                   filter(continentExp == input$selected_continent) %>%
                   pull(countriesAndTerritories)

                 updateSelectInput(session,
                                   "selected_country",
                                   choices = countries_in_continent)

  })
}

shinyApp(ui = ui, server = server)