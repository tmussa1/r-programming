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
  "Select a continent/country",
  tabPanel(
    fluidPage(fluidRow(
      column(
        selectInput("selected_continent",
                    "Select a continent",
                    choices = NULL),
        width = 4
      ),
      column(
        selectInput("selected_country",
                    "Select a country",
                    choices = NULL),
        width = 4
      )
    )),
    plotOutput("wdi_indicator_chart")
  ),
  collapsible = TRUE
))
ui <- dashboardPage(header, sidebar, body, skin = "blue")


server <- function(session, input, output) {
 
  
  covid_data <- read_excel("data/covid_data.xlsx")
  
  covid_data
  
}

shinyApp(ui = ui, server = server)