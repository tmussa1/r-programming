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
  "Select a Continent & Country",
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
    plotOutput("covid_chart"),
    plotOutput("covid_chart_deaths")
  ),
  collapsible = TRUE
))
ui <- dashboardPage(header, sidebar, body, skin = "blue")


server <- function(session, input, output) {
 
  
  covid_data <- as_tibble(read_excel("data/covid_data.xlsx"))
  countries <- unique(covid_data %>% pull(countriesAndTerritories))
  continents <- unique(covid_data %>% pull(continentExp))
  covid_data_per_country_per_month <- covid_data %>% group_by(countriesAndTerritories, month)
  covid_data_per_country_per_month <- covid_data_per_country_per_month %>% summarise(
    cases = sum(cases),
    deaths = sum(deaths)
  )
  
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
  
 
  observeEvent(input$selected_country,{
    
    months <- c("Jaunary", "February", "March", "April", "May", 
                "June", "July", "August", "September", "October")
    
    selectedCountry <- covid_data_per_country_per_month %>% 
      filter(countriesAndTerritories == input$selected_country)

    output$covid_chart <- renderPlot(
      ggplot(data = selectedCountry,
             aes(x=factor(month), y=cases, fill=cases)) +
        geom_bar(stat="identity", position = "dodge") +
      scale_size_area() + 
      xlab("Months numbered 1 to 12") +
      ylab("Cases"))
   
    output$covid_chart_deaths <- renderPlot(
      ggplot(data = selectedCountry,
             aes(x=factor(month), y=deaths, fill=deaths)) +
        geom_bar(stat="identity", position = "dodge") +
      scale_size_area() + 
      xlab("Months numbered 1 to 12") +
      ylab("Deaths")
  )
  })

}

shinyApp(ui = ui, server = server)