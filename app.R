# Load necessary libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(shinythemes)


library(scales)

# Load COVID-19 confirmed and death data
confirmed_cases <- read.csv("time_series_covid19_confirmed_global.csv")
deaths <- read.csv("time_series_covid19_deaths_global.csv")

# Reshape confirmed cases data
confirmed_cases_long <- confirmed_cases %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "cases") %>%
  mutate(date = as.Date(sub("X", "", date), format="%m.%d.%y"))

# Reshape deaths data
deaths_long <- deaths %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "deaths") %>%
  mutate(date = as.Date(sub("X", "", date), format="%m.%d.%y"))

# Merge the two datasets
covid_data <- confirmed_cases_long %>%
  select(-Lat, -Long) %>%
  left_join(deaths_long %>% select(-Lat, -Long), by = c("Province.State", "Country.Region", "date")) %>%
  rename(province = Province.State, country = Country.Region)

# Define the User Interface (UI)
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 Global Dashboard", windowTitle = "COVID-19 Dashboard"),
  
  # Sidebar Layout
  sidebarLayout(
    sidebarPanel(
      
      # Custom Input Panel with Icons
      selectInput("country", 
                  "Select Country:",
                  choices = unique(covid_data$country),
                  selected = "United States", multiple = TRUE,
                  selectize = TRUE, 
                  width = "100%"),
      
      dateRangeInput("dateRange", 
                     "Select Date Range:",
                     start = min(covid_data$date), 
                     end = max(covid_data$date),
                     format = "yyyy-mm-dd"),
      
      actionButton("update", "Update Data", class = "btn-primary"),
      br(),
      br(),
      
      # Add some informational text or links
      h4("About this Dashboard"),
      p("This dashboard provides an interactive visualization of global COVID-19 trends."),
      p("Data is sourced from Johns Hopkins University (JHU)."),
      tags$a(href = "https://github.com/CSSEGISandData/COVID-19", "Visit JHU Repository", target = "_blank")
    ),
    
    mainPanel(
      # Tabs layout to organize content
      tabsetPanel(
        type = "tabs",
        
        # Plot for Cases Over Time
        tabPanel("Cases Over Time", 
                 plotOutput("plot_cases", height = "400px"),
                 br(),
                 p("This plot shows the trends in COVID-19 confirmed cases over time.")),
        
        # Plot for Deaths Over Time
        tabPanel("Deaths Over Time", 
                 plotOutput("plot_deaths", height = "400px"),
                 br(),
                 p("This plot shows the trends in COVID-19 deaths over time.")),
        
        # Plot for Case Fatality Rate (CFR)
        tabPanel("Case Fatality Rate", 
                 plotOutput("plot_cfr", height = "400px"),
                 br(),
                 p("This bar chart visualizes the case fatality rate by country."))
      )
    )
  ),
  
  # Include custom CSS for styles
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f6f9;
        font-family: 'Arial', sans-serif;
      }
      .navbar, .navbar-default {
        background-color: #2c3e50;
        color: white;
      }
      h2 {
        color: #e74c3c;
      }
      .sidebar {
        background-color: #34495e;
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #2980b9;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #3498db;
      }
      .tabbable .nav-tabs {
        background-color: #3498db;
        color: white;
      }
      .tabbable .nav-tabs > li > a {
        color: white;
      }
      .tabbable .nav-tabs > li > a:hover {
        color: #e74c3c;
      }
    "))
  )
)

# Define the Server Logic
server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    req(input$country, input$dateRange)
    covid_data %>%
      filter(country %in% input$country,
             date >= input$dateRange[1],
             date <= input$dateRange[2])
  })
  
  # Plot Cases Over Time
  output$plot_cases <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = date, y = cases, color = country)) +
      geom_line() +
      labs(title = "Daily COVID-19 Cases Over Time", x = "Date", y = "Cases") +
      scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#2c3e50", size = 16, face = "bold"),
        axis.title = element_text(color = "#2c3e50", size = 12),
        axis.text = element_text(color = "#7f8c8d")
      )
  })
  
  
  # Plot Deaths Over Time
  output$plot_deaths <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = date, y = deaths, color = country)) +
      geom_line() +
      labs(title = "Daily COVID-19 Deaths Over Time", x = "Date", y = "Deaths") +
      scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#2c3e50", size = 16, face = "bold"),
        axis.title = element_text(color = "#2c3e50", size = 12),
        axis.text = element_text(color = "#7f8c8d")
      )
  })
  
  
  # Plot Case Fatality Rate (CFR)
  output$plot_cfr <- renderPlot({
    req(filtered_data())
    cfr_data <- filtered_data() %>%
      group_by(country) %>%
      summarize(case_fatality_rate = (sum(deaths) / sum(cases)) * 100)
    
    ggplot(cfr_data, aes(x = reorder(country, -case_fatality_rate), y = case_fatality_rate)) +
      geom_bar(stat = "identity", fill = "#e74c3c") +
      coord_flip() +
      labs(title = "Case Fatality Rate by Country", x = "Country", y = "Case Fatality Rate (%)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "#2c3e50", size = 16, face = "bold"),
            axis.title = element_text(color = "#2c3e50", size = 12),
            axis.text = element_text(color = "#7f8c8d"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



