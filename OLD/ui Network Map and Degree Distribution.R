library(data.table)
library(igraph)
library(ggplot2)
library(ggvis)
library(shiny)
library(leaflet)
library(timeDate)
library(shinythemes)
library(shinyjs)

ui <- fluidPage(theme = shinytheme("slate"),
  # Title
  titlePanel(fluidRow(
        column(1, img(height = 50, width = 50, src = "logo.png")),
        column(5, "Group 12 - Spotify Top 10"))
    ),

      tabsetPanel(
        tabPanel("Summary of the Database", htmlOutput("stat.summary")),
        tabPanel("Top 10 per date and country", htmlOutput("top.10")),
        tabPanel("Countries", htmlOutput("basic.network"),
        pageWithSidebar(
          headerPanel("Network exploratory analysis"),
          sidebarPanel(
            selectInput("country.selector", "Select a Country:", sort(unique(sf$Region)),
                        selected="Argentina"),
            sliderInput("time.selector", "Select time period:", min = as.Date("2017-01-01"),
                        max = as.Date("2017-12-31"), 
                        value = c(as.Date("2017-01-01"), as.Date("2017-12-31")), 
                        timeFormat="%b %Y"),
            sliderInput("number.of.connections", 
                        "Select minimum number of common artists between countries:", 
                        min = 1, max = 50, value = 1),
            sliderInput("top.x", 
                        "Select number of top x artists from daily charts:", 
                        min = 1, max = 10, value = 10)
          ),
        mainPanel(
                leafletOutput("network.map", width = 800, height = 600)
        )
      )
    ),
    tabPanel("Descriptives", htmlOutput("descriptives"),
    pageWithSidebar(
      headerPanel("Descriptive statistics"), 
      sidebarPanel(
        selectInput("country.selector.2", "Select a Country:", sort(unique(sf$Region)),
                    selected="Argentina"),
        sliderInput("time.selector.2", "Select time period:", min = as.Date("2017-01-01"),
                    max = as.Date("2017-12-31"), 
                    value = c(as.Date("2017-01-01"), as.Date("2017-12-31")), 
                    timeFormat="%b %Y"),
        sliderInput("number.of.connections.2", 
                    "Select minimum number of common artists between countries:", 
                    min = 1, max = 50, value = 1),
        sliderInput("top.x.2", 
                    "Select number of top x artists from daily charts:", 
                    min = 1, max = 10, value = 10),
        checkboxGroupInput("centrality.measures", label = h3("Calculate centrality measures:"), 
                           choices = list("Degree Centrality" = 1, "Betweenness" = 2, "Closeness" = 3, "Eigenvector" = 4))
        ),
      mainPanel(plotOutput("degree.distribution"),
                 tableOutput("table"),
                 hr(),
                 fluidRow(column(3, verbatimTextOutput("value =")))
)))))


