library(data.table)
library(igraph)
library(ggplot2)
library(ggvis)
library(shiny)

ui <- fluidPage(
  # Title
  titlePanel("Group 12 - Spotify Top 10"),

    # slider comes from the si object created in server.R
    sidebarLayout(
      sidebarPanel(
        uiOutput("sidebar")
      ),  
  
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of the Database", htmlOutput("stat.summary")) ,
        tabPanel("Top 10 per date and country", htmlOutput("top.10")),
        tabPanel("Network Analysis", htmlOutput("basic.network")),
        tabPanel("Advanced Analysis", htmlOutput("advanced.network"))
      )
    )
  )
)

ui
