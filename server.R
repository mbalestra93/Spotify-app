library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(ggvis)



server <- function(input, output) {
  
  output$stat.summary <- renderUI({
    #create slider with renderUI
    output$sidebar <- renderUI(
      sliderInput("obs", 
                  "Number of observations:", 
                  min = 1, 
                  max = 10, 
                  value = 5)
    )
    
  })
  
  output$top.10 <- renderUI({
    #create slider with renderUI
    output$sidebar <- renderUI(
      sliderInput("obs", 
                  "Number of observations:", 
                  min = 1, 
                  max = 100, 
                  value = 50)
    )
    
  })
  output$basic.network <- renderUI({
    #create slider with renderUI
    output$sidebar <- renderUI(
      sliderInput("obs", 
                  "Number of observations:", 
                  min = 1, 
                  max = 1000, 
                  value = 500)
    )
    
  })
  output$advanced.network <- renderUI({
    #create slider with renderUI
    output$sidebar <- renderUI(
      sliderInput("obs", 
                  "Number of observations:", 
                  min = 10, 
                  max = 10000, 
                  value = 5000)
    )
    
  })
}

server
