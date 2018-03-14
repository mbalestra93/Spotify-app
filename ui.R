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
        navbarPage(title = div(img(src = "logo.png", 
                                   height = 60, 
                                   width = 40, 
                                   style = "padding-bottom:10px"), 
                  "Group 12 - Spotify Top 10"),
                   
        tabPanel("Summary of the Database", 
                 htmlOutput("stat.summary"),
          
                 # Sidebar          
                 sidebarPanel(
                   titlePanel("Dataset Description:"),
                   verticalLayout(
                     h4(uiOutput("print"))
                    
                   ),
                   wellPanel(
                     sliderInput("Number", 
                                 label = "Choose the Top .. you want to see:", 
                                 min = 1, 
                                 max = 10, 
                                 value = 5)
                   )
                 ),
              
                 mainPanel( 
                   h2(textOutput("text.top5"),
                     
                   h4(textOutput("text.countries")),
                   DT::dataTableOutput("top5"),
                  
                   h4(textOutput("text.artist")),
                   DT::dataTableOutput("top5.artist"), 
                  
                   h4(textOutput("text.songs")),
                   DT::dataTableOutput("top5.songs"))
                 )
        ), 
        
        tabPanel("Top 10 per date and country", 
                 htmlOutput("top.10"),
                 sidebarPanel(
                    titlePanel("Use Filters:"),
      
                    # Sidebar
                    wellPanel(
                      selectInput("Region", h5("Select a Country:"), 
                                  sort(unique(dt.spotify$Region)),
                                  selected = "Argentina"),
                      dateInput("date.selector", 
                                h5("Select a Date:"),  
                                min = "2017-01-01", 
                                max = "2017-12-31", 
                                value = "2017-01-01", 
                                startview = "month"
                               )
                    )
                 ),
    
                mainPanel(   
                  titlePanel("Spotify Top 10 by Day by Country!"),
                  p("Use the filters to see the top 10 most listened songs 
                    on Spotify in a specified country and on a specified day."),
                  
                  h5(textOutput("text.top10.intro")),
                  DT::dataTableOutput("country")
                )
        ),
  
        tabPanel("Countries", htmlOutput("basic.network"),
         
                 sidebarPanel(
                   wellPanel(
                     selectInput("country.selector", 
                                 "Select a Country:", 
                                 sort(unique(dt.spotify$Region)),
                                 selected="Argentina"
                                 ),
                      
                     sliderInput("time.selector", 
                                 "Select time period:", 
                                 min = as.Date("2017-01-01"),
                                 max = as.Date("2017-12-31"), 
                                 value = c(as.Date("2017-01-01"), 
                                           as.Date("2017-12-31")
                                          ), 
                                 timeFormat="%b %Y"
                                 ),
                     
                    sliderInput("number.of.connections", 
                                "Select minimum number of common artists between countries:", 
                                min = 1, 
                                max = 50, 
                                value = 1
                                ),
                    
                    sliderInput("top.x", 
                                "Select number of top x artists from daily charts:", 
                                min = 1, 
                                max = 10, 
                                value = 10
                                ),
                    
                    checkboxGroupInput("centrality_measures", 
                                       label = h3("Calculate centrality measures:"), 
                                       choices = list("Degree Centrality" = 1, 
                                                      "Betweenness" = 2, 
                                                      "Closeness" = 3, 
                                                      "Eigenvector" = 4, 
                                                      "Clustering Coefficient" = 5
                                                      )
                                      )
                  )
                ),
                
                mainPanel(
                  titlePanel("Network Exploratory Analysis"),
                  p("Below you can find a map with a network of your selected country. 
                    The countries are connected if those countries listen to the same 
                    artists. On the left you can select how many artists the countries 
                    need to have in common to be connected. Moreover, you can select 
                    the time period over which the app calculates the similarities. 
                    Furthermore, you can filter on the top number of songs. 
                    So for example the app can only look at the top 3 songs 
                    in each country and find the connections. "
                  ),
                  
                  leafletOutput("network.map", 
                                width = 1000, 
                                height = 600),
                  
                  uiOutput("myConditionalPanel"),
                  
                  h4("Some cool findings!"),
                  p("Hello")
                )
        
      ),
      
      
      
      tabPanel("Descriptives", htmlOutput("descriptives"),
        sidebarPanel(
          wellPanel(
            selectInput("country.selector.2", 
                        "Select a Country:", 
                        sort(unique(dt.spotify$Region)),
                        selected="Argentina"
                       ),
            sliderInput("time.selector.2", 
                        "Select time period:", 
                        min = as.Date("2017-01-01"),
                        max = as.Date("2017-12-31"), 
                        value = c(as.Date("2017-01-01"), as.Date("2017-12-31")), 
                        timeFormat="%b %Y"
                       ),
            
            sliderInput("top.x.2", 
                        "Select number of top x artists from daily charts:", 
                        min = 1, 
                        max = 10, 
                        value = 10)
                        )
          ),
        
        mainPanel(
          titlePanel("Degree Distribution"),
          p("Write a little intro"),
          plotOutput("degree.dist"),
          p("Explanations")
        )
      )
      
      
    # Here end navbarpage()
    )
  
# Here ends fluidpage()
)

# Here be dragons
# :-)