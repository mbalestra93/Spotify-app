library(data.table)
library(igraph)
library(ggplot2)
library(ggvis)
library(shiny)
library(leaflet)
library(timeDate)
library(shinythemes)
library(shinyjs)

ui <- fluidPage(title = "Spotify Shiny app", 
                theme = shinytheme("slate"),
                # Title panel
                navbarPage(title = div(img(src = "logo.png", 
                                           height = 60, 
                                           width = 40, 
                                           style = "padding-bottom:10px"), 
                                       "Group 12 - Spotify Top 10"),
                           # Tab 1
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
                                                    label = "Choose the number of highest scores that you want to see:", 
                                                    min = 1, 
                                                    max = 10, 
                                                    value = 5)
                                      )
                                    ),
                                    
                                    # Table
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
                           
                           # Tab 2
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
                                    
                                    # Table
                                    mainPanel(   
                                      titlePanel("Spotify Top 10 by Day by Country!"),
                                      p("Use the filters to see the top 10 most listened songs 
                                        on Spotify in a specified country and on a specified day."),
                                      
                                      h5(textOutput("text.top10.intro")),
                                      DT::dataTableOutput("country")
                                      )
                           ),
                           
                           # Tab 3
                           tabPanel("Countries", htmlOutput("basic.network"),
                                    
                                    #Sidebar
                                    sidebarPanel(
                                      wellPanel(
                                        selectInput("country.selector", 
                                                    "Select a Country:", 
                                                    sort(unique(dt.spotify$Region)),
                                                    selected = "Argentina"
                                        ),
                                        
                                        sliderInput("time.selector", 
                                                    "Select time period:", 
                                                    min = as.Date("2017-01-01"),
                                                    max = as.Date("2017-12-31"), 
                                                    value = c(as.Date("2017-01-01"), 
                                                              as.Date("2017-12-31")
                                                    ), 
                                                    timeFormat = "%b %Y"
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
                                      p("Below you can find a map with a network of the country that you select. 
                                        The two countries are shown as connected if those countries listen to at
                                        least one same artist.
                                        On the left you can select the minimum number of artists that the countries 
                                        need to have in common to be shown as connected on the map. Moreover, you can 
                                        select the time frame over which the calculations are performed.
                                        Therefore, you can see how the network changes over the selected
                                        time (e.g. 2 months) for the same country and whether the musical neighbours
                                        stay the same.
                                        Furthermore, it is possible to display calculations based on 
                                        different amounts of the top songs. 
                                        For instance, the app can look at only the top 3 artists 
                                        in each country and find the corresponding connections."
                                      ),
                                      # Map
                                      leafletOutput("network.map", 
                                                    width = 1000, 
                                                    height = 600),
                                      
                                      uiOutput("myConditionalPanel"),
                                      
                                      h4("Exemplary analysis"),
                                      p("By setting parameters to Argentina, time frame to Jan17-Feb17, min number
                                        of connections to 4 and number of top artists to 10 we can see that the 
                                        map plots connections only to the Central and South America countries
                                        and to Spain. It shows that the Hispanic countries tend to listen to
                                        the similar music. What is interesting, the United States of America 
                                        are not connected to Argentina for this set of parameters. 
                                        They become connected only if we lower the number of required minimum 
                                        connections to one, which means that during this time period Argentina
                                        and USA listened to only one same artist.")
                                      )
                                    
                                    ),
                           
                           
                           # Tab 4
                           tabPanel("Descriptives", htmlOutput("descriptives"),
                                    sidebarPanel(
                                      wellPanel(
                                        selectInput("country.selector.2", 
                                                    "Select a Country:", 
                                                    sort(unique(dt.spotify$Region)),
                                                    selected = "Argentina"
                                        ),
                                        sliderInput("time.selector.2", 
                                                    "Select time period:", 
                                                    min = as.Date("2017-01-01"),
                                                    max = as.Date("2017-12-31"), 
                                                    value = c(as.Date("2017-01-01"), as.Date("2017-12-31")), 
                                                    timeFormat = "%b %Y"
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
