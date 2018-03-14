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
      ),
      
      tabPanel("Internationality", htmlOutput("international"),
               sidebarPanel(
                 wellPanel(
                   selectInput("country.selector.5", "Select a Country to highlight:", sort(unique(dt.spotify$Region)),
                               selected="Argentina"),
                   sliderInput("time.selector.3", "Select time period:", min = as.Date("2017-01-01"),
                               max = as.Date("2017-12-31"), 
                               value = c(as.Date("2017-01-01"), as.Date("2017-12-31")), 
                               timeFormat="%b %Y"),
                   sliderInput("top.x.3", 
                               "Select number of top x artists from daily charts:", 
                               min = 1, max = 10, value = 10)
                 )),
               
               mainPanel(
                 titlePanel("Internationality and Diversity"),
                 p("Hey, look at me! I'm in a shiny app! All the other pieces of text I know will be jelous!"),
                 plotOutput("g.point",  click = "plot_click",
                            dblclick = dblclickOpts(
                              id = "plot_dblclick")),
                 verbatimTextOutput("click_info"),
                 
                 p("Explanations")
               )),
      
      
      tabPanel("Hofstede", htmlOutput("hostede"),
               sidebarPanel(
                 wellPanel(
                   selectInput("country.selector.3", "Select a Country:", sort(unique(dt.spotify$Region)),
                               selected="Argentina"),
                   selectInput("country.selector.4", "Select a Country:", sort(unique(dt.spotify$Region)),
                               selected="Austria")
                 )),
               mainPanel(
                 titlePanel("Hofstede Cultural Dimensions"),
                 p("Professor Geert Hofstede conducted one of the most comprehensive
                   studies on cultural values of each country. He defines culture as
                   "the collective programming of the mind distinguishing the members
                   of one group or category of people from others"."),
                 p("In order to explore whether music preferences could somehow mirror
                   the cultural difference we plotted the 6 Hofstede dimensions and then
                   controlled for the number of artists that the two countries have in common."),
                 bsCollapse(id = "collapseExample",
                            bsCollapsePanel("Hofstede 6 Dimensions Definition ", 
                                            p("POWER DISTANCE INDEX (PDI)"),
                                            p("This dimension expresses the degree to 
                                              which the less powerful members of a society
                                              accept and expect that power is distributed
                                              unequally. The fundamental issue here is how
                                              a society handles inequalities among people.
                                              People in societies exhibiting a large degree
                                              of Power Distance accept a hierarchical order
                                              in which everybody has a place and which needs
                                              no further justification. In societies with low
                                              Power Distance, people strive to equalise the
                                              distribution of power and demand justification
                                              for inequalities of power."),
                                            p("INDIVIDUALISM VERSUS COLLECTIVISM (IDV)"),
                                            p("The high side of this dimension, called
                                              Individualism, can be defined as a preference
                                              for a loosely-knit social framework in which 
                                              individuals are expected to take care of only 
                                              themselves and their immediate families. Its 
                                              opposite, Collectivism, represents a preference
                                              for a tightly-knit framework in society in which
                                              individuals can expect their relatives or members
                                              of a particular ingroup to look after them in 
                                              exchange for unquestioning loyalty. A society's 
                                              position on this dimension is reflected in whether
                                              people's self-image is defined in terms of "I" or "we.""),
                                            p("MASCULINITY VERSUS FEMININITY (MAS)"),
                                            p("The Masculinity side of this dimension represents
                                              a preference in society for achievement, 
                                              heroism, assertiveness, and material rewards
                                              for success. Society at large is more competitive.
                                              Its opposite, Femininity, stands for a preference
                                              for cooperation, modesty, caring for the weak
                                              and quality of life. Society at large is more 
                                              consensus-oriented. In the business context 
                                              Masculinity versus Femininity is sometimes also
                                              related to as "tough versus tender" cultures."),
                                            p("UNCERTAINTY AVOIDANCE INDEX (UAI)"),
                                            p("The Uncertainty Avoidance dimension expresses
                                              the degree to which the members of a society 
                                              feel uncomfortable with uncertainty and ambiguity.
                                              The fundamental issue here is how a society deals 
                                              with the fact that the future can never be known: 
                                              should we try to control the future or just let it
                                              happen? Countries exhibiting strong UAI maintain 
                                              rigid codes of belief and behaviour, and are 
                                              intolerant of unorthodox behaviour and ideas. Weak
                                              UAI societies maintain a more relaxed attitude in 
                                              which practice counts more than principles."),
                                            p("LONG TERM ORIENTATION VERSUS SHORT TERM 
                                              NORMATIVE ORIENTATION (LTO)"),
                                            p("Every society has to maintain some links
                                              with its own past while dealing with the 
                                              challenges of the present and the future. 
                                              Societies prioritize these two existential 
                                              goals differently. Societies who score low on
                                              this dimension, for example, prefer to maintain
                                              time-honoured traditions and norms while viewing
                                              societal change with suspicion. Those with
                                              a culture which scores high, on the other hand,
                                              take a more pragmatic approach: they encourage
                                              thrift and efforts in modern education as a way
                                              to prepare for the future. In the business context,
                                              this dimension is referred to as "(short-term) 
                                              normative versus (long-term) pragmatic" (PRA).
                                              In the academic environment, the terminology 
                                              Monumentalism versus Flexhumility is sometimes 
                                              also used."),
                                            p("INDULGENCE VERSUS RESTRAINT (IND)"),
                                            p("Indulgence stands for a society that allows
                                              relatively free gratification of basic and 
                                              natural human drives related to enjoying life
                                              and having fun. Restraint stands for a society
                                              that suppresses gratification of needs and
                                              regulates it by means of strict social norms."),
                                            style = "default")),
                 
                 plotOutput("hofstede"),
                 h5(textOutput("similarities")),
                 bsCollapsePanel("Insights on the outcome of the analysis", 
                                 p("Despite the fact that some countries such as Latvia and
                                   Lithuania are showing storng similarities according to 
                                   the Hofstede Framework and have a quite high percentage
                                   of common artists, it's the exception rather than
                                   the rule. More immediate factors, such as geographic
                                   proximity and language (e.g. the 'Spanish Block'), 
                                   are stonger triggers for similar music preferences 
                                   than more implicit cultural values such as Hofstede's
                                   ones."),
                                 style = "default")
                 
            # Here ends mainpanel()
            )
       # Here ends tabpanel()
      )
    # Here end navbarpage()
    )
  
# Here ends fluidpage()
)

# Here be dragons
# :-) 