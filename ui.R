ui <- fluidPage(title = "Spotify Shiny app",
                theme = shinytheme("slate"),
                # Title
                navbarPage(title = div(img(src = "logo.png", 
                                           height = 50, 
                                           width = 30, 
                                           style = "padding-bottom:20px"), 
                                       "Group 12 - Spotify Top 10" 
                                       ),
                           
                           # Tab 1
                           tabPanel("Summary of the Database", 
                                    htmlOutput("stat.summary"),
                                    
                                    # Sidebar          
                                    sidebarPanel(
                                      titlePanel("Dataset Description:"),
                                      verticalLayout(
                                        p(uiOutput("print"))
                                        
                                      ),
                                      wellPanel(
                                        sliderInput("Number", 
                                                    label = "Choose the number of highest scores you want to see:", 
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
                           tabPanel("Top 10 per Date and Country", 
                                    htmlOutput("top.10"),
                                    sidebarPanel(
                                      titlePanel("Use Filters:"),
                                      
                                      # Sidebar
                                      wellPanel(
                                        selectInput("Region", ("Select a Country:"), 
                                                    sort(unique(dt.spotify$Region)),
                                                    selected = "Argentina"),
                                        dateInput("date.selector", 
                                                  ("Select a Date:"),  
                                                  min = "2017-01-01", 
                                                  max = "2017-12-31", 
                                                  value = "2017-01-01", 
                                                  startview = "month"
                                        )
                                      )
                                    ),
                                    
                                    # Table
                                    mainPanel(   
                                      titlePanel("Spotify Top 10 by Day by Country"),
                                      p("Use the filters to see the top 10 most listened songs 
                                        on Spotify in a specified country and on a specified day."),
                                      
                                      h5(textOutput("text.top10.intro")),
                                      DT::dataTableOutput("country")
                                      )
                           ),
                           
                           # Tab 3
                           tabPanel("Countries", htmlOutput("basic.network"),
                                    # Sidebar
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
                                        different amounts of the top daily artists. 
                                        For instance, if you choose top 3 artists, the app takes into 
                                        consideration all the top 3 daily artists that people listened to
                                        in each day of the selected time period. E.g., it can be a maximum of 93 unique 
                                        artists for January."
                                      ),
                                      
                                      leafletOutput("network.map", 
                                                    width = 1000, 
                                                    height = 600),
                                      
                                      uiOutput("myConditionalPanel"),
                                      
                                      h4("Illustrational example"),
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
                                      ),
                                      checkboxGroupInput("descriptive.measures", 
                                                         label = h3("Calculate descriptive measures:"), 
                                                         choices = list("Average path length" = 1, 
                                                                        "Average clustering coefficient" = 2, 
                                                                        "Diameter" = 3
                                                         )
                                      )
                                    ),
                                    
                                    mainPanel(
                                      titlePanel("Degree Distribution"),
                                      p("The graph below presents the degree distribution of 
                                        the countries' artists. 
                                        It is possible to select the country, time period over
                                        which the calculations are performed and the number of 
                                        top daily artists charts that are supposed to be taken under
                                        consideration. Number of connections represent to how many
                                        countries is the selected country connected by specific
                                        artist. The counts represent the number of artists
                                        creating particular amount of connections.
                                        "),
                                      plotOutput("degree.dist"),
                                      p("Let us look at the following example. By selecting
                                        Iceland and keeping the sliders as default we can 
                                        observe that there are around 25 artists that 
                                        connect Iceland with only one other country. 
                                        However, there are also 2 artists that connect 
                                        the country with every other country in
                                        the network."),
                                      uiOutput("myConditionalPanel2")
                                      )
                                      )
                           ,

                           # Tab 5
                           tabPanel("Internationality", htmlOutput("international"),
                                    sidebarPanel(
                                      wellPanel(
                                        sliderInput("time.selector.3", 
                                                    "Select time period:", 
                                                    min = as.Date("2017-01-01"),
                                                    max = as.Date("2017-12-31"), 
                                                    value = c(as.Date("2017-01-01"), as.Date("2017-12-31")), 
                                                    timeFormat = "%b %Y"
                                        ),
                                        
                                        sliderInput("top.x.3", 
                                                    "Select number of top x artists from daily charts:", 
                                                    min = 1, 
                                                    max = 10, 
                                                    value = 10)
                                      )
                                    ),
                                    
                                    mainPanel(
                                      titlePanel("Internationality and Diversity"),
                                      p("Hey, look at me! I'm in a shiny app! All the other pieces of text I know will be jelous!"),
                                      plotlyOutput("int_plot"),
                                      p("Explanations")
                                    )),
                          # Tab 6
                           tabPanel("Hofstede", htmlOutput("hofstede.tab"),
                                    sidebarPanel(
                                      wellPanel(
                                        selectInput("country.selector.3", "Select a Country:", sort(unique(dt.spotify$Region)),
                                                    selected = "Argentina"),
                                        selectInput("country.selector.4", "Select a Country:", sort(unique(dt.spotify$Region)),
                                                    selected = "Austria")
                                      )),
                                    mainPanel(
                                      titlePanel("Hofstede Cultural Dimensions"),
                                      p('Professor Geert Hofstede conducted one of the most comprehensive
                                        studies on cultural values of each country. He defines culture as
                                        "the collective programming of the mind distinguishing the members
                                        of one group or category of people from others".'),
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
                                                                   exchange for unquestioning loyalty. A position of
                                                                   society on this dimension is reflected in whether
                                                                   people's self-image is defined in terms of I or we."),
                                                                 p("MASCULINITY VERSUS FEMININITY (MAS)"),
                                                                 p('The Masculinity side of this dimension represents
                                                                   a preference in society for achievement,
                                                                   heroism, assertiveness, and material rewards
                                                                   for success. Society at large is more competitive.
                                                                   Its opposite, Femininity, stands for a preference
                                                                   for cooperation, modesty, caring for the weak
                                                                   and quality of life. Society at large is more
                                                                   consensus-oriented. In the business context
                                                                   Masculinity versus Femininity is sometimes also
                                                                   related to as "tough versus tender" cultures.'),
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
                                                                 p('Every society has to maintain some links
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
                                                                   also used.'),
                                                                 p("INDULGENCE VERSUS RESTRAINT (IND)"),
                                                                 p("Indulgence stands for a society that allows
                                                                   relatively free gratification of basic and
                                                                   natural human drives related to enjoying life
                                                                   and having fun. Restraint stands for a society
                                                                   that suppresses gratification of needs and
                                                                   regulates it by means of strict social norms."),
                                                                 style = "default")),

                                      textOutput("hofstede.txt"),

                                      plotOutput("hofstede.plot"),
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
