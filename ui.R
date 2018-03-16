ui <- fluidPage(title = "Spotify Shiny app",
                theme = shinytheme("slate"),
                # Title
                navbarPage(title = div(img(src = "logo.png", 
                                           height = 50, 
                                           width = 30, 
                                           style = "padding-bottom:20px"), 
                                       "Group 12 - Spotify Top 10" 
                ),
                
                # Landing Page ------------------------------------------
                
                tabPanel("Landing Page", 
                         
                         # Sidebar          
                         sidebarPanel(
                           titlePanel("Group Number 12:"),
                           verticalLayout(
                             h4("Agnieszka Kloc     - 490033"),
                             h4("Esmee Peet         - 486389"),
                             h4("Laurens Jansma     - 377675"),
                             h4("Marco Fogli        - 462924")
                           )
                         ),
                         
                         # Maintab
                         mainPanel( 
                           h2("Spotify Network Analysis"),
                           h3("Scope of the Analysis"),
                           p("In order to analyze the trends, similarities and 
                             differences that the daily Top 10 playlist have around
                             the world we decided to carry out this analysis. In this
                             shiny app each tab will cover a part of the analysis that,
                             when combined with the others, it will  provide an all-around
                             overview of the trends present in Spotify's Top 10."),
                           h3("Data"),
                           p("The Spotify database, on which is based the large
                             majority of this analysis has been retrieved from 
                             Kaggle. In particular, the data has the information 
                             of the daily Top 10 most listened to songs in more 
                             than 50 different countries over ayear of observation."),
                           p("To further expand the scope of analysis we decided
                             to scrape the web in order to gather the nationality
                             of all the artists that have gotten into the top 10 at
                             least once. In order to fill gaps that the scraping tool
                             was unable to fill we searched ourselves for such information and
                             added it manually to this database."),
                           p("Finally, in order to control for any further cultural
                             factor that could influence music preferences in a country
                             compared to another one, we decided to import the Hofstede's
                             6 Dimension database for all the countries of our dataset."),
                           h3("Business utility"),
                           p("Spotify derives value from its music recommendation
                             system. Having this information on cultural and 
                             musical similarities can help both simplify and 
                             improve the playlist curating process."),
                           p("This app can also give artists insight in the
                             countries where the similarity is highest. Using 
                             this information, artists and their managers can 
                             coordinate promotional efforts in a better way, 
                             giving a higher value.")
                           
                         )
                ),
                
                # Tab 1 ------------------------------------------
                
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
                         
                         # Maintab
                         mainPanel( 
                           h2(textOutput("text.top5"),
                              
                              # Tables
                              h4(textOutput("text.countries")),
                              DT::dataTableOutput("top5"),
                              
                              h4(textOutput("text.artist")),
                              DT::dataTableOutput("top5.artist"), 
                              
                              h4(textOutput("text.songs")),
                              DT::dataTableOutput("top5.songs"))
                         )
                ), 
                
                # Tab 2 ------------------------------------------
                
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
                         
                         # Maintab
                         mainPanel(   
                           titlePanel("Spotify Top 10 by Day by Country"),
                           p("Use the filters to see the top 10 most listened songs 
                             on Spotify in a specified country and on a specified day."),
                           
                           # Table
                           h5(textOutput("text.top10.intro")),
                           DT::dataTableOutput("country")
                           )
                ),
                
                # Tab 3 ------------------------------------------
                
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
                         
                         # Maintab
                         mainPanel(
                           titlePanel("Network Exploratory Analysis"),
                           p("Below you can find a map with a network of the country that you select. 
                              Two countries are shown as connected if those countries listen to at 
                              least one common artist. 
                              On the left you can select the minimum number of artists that the countries 
                              need to have in common to be shown as connected on the map. Moreover, you can 
                              select the time frame over which the calculations are performed. Therefore, 
                              you can see how the network changes over the selected time (e.g. 2 months) for 
                              the same country and whether the musical neighbours stay the same. 
                              Furthermore, it is possible to display calculations based on different numbers 
                              of the top daily artists. For instance, if you choose the top 3 artists, 
                              the app takes into consideration all the top 3 daily artists that people 
                              listened to in each day of the selected time period. E.g., it can be a 
                              maximum of 93 unique artists for January."
                           ),
                           
                           # Table
                           leafletOutput("network.map", 
                                         width = 1000, 
                                         height = 600),
                           
                           uiOutput("myConditionalPanel"),
                           
                          
                          # Comment 
                           bsCollapsePanel("Illustrational Example",
                                           p("By setting the parameters to Argentina, time frame to Jan17-Feb17, min number
                                             of connections to 5 and number of top artists to 10, we can see that the map 
                                             plots connections only to Central and South American countries and Spain. 
                                             From this we can interpret that Hispanic countries tend to listen to the same music.
                                             What is interesting, the United States of America are not connected to Argentina for 
                                             this set of parameters. They become connected only if we lower the number of 
                                             required minimum connections to two, which means that during this specific time period 
                                             Argentina and the USA listened to only two common artists."
                                             ),
                                           style = "default"))
                         
                           ),
                
                # Tab 4  ------------------------------------------
                
                tabPanel("Descriptives", htmlOutput("descriptives"),
                         
                         # Sidebar
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
                         
                         # Maintab
                         mainPanel(
                           titlePanel("Degree Distribution"),
                           
                           #Presentation
                           p("The graph below presents the degree distribution of the countries' artists. 
                             It is possible to select a country, time period over which the calculations 
                             are performed and the number of top daily artists charts that are supposed 
                             to be taken into consideration. The x-axis displays the number of connections, 
                             which shows how a specific artists connect a country to multiple other countries. 
                             The y-axis displays the number of artists that create that specific number of 
                             connections."
                             ),
                           plotOutput("degree.dist"),
                           
                           # Comment 
                           bsCollapsePanel("Illustrational Example",
                           p("Let us look at the following example. By selecting Iceland and
                             keeping the sliders as default we can observe that there are around 
                             25 artists that connect Iceland with only one other country. 
                             However, there are also 2 artists that connect the country with 
                             every other country in the network. As we can see in our first tab 
                             this must be Ed Sheeran and Luis Fonsi as they were both in the top 10 
                             in all 52 countries."
                             ),
                           style = "default"),
                           uiOutput("myConditionalPanel2")
                           )
                           ),
                
                # Tab 5  ------------------------------------------
                tabPanel("Internationality", htmlOutput("international"),
                         
                         # sidetab
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
                         
                         # Maintab
                         mainPanel(
                           titlePanel("Internationality and Diversity"),
                           
                           #Presentation
                           p("By intersecting the artist nationality 
                             database and the Spotify database we were 
                             able to further explore the behavior of two 
                             measures, namely diversification - calculated
                             as number of different artists taking their 
                             spot in the top 10 - and the internationality - 
                             basically, the percentage of foreign artists 
                             amongst the ones in the top 10."),
                           
                           # Plot
                           plotlyOutput("int_plot"),
                           
                           #Comment
                           bsCollapse(id = "collapseExample",
                                      bsCollapsePanel("Analysis",
                                                      p("As we can see there are some trends that were unexplored
                                                        that this plot is showing. This analysis will, in particular,
                                                        be explaining the top 10 in a whole year time period."),
                                                      p("First of all, looking at the y axes, we can see that
                                                        northern European countries (such as Finland, Denmark, Sweden,
                                                        Latvia and Lithuania) are the countries that, all over the world
                                                        show a higher diversification of artists. For a newcomer artist
                                                        it appears that standing out of the crowd is easier in the
                                                        above mentioned countries, which is a factor to take into
                                                        consideration when deciding where to launch a new single."),
                                                      p("Opposite to the trend shown in the northern European countries,
                                                        the South Americans are showing a very low differentiation, with
                                                        countries like Colombia, Mexico, Perù, Bolivia, Ecuador, Argentina
                                                        showing even less that 40 different artists a year."),
                                                      p("Internationality wise, we can see how the very large majority of
                                                        the countries have over 80% of foreign artists - mostly English
                                                        speaking - having a spot in their top 10 over the year."),
                                                      p("Apart from this cluster, the 3 countries that stand out the most are
                                                        U.S.A., Brasil, Italy and France and Finland. While for the first one the decisive
                                                        factor is the fact that American artists are the ones leading the
                                                        world wide music market. For Brasil and Finland is more the fact
                                                        that in these countries there is an extremely strongly present niche
                                                        that hardly is as much popular in other countries - namely metal
                                                        and the latin music such as salsa, mambo, merengue, rumba and bachata.
                                                        For France and Italy, the national evolution led to have a strong
                                                        national pride in parallel with a very low knowledge of English.
                                                        These two factors lead to a very high presence of nationsal artists."),
                                                      p("When both the axes are taken into consideration two main clusters can be
                                                        seen. The first one is the South American one, that shows high internationality
                                                        yet very low diversification. The second one is the European one, with various 
                                                        countries showing high iternationality with medium diversification (between
                                                        60 and 80 different artists a year). The other countries are scattered around, with
                                                        small clusters that can be deligned (such as the German and nordic one having about
                                                        80 artist a year and a 70% of international artists)."),
                                                      style = "default"))
                                                      )),
                
                # Tab 6  ------------------------------------------
                
                tabPanel("Hofstede", htmlOutput("hofstede.tab"),
                         
                         #Sidetab
                         sidebarPanel(
                           wellPanel(
                             selectInput("country.selector.3", "Select a Country:", sort(unique(dt.spotify$Region)),
                                         selected = "Argentina"),
                             selectInput("country.selector.4", "Select a Country:", sort(unique(dt.spotify$Region)),
                                         selected = "Austria")
                                     )
                                      ),
                         
                         # Maintab
                         mainPanel(
                           titlePanel("Hofstede Cultural Dimensions"),
                           
                           # Presentation
                           p('Professor Geert Hofstede conducted one of the most comprehensive
                             studies on cultural values of each country. He defines culture as
                             "the collective programming of the mind distinguishing the members
                             of one group or category of people from others".'),
                           p("In order to explore whether music preferences could somehow mirror 
                              the cultural difference we plotted the 6 Hofstede dimensions and then 
                              calculated the number of artists that two countries have in common as 
                              a percentage from the total number of artists that each of them 
                              listened to."),
                           
                           # Hofstede defintion
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
                           
                           # Error Message
                           textOutput("hofstede.txt"),
                           
                           # Plot
                           plotOutput("hofstede.plot"),
                           h5(textOutput("similarities")),
                           
                           # Comment
                           bsCollapsePanel("Analysis",
                                           p("Despite the fact that some countries such as Latvia and
                                             Lithuania are showing storng similarities according to
                                             the Hofstede Framework and have a quite high percentage
                                             of common artists, it's the exception rather than
                                             the rule. More immediate factors, such as geographic
                                             proximity and language (e.g. the 'Spanish Block'),
                                             are stronger triggers for similar music preferences
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
