server <- function(input, output) {
  
  
  # Tab 1: Summary of the database -------------------------------------------------------
  
  # This tab contains the numerous top-lists (e.g. artist, song) of the data. 
  # The user can select how many items are shown for every top list (e.g. top 5)
  
  output$print <- renderUI({
    str1 <- paste("The dataset contains the top 10 listened 
                  songs on Spotify in multiple countries per day in 2017.")
    
    str2 <- paste("The dataset contains: 
                  <ul> <li>", dt.spotify[, length(unique(Artist))], "different artists, </li> 
                  <li>", dt.spotify[, length(unique(Track.Name))], "different songs, </li> 
                  <li>", dt.spotify[, length(unique(Region))], "different countries and </li> 
                  <li>artists from", dt.spotify[, length(unique(ArtistCountry))], 
                  "different nationalities.</li></ul>")
    
    str3 <- paste("Use the filter to play around and see which countries 
                  listened to most different artists and which artists 
                  were in the top 10 with most songs.")
    
    # This allows us to use multiple paragraphs, instead of 1 block of text. 
    # Every block of text is followed by 2 breaklines and a non-breaking space between them. 
    
    HTML(paste(str1, str2, str3, sep = "<br/> &nbsp; <br/>"))
    
  })
  
  output$text.top5 <- renderText({paste0("Check out those top", 
                                        input$Number, "'s!")
  })
  
  #Table 1: Top x countries with the highest diversity of artists -----------------
  
  output$text.countries <- renderText({paste("Here you see the top", input$Number, 
                                             "countries that listed most artists")
  })
  
  output$top5 <- 
    DT::renderDataTable({
      dt.sf.top5 <- dt.spotify[, list("Artists_per_Country" = length(unique(Artist))), 
                               by = "Region"]
      dt.sf.top5 <- head(dt.sf.top5[order(-Artists_per_Country)], input$Number)
      
      DT::datatable(dt.sf.top5, colnames = c('Country', 'Artists per country'), rownames=TRUE,
                    options = list(deferRender = TRUE, dom = 't',
                                   initComplete = JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css(
                                                     {
                                                     'background-color': '#1ed760', 
                                                     'color': '#fff'
                                                     }
                                  );
                                                     $(this.api().table().body()).css(
                                                     { 'color': '#000000' }
                                                     );",
                                                     "}")), selection ='single') 
      
      })
  
  #Table 2: Top x artists per countries in which they've been in the Top x ----------------
  
  output$text.artist <- renderText({paste("Here you see the top", input$Number, 
                                          "artists with most countries.")
  })
  
  output$top5.artist <- 
    DT::renderDataTable({
      dt.sf.artist.top5 <- dt.spotify[, list("Countries_per_Artist" = length(unique(Region))),
                                      by = "Artist"]
      
      DT::datatable(head(dt.sf.artist.top5[order(-Countries_per_Artist)], input$Number),
                    colnames = c('Artist', 'Countries per artist'), 
                    options = list(dom = "t",
                                   initComplete = JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css(
                                                     {
                                                     'background-color': '#1ed760', 
                                                     'color': '#ffffff'
                                                     }
                                   );
                                                     $(this.api().table().body()).css(
                                                     { 'color': '#000000' }
                                                     );",
                                                      "}")
                    )
                    )
      })
  
  #Table 3: Top X artists with the higher number of songs appearing in a Top x -------------
  
  output$text.songs <- renderText({paste("Here you see the top", input$Number, 
                                         "artists with most hits.")})
  
  output$top5.songs <- 
    DT::renderDataTable({
      dt.sf.artist.songs.top5 <- dt.spotify[, list("Songs_per_Artist" = length(unique(Track.Name))),
                                            by = "Artist"]
      
      DT::datatable(head(dt.sf.artist.songs.top5[order(-Songs_per_Artist)], input$Number),
                    colnames = c('Artist', 'Songs per artist'),
                    options = list(dom = "t", 
                                   initComplete = JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css(
                                                     {
                                                     'background-color': '#1ed760', 
                                                     'color': '#ffffff'
                                                     }
                                   );
                                                     $(this.api().table().body()).css(
                                                     { 
                                                     'color': '#000000' 
                                                     }
                                                     );",
                                                    "}")
                    )
                    )
      })
  
  # Tab 2: Top 10 per date and country ----------------------------------------------------------
  
  
  output$country <- 
    DT::renderDataTable({
      DT::datatable(dt.spotify[Region == input$Region & 
                                 Date == input$date.selector, 
                               list(Position, Artist, Track.Name, Region, Date)][order(Position), ],
                    options = list(dom = "t",
                                   initComplete = JS("function(settings, json) {",
                                                     "$(this.api().table().header()).css(
                                                     {
                                                     'background-color': '#1ed760', 
                                                     'color': '#ffffff'
                                                     }
                                   );
                                                     $(this.api().table().body()).css(
                                                       { 
                                                         'color': '#000000' 
                                                       }
                                                     );",
                                                     "}")
                    )
                    )
      })
  
  
  # Tab 3: Countries Network --------------------------------------------------- 
  
  sliderMonth <- reactiveValues()
  observe({
    start_date <- as.POSIXct(input$time.selector[1], tz = "GMT")
    
    sliderMonth$start <- as.Date(timeFirstDayInMonth(start_date))
    
    end_date <- as.POSIXct(input$time.selector[2], tz = "GMT")
    
    sliderMonth$end <- as.Date(timeLastDayInMonth(end_date))
  })
  
  connections <- reactive({
    # Filtering dates from slider
    dt.spotify <- dt.spotify[Date >= sliderMonth$start & Date <= sliderMonth$end, ]
    
    # Selecting top artists
    dt.spotify <- dt.spotify[Position <= input$top.x, ]
    
    # Filtering the data table
    dt.spotify <- unique(dt.spotify[, .(Artist, Region)])
    
    # Graph preparation
    dt.unique.countries <- dt.spotify[, .(name = unique(Region), type = TRUE)]
    dt.unique.artists <- dt.spotify[, .(name = unique(Artist), type = FALSE)]
    
    # Bind tables to create vertices for the graph.
    dt.spotify.vert <- rbind(dt.unique.countries, dt.unique.artists)
    
    # Create a graph with countries connected by listening to commmon artists.
    g.countries.artists <- graph.data.frame(dt.spotify[, .(Region, Artist)],
                                            directed = FALSE,
                                            vertices = dt.spotify.vert)
    
    g.country.proj <- bipartite.projection(g.countries.artists)$proj2
    
    g.countries.connections <- delete.edges(g.country.proj,
                                            which(E(g.country.proj)$weight < input$number.of.connections))
    
    # Enable selection of country for the plot
    dt.edge <- as.data.table(ends(g.countries.connections, E(g.countries.connections)))
    dt.geo.data <- dt.edge[(V1 == input$country.selector | V2 == input$country.selector), 1:2]
    
    setnames(dt.geo.data, 
             old = c("V1", "V2"), 
             new = c("from", "to")
    )
    
    dt.geo.data <- merge(dt.geo.data, 
                         dt.world.cities[, .(country, lat, long)], 
                         by.x = 'from', 
                         by.y = 'country'
    )
    
    setnames(dt.geo.data, 
             old = c("lat", "long"), 
             new = c("from_lat", "from_long")
    )
    
    dt.geo.data <- merge(dt.geo.data, 
                         dt.world.cities[, .(country, lat, long)], 
                         by.x = 'to', 
                         by.y = 'country'
    )
    
    setnames(dt.geo.data, 
             old = c("lat", "long"), 
             new = c("to_lat", "to_long")
    )
    
    dt.geo.data <- dt.geo.data[, 3:6]
    dt.geo.data$id <- seq(1, dim(dt.geo.data)[1])
    
    df.lines <- gather(dt.geo.data, measure, val, -id) %>% group_by(id) %>%
      do(data.frame(lat=c(.[["val"]][.[["measure"]]=="from_lat"],
                          .[["val"]][.[["measure"]]=="to_lat"]),
                    long = c(.[["val"]][.[["measure"]]=="from_long"],
                             .[["val"]][.[["measure"]]=="to_long"]))) %>% as.data.frame()
  })
  
  # Render network map
  output$network.map <- renderLeaflet({
    if(nrow(connections()) == 0){
      leaflet() %>%
        setView(lat = as.numeric(dt.world.cities[country == input$country.selector, "lat"]), 
                lng = as.numeric(dt.world.cities[country == input$country.selector, "long"]), 
                zoom = 5) %>%
        
        addProviderTiles(providers$CartoDB.DarkMatter,
                         options = providerTileOptions(noWrap = FALSE)) %>%
        
        addPopups(lat = as.numeric(dt.world.cities[country == input$country.selector, "lat"]),
                  lng = as.numeric(dt.world.cities[country == input$country.selector, "long"]), 
                  popup = "Please choose different input", 
                  options = popupOptions(closeButton = TRUE))    
    } else {
      leaflet(connections()) %>%
        addProviderTiles(providers$CartoDB.DarkMatter,
                         options = providerTileOptions(noWrap = FALSE)) %>%
        addPolylines(data = connections(), 
                     lng = ~long, 
                     lat = ~lat, 
                     group = ~id, 
                     color = '#00b200', 
                     opacity = 0.5, 
                     weight = 2) %>%
        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    }
  })
  
  network.graph.stats <- reactive({
    
    # Filtering dates from slider
    dt.spotify <- dt.spotify[Date >= sliderMonth$start & Date <= sliderMonth$end, ]
    
    # Selecting top artists
    dt.spotify <- dt.spotify[Position <= input$top.x, ]
    
    # Filtering the data table
    dt.spotify <- unique(dt.spotify[, .(Artist, Region)])
    
    # Graph preparation
    dt.unique.countries <- dt.spotify[, .(name = unique(Region), type = TRUE)]
    dt.unique.artists <- dt.spotify[, .(name = unique(Artist), type = FALSE)]
    
    # Bind tables to create vertices for the graph.
    dt.spotify.vert <- rbind(dt.unique.countries, dt.unique.artists)
    
    # Create a graph with countries connected by listening to commmon artists.
    g.countries.artists <- graph.data.frame(dt.spotify[, .(Region, Artist)],
                                            directed = FALSE,
                                            vertices = dt.spotify.vert)
    
    g.country.proj <- bipartite.projection(g.countries.artists)$proj2
    
    # Filter the minimum number of connections
    g.countries.connections <- delete.edges(g.country.proj, 
                                            which(E(g.country.proj)$weight < input$number.of.connections)
    )
    
    
    results <- data.frame(measure = character(),
                          value = character()
    )
    
    # Centrality measures selection
    
    if (any(1 %in% input$centrality_measures)){
      results <- rbind(results, 
                       data.frame(measure = 'Degree Centrality',
                                  value = as.character(
                                    round(igraph::degree(g.countries.connections)[input$country.selector], 0)
                                  )
                       )
      )
    }
    
    if (any(2 %in% input$centrality_measures)){
      results <- rbind(results, 
                       data.frame(measure = 'Betweenness',
                                  value = as.character(
                                    round(igraph::betweenness(g.countries.connections)[input$country.selector], 4)
                                  )
                       )
      )
    }
    
    if (any(3 %in% input$centrality_measures)){
      results <- rbind(results, 
                       data.frame(measure = 'Closeness',
                                  value = as.character(
                                    round(igraph::closeness(g.countries.connections)[input$country.selector], 4)
                                  )
                       )
      )
    }
    
    if (any(4 %in% input$centrality_measures)){
      results <- rbind(results, 
                       data.frame(measure = 'Eigenvector',
                                  value = as.character(
                                    round(igraph::evcent(g.countries.connections)$vector[input$country.selector], 4)
                                  )
                       )
      )
    }
    
    if (any(5 %in% input$centrality_measures)){
      results <- rbind(results, 
                       data.frame(measure = 'Clustering Coefficient',
                                  value = as.character(round(igraph::transitivity(g.countries.connections,
                                                                                  type = 'local')[which(V(g.countries.connections)$name == input$country.selector)], 
                                                             2)
                                  )
                       )
      )
    }
    
    
    return(results)
  })
  
  output$table <- renderTable({
    network.graph.stats()
  })
  
  output$myConditionalPanel = renderUI({
    if(length(input$centrality_measures) > 0) {
      tableOutput("table")
    }
  })
   
  
  # Tab 4: Degree Distribution -------------------------------------------------
  
  # Create a slider output that enables to select the time range
  sliderMonth.2 <- reactiveValues()
  observe({
    start_date <- as.POSIXct(input$time.selector.2[1], tz = "GMT")
    
    sliderMonth.2$start <- as.Date(timeFirstDayInMonth(start_date))
    
    end_date <- as.POSIXct(input$time.selector.2[2], tz = "GMT")
    
    sliderMonth.2$end <- as.Date(timeLastDayInMonth(end_date))
  })
  
  # Create a reactive function to calculate the degree distribution
  degree <- reactive({
    # Add option of filteting dates from slider
    dt.spotify <- dt.spotify[Date >= sliderMonth.2$start & Date <= sliderMonth.2$end, ]
    
    # Add option of selecting top artists
    dt.spotify <- dt.spotify[Position <= input$top.x.2, ]
    
    # Add option of filtering the data table 
    dt.spotify <- unique(dt.spotify[, .(Artist, Region)])
    
    # Create a data table that contains all the degrees per artist
    degree.dist <- dt.spotify[, .(deg = .N), by = .(Artist)]
    degree.dist <- degree.dist[Artist %in% as.character(dt.spotify[Region == input$country.selector.2,
                                                                   Artist]), ]
    
  })
  
  output$degree.dist <- renderPlot({
    ggthemr("chalk", type = "outer")
    ggplot(degree(), aes(x = deg)) + 
      geom_histogram(binwidth = 1) + 
      xlab("Number of connections") + 
      ylab("Count") + 
      ggtitle("Degree Distribution")
  })
  
  descriptive.stats <- reactive({
    
    # Filtering dates from slider
    dt.spotify <- dt.spotify[Date >= sliderMonth.2$start & Date <= sliderMonth.2$end, ]
    
    # Selecting top artists
    dt.spotify <- dt.spotify[Position <= input$top.x.2, ]
    
    # Filtering the data table
    dt.spotify <- unique(dt.spotify[, .(Artist, Region)])
    
    # Graph preparation
    dt.unique.countries <- dt.spotify[, .(name = unique(Region), type = TRUE)]
    dt.unique.artists <- dt.spotify[, .(name = unique(Artist), type = FALSE)]
    
    # Bind tables to create vertices for the graph.
    dt.spotify.vert <- rbind(dt.unique.countries, dt.unique.artists)
    
    # Create a graph with countries connected by listening to commmon artists.
    g.countries.artists <- graph.data.frame(dt.spotify[, .(Region, Artist)],
                                            directed = FALSE,
                                            vertices = dt.spotify.vert)
    
    g.country.proj <- bipartite.projection(g.countries.artists)$proj2
    
    
    descriptive.results <- data.frame(measure = character(),
                          value = character()
    )
    
    # Descriptive measures selection
    
    if (any(1 %in% input$descriptive.measures)){
      descriptive.results <- rbind(descriptive.results, 
                       data.frame(measure = 'Average path length',
                                  value = as.character(
                                    round(igraph::average.path.length(g.country.proj), 0)
                                  )
                       )
      )
    }
    
    if (any(2 %in% input$descriptive.measures)){
      descriptive.results <- rbind(descriptive.results, 
                       data.frame(measure = 'Average clustering coefficient',
                                  value = as.character(
                                    round(igraph::transitivity(g.country.proj, type = "average"), 4)
                                  )
                       )
      )
    }
    
    if (any(3 %in% input$descriptive.measures)){
      descriptive.results <- rbind(descriptive.results, 
                       data.frame(measure = 'Diameter',
                                  value = as.character(
                                    round(igraph::diameter(g.country.proj), 4)
                                  )
                       )
      )
    }

    return(descriptive.results)
  })
  
  output$table <- renderTable({
    descriptive.stats()
  })
  
  output$myConditionalPanel2 = renderUI({
    if(length(input$descriptive.measures) > 0) {
      tableOutput("table")
    }
  })
  
  # Tab 5: Internationality ----------------------------------------------------

  sliderMonth.3 <- reactiveValues()
  observe({
    start_date <- as.POSIXct(input$time.selector.3[1], tz = "GMT")
    
    sliderMonth.3$start <- as.Date(timeFirstDayInMonth(start_date))
    
    end_date <- as.POSIXct(input$time.selector.3[2], tz = "GMT")
    
    sliderMonth.3$end <- as.Date(timeLastDayInMonth(end_date))
  })
  
  dt.internationality <- reactive({

    # Filtering dates from slider
    dt.spotify <- dt.spotify[Date >= sliderMonth.3$start & Date <= sliderMonth.3$end, ]
    
    # Selecting top artists
    dt.spotify <- dt.spotify[Position <= input$top.x.3, ]
    
    dt.artists.count <- unique(dt.spotify[, .(Region, Artist) ])
    dt.artists.count <- dt.artists.count[, .(artist_no = .N), by = .(Region)]
    
    dt.artists.inter <- unique(dt.spotify[Region_cd != ArtistCountry, .(Region,
                                                                        Artist,
                                                                        Region_cd,
                                                                        ArtistCountry) ])
    dt.artists.inter <- dt.artists.inter[, .(artist_int = .N), by = .(Region)]
    
    dt.internationality <- merge(dt.artists.count, dt.artists.inter, by = "Region")
    dt.internationality <- dt.internationality[, .(Region, 
                                                   number_of_artistis = artist_no,
                                                   perc_of_int_artists = round(artist_int / artist_no, 2))]
    })
  
  #plotting
  library(ggrepel)
  output$int_plot <- renderPlotly({
    ggthemr("chalk", type = "outer")
    plot <- ggplot(data = dt.internationality(),
                   aes(x = number_of_artistis, 
                       y = perc_of_int_artists, 
                       label = Region)) +
      geom_point() +
      labs(x = "Number of Different Artists per Country",
           y = "Percentage of Country's International Artists")
    ggplotly(plot) %>% config(displayModeBar = F)
  })
  

    
  # Tab 6: Hofstede ------------------------------------------------------------

  # Errors text output 
  
  output$hofstede.txt <- renderText ({
    if (input$country.selector.3 == input$country.selector.4) {
      "Error: Please change country selection by selecting two different countries."
    } else{
            if ((input$country.selector.3 == "Bolivia") |
                (input$country.selector.3 == "Paraguay") |
                (input$country.selector.4 == "Bolivia") |
                (input$country.selector.4 == "Paraguay") ){
              "Error: Please change country selection. Paraguay and Bolivia
              don't have any data in the Hofstede database."
            } else {"Note that potential missing columns are due to a lack
              of data in the Hofstede Database."}
    }
  })

  # Data Preparation 
  
  reactive_data <- reactive({
    country1 <- input$country.selector.3
    country2 <- input$country.selector.4
    return(subset(dt.hofstede, COUNTRY == country1 | COUNTRY == country2))

  })

  # Plotting 
  
  output$hofstede.plot <- renderPlot({
    ggthemr("chalk", type = "outer")
    ggplot(data = reactive_data(), aes(x = var_type, y = value, fill = COUNTRY)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Hofstede's cultural dimension", y = "Value",
           caption = "
           Note that every cultural dimension ranges from 1 to 100.
           Data Retrieved on www.hofstede-insights.com") +
      geom_text(aes(label = value), hjust = 1.6, color = "black",
                position = position_dodge(0.9), size = 3.5) +
      scale_fill_manual(values = c('#6AE368','#D3D3D3')) +
      coord_flip()
  })

  # Percentage of common artists over total number of unique artists calculation
  
  shared_percentage <-  reactive({

    artists.country1 <- unique(dt.spotify[ Region == input$country.selector.3, Artist])
    artists.country2 <- unique(dt.spotify[ Region == input$country.selector.4, Artist])

    common.artists <- length(intersect(artists.country1, artists.country2))

    percentage <- round(((common.artists /
                            ((length(artists.country1) + length(artists.country2) - common.artists)))
                         * 100), 3)

    return(percentage)
  })
  
  # Percentage Text
  
  output$similarities <- renderText({paste0(input$country.selector.3,
                                            " and ",
                                            input$country.selector.4,
                                            " have a percentage of similar artists of ",
                                            shared_percentage(),
                                            "%.")})
  
  }

