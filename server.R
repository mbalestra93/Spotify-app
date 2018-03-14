library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(ggvis)
library(timeDate)
library(tidyr)
library(dplyr)
library(sp)
library(maptools)
library(DT)
library(countrycode)


server <- function(input, output) {
  
# Tab 1 ------------------------------------------------------------------------  
  output$print <- renderUI({
    str1 <- paste("The dataset contains the top 10 listened songs on Spotify in multiple countries per day in 2017.")
    str2 <- paste("The dataset contains: <ul> <li>", dt.spotify[, length(unique(Artist))], "different artists, </li> <li>", dt.spotify[, length(unique(Track.Name))], "different songs, </li> <li>", dt.spotify[, length(unique(Region))], "different countries and </li> <li>artists from", dt.spotify[, length(unique(ArtistCountry))], "different nationalities.</li></ul>")
    str3 <- paste("Use the filter to play around and see which countries listened to most different artists and which artists were in the top 10 with most songs.")
    
    HTML(paste(str1, str2, str3, sep = "<br/> &nbsp; <br/>"))
    
  })
    
  output$text.top5 <- renderText({paste("Check out those top", input$Number, "'s!")})
  
  output$text.countries <- renderText({paste("Here you see the top", input$Number, "countries that listed most artists")})

  output$top5 <- 
    DT::renderDataTable({
      dt.sf.top5 <- dt.spotify[, list("Artists_per_Country" = length(unique(Artist))), by = "Region"]
      DT::datatable( head(dt.sf.top5[order(-Artists_per_Country)], input$Number), options = list(dom = "t", 
    initComplete = JS("function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#1ed760', 'color': '#fff'});",
    "}")))
    })
  
  output$text.artist <- renderText({paste("Here you see the top", input$Number, "artists with most countries.")})
  
  output$top5.artist <- 
    DT::renderDataTable({
      dt.sf.artist.top5 <- dt.spotify[, list("Countries_per_Artist" = length(unique(Region))), by = "Artist"]
      DT::datatable(head(dt.sf.artist.top5[order(-Countries_per_Artist)], input$Number), options = list(dom = "t", 
     initComplete = JS("function(settings, json) {",
"$(this.api().table().header()).css({'background-color': '#1ed760', 'color': '#fff'});",
 "}")))
    })
  
  output$text.songs <- renderText({paste("Here you see the top", input$Number, "artists with most hits.")})
  
  output$top5.songs <- 
    DT::renderDataTable({
      dt.sf.artist.songs.top5 <- dt.spotify[, list("Songs_per_Artist" = length(unique(Track.Name))), by = "Artist"]
      DT::datatable(head(dt.sf.artist.songs.top5[order(-Songs_per_Artist)], input$Number), options = list(dom = "t", 
    initComplete = JS("function(settings, json) {",
 "$(this.api().table().header()).css({'background-color': '#1ed760', 'color': '#fff'});","}")))
    })
  
# Tab 2 ------------------------------------------------------------------------

  

  output$country <- 
    DT::renderDataTable({
      DT::datatable(dt.spotify[Region == input$Region & Date == input$date.selector, list(Position, Artist, Track.Name, Region, Date)], 
                                                                                                                           options = list(dom = "t", 
                                                                                                                          initComplete = JS("function(settings, json) {",
"$(this.api().table().header()).css({'background-color': '#1ed760', 'color': '#fff'});","}")))
    })


# Tab 3 ----   
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

    g.countries.connections <- delete.edges(g.country.proj, which(E(g.country.proj)$weight < input$number.of.connections))
    #g.countries.connections.proj <- bipartite.projection(g.countries.connections)$proj2

    # Enable selection of country for the plot
    dt.edge <- as.data.table(ends(g.countries.connections, E(g.countries.connections)))
    dt.geo.data <- dt.edge[(V1 == input$country.selector | V2 == input$country.selector), 1:2]

    setnames(dt.geo.data, old = c("V1", "V2"), new = c("from", "to"))
    dt.geo.data <- merge(dt.geo.data, dt.world.cities[, .(country, lat, long)], by.x = 'from', by.y = 'country')
    setnames(dt.geo.data, old = c("lat", "long"), new = c("from_lat", "from_long"))
    dt.geo.data <- merge(dt.geo.data, dt.world.cities[, .(country, lat, long)], by.x = 'to', by.y = 'country')
    setnames(dt.geo.data, old = c("lat", "long"), new = c("to_lat", "to_long"))

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
              lng = as.numeric(dt.world.cities[country == input$country.selector, "long"]), popup = "Please choose different input", options = popupOptions(closeButton = TRUE))    
    } else {
    leaflet(connections()) %>%
    addProviderTiles(providers$CartoDB.DarkMatter,
                     options = providerTileOptions(noWrap = FALSE)) %>%
    addPolylines(data = connections(), lng = ~long, lat = ~lat, group = ~id, color = '#00b200', opacity = 0.5, weight = 2) %>%
    fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    }

})

network.graph.stats <- reactive({
# Filtering dates from slider
#  dt.spotify <- dt.spotify[Date >= sliderMonth$start & Date <= sliderMonth$end, ]
#   # Selecting top artists
#  dt.spotify <- dt.spotify[Position <= input$top.x, ]
#   # Filtering the data table
# dt.spotify <- unique(dt.spotify[, .(Artist, Region)])
#   # Graph preparation
#     dt.unique.countries <- dt.spotify[, .(name = unique(Region), type = TRUE)]
#     dt.unique.artists <- dt.spotify[, .(name = unique(Artist), type = FALSE)]
# 
#   # Bind tables to create vertices for the graph.
#     dt.spotify.vert <- rbind(dt.unique.countries, dt.unique.artists)
# 
#   # Create a graph with countries connected by listening to commmon artists.
#     g.countries.artists <- graph.data.frame(dt.spotify[, .(Region, Artist)],
#                                             directed = FALSE,
#                                             vertices = dt.spotify.vert)
# 
#     g.country.proj <- bipartite.projection(g.countries.artists)$proj2
# 
#     g.countries.connections <- delete.edges(g.country.proj, which(E(g.country.proj)$weight < input$number.of.connections))
  
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
  g.countries.connections <- delete.edges(g.country.proj, which(E(g.country.proj)$weight < input$number.of.connections))
  # dt.countries.connections <- as.data.frame(g.countries.connections)
    results <- data.frame(measure = character(),
                          value = character())

    if (any(1 %in% input$centrality_measures)){
      results <- rbind(results, data.frame(measure = 'Degree Centrality',
                                           value = as.character(round(igraph::degree(g.countries.connections)[input$country.selector], 0))))
    }
    if (any(2 %in% input$centrality_measures)){
      results <- rbind(results, data.frame(measure = 'Betweenness',
                                           value = as.character(round(igraph::betweenness(g.countries.connections)[input$country.selector], 4))))
    }
    if (any(3 %in% input$centrality_measures)){
      results <- rbind(results, data.frame(measure = 'Closeness',
                                           value = as.character(round(igraph::closeness(g.countries.connections)[input$country.selector], 4))))
    }
    if (any(4 %in% input$centrality_measures)){
      results <- rbind(results, data.frame(measure = 'Eigenvector',
                                           value = as.character(round(igraph::evcent(g.countries.connections)$vector[input$country.selector], 4))))
    }
    if (any(5 %in% input$centrality_measures)){
      results <- rbind(results, data.frame(measure = 'Clustering Coefficient',
                                           value = as.character(round(igraph::transitivity(g.countries.connections, type = 'local')[which(V(g.countries.connections)$name == input$country.selector)], 2))
))
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


  # Tab 4: Degree Distribution ----

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
    degree.dist <- degree.dist[Artist %in% as.character(dt.spotify[Region == input$country.selector.2, Artist]), ]

  })
  
output$degree.dist <- renderPlot({

ggthemr("chalk", type = "outer")
ggplot(degree(), aes(x = deg)) + geom_histogram(binwidth = 1) + xlab("Number of connections") + 
    ylab("Count") + ggtitle("Degree Distribution")
    
})

  # Tab 5: Internationality ----

# sliderMonth <- reactiveValues()
# observe({
#    start_date <- as.POSIXct(input$time.selector.3[1], tz = "GMT")
#    sliderMonth$start <- as.Date(timeFirstDayInMonth(start_date))
#    end_date <- as.POSIXct(input$time.selector.3[2], tz = "GMT")
#    sliderMonth$end <- as.Date(timeLastDayInMonth(end_date))
#  })
#  
#  international.data <- reactive({
#    # Filtering dates from slider
#    dt.charts.merged <- dt.charts.merged[date >= sliderMonth$start & date <= sliderMonth$end, ]
#    # Selecting top artists
#    dt.charts.merged <- dt.charts.merged[Position <= input$top.x3, ]
# 
# get_num_artists <- function(country_id){
#   dt.country.temp <- dt.charts.merged[charts.regioncode == country_id]
#   l.artists <- unique(dt.country.temp$artist)
#   return(length(l.artists))
# }
# 
# # We also want to know how many of those artists were not from that
# # country itself. We can use this to determine the percentage of 
# # foreign artists in the complete chart list for a country. 
# 
# # A very small chart [DE DE DE NL] would be 25% internationality for country DE.  
# 
# get_international_perc <- function(country_id){
#   dt.country.temp <- dt.charts.merged[charts.regioncode == country_id]
#   l.artists.countries <- dt.country.temp$artist.regioncode
#   
#   # We get the amount of appearances of local artists
#   n.own.country <- length(which(l.artists.countries == country_id))
#   
#   # We get the amount of appearances of foreign artists
#   n.foreign.country <- (length(l.artists.countries) - n.own.country)
#   
#   # What % of the total was foreign?
#   total.rows <- nrow(dt.country.temp)
#   
#   return(round(((n.foreign.country / total.rows) * 100), 3))
# }
# 
# # The deliverable is a table with info on the amount of artists
# # and the internationality for every country we have charts of. 
# # So, we create this placeholder table and fill it afterwards. 
# 
# col.length <- length(unique(dt.charts.per.country$charts.regioncode))
# 
# dt.countries.info <- data.table(country = unique(dt.charts.per.country$charts.regioncode),
#                                 numArtists = numeric(col.length),
#                                 percInternational = numeric(col.length))
# 
# # Fill the countries.info table
# dt.countries.info$numArtists <- vapply(dt.countries.info$country,
#                                        get_num_artists,
#                                        numeric(1))
# 
# dt.countries.info$percInternational <- vapply(dt.countries.info$country,
#                                               get_international_perc,
#                                               numeric(1))
# 
# return(dt.countries.info)
# })
  
#plotting 
  
output$g.point <- renderPlot({
  ggplot(data = dt.countries.info , aes(x = percInternational, y = numArtists, label=country)) + 
    geom_point() + 
#    geom_point(aes(label == countrycode(input$country.selector.5, 'country.name', 'iso2c'), colour = "green"), size=3) + 
    labs(x = "Percentage of International Actors", y = "Number of Different Actors")
  })

#adding click event data 
output$click_info <- renderPrint({
  nearPoints(dt.countries.info, input$plot_click, xvar = "percInternational", yvar = "numArtists")
})

  # Tab 6: Hofstede ----
reactive_data <- reactive({
  country1 <- input$country.selector.3
  country2 <- input$country.selector.4
  return(subset(hofstede, COUNTRY == country1 | COUNTRY == country2))
  
})

output$hofstede <- renderPlot({
  
ggplot(data = reactive_data(), aes(x=var_type, y=value, fill=COUNTRY)) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(x = "Hofstede's cultural dimension", y = "Value",
         caption = "

                    Note that every cultural dimension ranges from 1 to 100.
                    Data Retrieved on www.hofstede-insights.com") +
    geom_text(aes(label = value), vjust=1.6, color="white",
              position = position_dodge(0.9), size=3.5) +
    scale_fill_manual(values=c('#6AE368','#D3D3D3')) +
    theme_bw()
  })

shared_percentage <-  reactive({

  dt.country1 <- subset(dt.charts.per.country, charts.regionname == input$country.selector.3)
  dt.country2 <- subset(dt.charts.per.country, charts.regionname == input$country.selector.4)
  artists.country1 <- unique(dt.country1$artist)
  artists.country2 <- unique(dt.country2$artist)
  
  common.artists <- length(intersect(artists.country1, artists.country2))
  
  percentage <- round(((common.artists / ((length(artists.country1) + length(artists.country2) - common.artists)))*100), 3)
  
  return(percentage)
})

output$similarities <- renderText({paste(input$country.selector.3,"and", input$country.selector.4, "have a percentage of similar artists of", shared_percentage(),"%.")})

}

