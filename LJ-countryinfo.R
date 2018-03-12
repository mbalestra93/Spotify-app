# load libraries
library(data.table)     
library(ggplot2) 
library(igraph)
library(dplyr)
library("readxl")
library(countrycode)

options(stringsAsFactors = FALSE)

# Import the .csv and the .xlsx
dt.charts.per.country <- read.csv("initial-merge.csv", sep = ",")
dt.artists.countries <- read_excel("Artists & Countries.xlsx")
save.image(file = "countries.RData")

# Or read the .RData file
# load("countries.RData")

# Convert them to data tables, so we can select with square brackets
setDT(dt.artists.countries)
setDT(dt.charts.per.country)

# Change some column names to more clear names
colnames(dt.charts.per.country) <- c("position",
                                     "artist",
                                     "track.name",
                                     "charts.regioncode",
                                     "charts.regionname",
                                     "date",
                                     "day",
                                     "month",
                                     "year")

colnames(dt.artists.countries) <- c("artist.regioncode", "artist")

# Drop the "Global" charts
dt.charts.per.country <- dt.charts.per.country[charts.regioncode != "global"]

dt.charts.per.country$charts.regionname <- countrycode(toupper(dt.charts.per.country$charts.regioncode), "iso2c", "country.name")

# Unify all country codes to upper caps, as R is case-sensitive when matching
dt.charts.per.country$charts.regioncode <- toupper(dt.charts.per.country$charts.regioncode)
dt.artists.countries$artist.regioncode <- toupper(dt.artists.countries$artist.regioncode)

# Merge the dt.charts.per.country and dt.artists.countries on Artist
dt.charts.merged <- merge(dt.charts.per.country,
                          dt.artists.countries,
                          by.x = "artist",
                          by.y = "artist")

# Now, we want to create a table with all countries we have the charts of,
# and determine the number of artists they have had in their charts. 

get_num_artists <- function(country_id){
  dt.country.temp <- dt.charts.merged[charts.regioncode == country_id]
  l.artists <- unique(dt.country.temp$artist)
  return(length(l.artists))
}

# We also want to know how many of those artists were not from that
# country itself. We can use this to determine the percentage of 
# foreign artists in the complete chart list for a country. 

# A very small chart [DE DE DE NL] would be 25% internationality for country DE.  

get_international_perc <- function(country_id){
  dt.country.temp <- dt.charts.merged[charts.regioncode == country_id]
  l.artists.countries <- dt.country.temp$artist.regioncode
  
  # We get the amount of appearances of local artists
  n.own.country <- length(which(l.artists.countries == country_id))
  
  # We get the amount of appearances of foreign artists
  n.foreign.country <- (length(l.artists.countries) - n.own.country)
  
  # What % of the total was foreign?
  total.rows <- nrow(dt.country.temp)
  
  return(round(((n.foreign.country / total.rows) * 100), 3))
}

# The deliverable is a table with info on the amount of artists
# and the internationality for every country we have charts of. 
# So, we create this placeholder table and fill it afterwards. 

col.length <- length(unique(dt.charts.per.country$charts.regioncode))

dt.countries.info <- data.table(country = unique(dt.charts.per.country$charts.regioncode),
                                numArtists = numeric(col.length),
                                percInternational = numeric(col.length))

# Fill the countries.info table
dt.countries.info$numArtists <- vapply(dt.countries.info$country,
                                       get_num_artists,
                                       numeric(1))

dt.countries.info$percInternational <- vapply(dt.countries.info$country,
                                              get_international_perc,
                                              numeric(1))

dt.countries.info

write.table(dt.countries.info, "countriesinfo.txt", sep="\t")


# Function to determine the total number of shared artists 
# over the total numbers of artists present in the charts of both countries
shared_percentage <- function(country1, country2){
  dt.country1 <- subset(dt.charts.per.country, charts.regioncode == country1)
  dt.country2 <- subset(dt.charts.per.country, charts.regioncode == country2)
  
  artists.country1 <- unique(dt.country1$artist)
  artists.country2 <- unique(dt.country2$artist)
  
  print(length(artists.country1))
  print(length(artists.country2))
  
  common.artists <- length(intersect(artists.country1, artists.country2))
  
  print(common.artists)
  
  return(common.artists / (length(artists.country1) + length(artists.country2) - common.artists) )
}

shared_percentage("NL", "DE")