library('data.table')
library('maps')
library('countrycode')
library('readxl')
library('tidyr')

data("world.cities")

dt.world.cities <- as.data.table(world.cities)

setnames(dt.world.cities, old = c("country.etc"), new = c("country"))

rm(world.cities)
dt.world.cities <- dt.world.cities[capital == 1, .(name, country, lat, long)]
dt.world.cities[which(dt.world.cities$country == 'Czech Republic'), 'country'] <- 'Czechia'
dt.world.cities[which(dt.world.cities$country == 'UK'), 'country'] <- 'United Kingdom'
dt.world.cities[which(dt.world.cities$country == 'USA'), 'country'] <- 'United States'
dt.world.cities <- rbind(dt.world.cities, data.frame(name = 'Hong Kong', country = 'Hong Kong',
                                                     lat = 22.3700556,  long = 114.1535941))

load("spotify-rdata.RData")
dt.spotify <- as.data.table(sf)
rm(sf)

# Convert the chart regions' country codes to actual names
dt.spotify <- subset(dt.spotify, Region != "global")
dt.spotify$Region <- countrycode(toupper(dt.spotify$Region), "iso2c", "country.name")

dt.spotify <- as.data.table(dt.spotify[, Date := as.Date(paste(dt.spotify$Year,
                                                               ifelse(dt.spotify$Month<10, paste0('0', dt.spotify$Month), dt.spotify$Month),
                                                               ifelse(dt.spotify$Day<10, paste0('0', dt.spotify$Day), dt.spotify$Day),
                                                               sep = '/'))
                                       ]
)

dt.spotify <- as.data.table(dt.spotify[, month_year := paste0(dt.spotify$Month, '-', dt.spotify$Year)])

dt.spotify <- merge(dt.spotify, dt.world.cities[, .(country, lat, long)],
                    by.x = 'Region',
                    by.y = 'country'
)

# Internationality and Divergence --

load("International.RData")

dt.spotify <- merge(dt.spotify, 
                    unique(dt.charts.merged[, .(charts.regioncode, charts.regionname)]), 
                           by.x = 'Region', by.y = 'charts.regionname')
setnames(dt.spotify, 'charts.regioncode', 'Region_cd')

# Hofstede ---

# remove the factorization of strings
options(stringsAsFactors = FALSE)

# load the database
dt.hofstede <- read_excel('Hofstede.xlsx')

# reshaping the database from wide to long
dt.hofstede <- gather(dt.hofstede, var_type, value, "Power Distance":"Indulgence")

dt.hofstede[which(dt.hofstede$COUNTRY == 'Czech Republic'), 'COUNTRY'] <- 'Czechia'
dt.hofstede[which(dt.hofstede$COUNTRY == 'U.S.A'), 'COUNTRY'] <- 'United States'

rm(col.length, dt.countries.info, dt.charts.merged, dt.charts.per.country, 
   get_international_perc, get_num_artists, shared_percentage)
save.image(file = "my_work_space.RData")

