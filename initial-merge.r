library(countrycode)

sf <- read.csv("initial-merge.csv", sep = ";")

sf <- subset(sf, RegionCode != "global")

sf$Region <- countrycode(toupper(sf$RegionCode), "iso2c", "country.name")

write.csv(sf, "finaldata.csv")

 