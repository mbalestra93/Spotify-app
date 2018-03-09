
# load libraries
library(data.table)     
library(ggplot2) 
library(igraph)
library(dplyr)
library("readxl")

# load new artist database
test <- read_excel("Artists & Countries.xlsx")

# merge to a final one
final <- merge(sf, test, by = "Artist")
View(final)

# add internationality check
final$internationality <- final$RegionCode == final$country
View(final)

# collapsing
test <- final[, list(length(unique(Artist))), by = "RegionCode"]
test <- final[, list(n_likes = sum(internationality)), by = "RegionCode"]

maybe <- unique(final$RegionCode)

test <- final[, list(n_likes = sum(internationality)), by = maybe]

