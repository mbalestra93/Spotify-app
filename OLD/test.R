
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





#HOFSTEDE
hofstede$COUNTRY <- as.character(hofstede$COUNTRY)

#Function for individual measure plot
get_plot <- function(var){
  return( 
    ggplot(data=hofstede, aes(x=`ISO CODE`, y=var)) + geom_bar(stat = "identity")
    )
}

get_plot(hofstede$Individualism)
get_plot(hofstede$Masculinity)
...


ggplot(data=hofstede, aes(x=colnames(hofstede.trans)[3:8], y=hofstede$COUNTRY[1], fill=hofstede$COUNTRY)) + geom_bar(stat = "identity")

#function for individual country plot

#create transpoded data frame
hofstede.trans <- transpose(hofstede)
colnames(hofstede.trans) = hofstede.trans[2, ] 
hofstede.trans <- hofstede.trans[-c(1, 2),]
rownames(hofstede.trans) = c("Power Distance", "Individualism", "Masculinity", "Uncertainty Avoidance", "Long Term Orientation", "Indulgence")

#new function
get_country_plot <- function(i){
  return( 
    ggplot(data=hofstede.trans, aes(x=rownames(hofstede.trans), y=hofstede.trans[, i])) + geom_bar(stat = "identity")
  )
}

#uses the function
get_country_plot("Canada")
get_country_plot("France")


#comparing plots

compared_plot <- function(i, j){
  dt.temp <- subset(df, COUNTRY==i | COUNTRY==j)
  return(
    ggplot(data=dt.temp, aes(x=var_type, y=dt.temp$value, fill=COUNTRY)) + geom_bar(stat = "identity", position = position_dodge())
  )
}

#using function
compared_plot("Canada", "France")

