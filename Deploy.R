library(igraph)
library(shiny)
library(data.table)
library(rsconnect) 

setAccountInfo(name='laurens',
               token='60B95632EA04F691ECBBABB5BDB8B88C',
               secret='Fvoc/q/sSfl0iCY1WUlzN0IWztflCzcOljUIzPuc')

deployApp('I.Am.An.App.Name')
