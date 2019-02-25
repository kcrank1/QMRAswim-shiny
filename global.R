
rm(list = ls())

# Handle packages
lapply(
  X = c('shiny', 'CharFun', 'reshape2', 'ggplot2', 'scales', 'shinydashboard'),
  FUN = function(p) {
    if(p %in% installed.packages()[,'Package'] == FALSE) {
      install.packages(p, dependencies = TRUE)
    }
    library(p, character.only = TRUE)
  }
)
