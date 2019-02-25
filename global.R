
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


text_blocks <- list(
  appinfo = paste(
    "QMRAswim is a web tool that models the range of probability of illnesses from swimming in a sewage impacted water body.",
    "The user inputs the measured concentration of the indicator organism and inputs desired reference values, and the output is a histogram of probability of illness from pathogens the user inputs data for.",
    "Downloads will be available for every computational step in the form of an Excel document.",
    "The intended audience for this tool are scientists or public health professionals who have access to the wide variety of reference information that is required to run the tool.",
    "Currently the web tool is coded in R Shiny, but will ideally be published on the web for general use.",
    "What makes this tool useful and unique it can take one measurement from an impacted water body and tell the user ranges of probabilities of illness for any number or type of pathogen."
  ),
  doseresponseinfo = paste(
    "Below is the default values for water consumed by a swimmer in mL.",
    "Water dose is from 'Water ingestion during swimming activities in a pool: a pilot study', lognormal distribution with a mean of 2.92 and a standard deviation of 1.43 (Dufor et al. 2006).",
    "The parameters can be changed but using any distribution besides log normal is not currently supported."
  )
)

