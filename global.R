
rm(list = ls())

# # Handle packages
# lapply(
#   X = c("reshape", "shiny", "CharFun", "tidyverse", "scales", "shinydashboard", "shinyjs","shinythemes","shinytest"),
#   FUN = function(p) {
#     if(p %in% installed.packages()[,"Package"] == FALSE) {
#       install.packages(p, dependencies = TRUE)
#     }
#     library(p, character.only = TRUE)
#   }
# )
library(reshape)
library(shiny)
library(CharFun)
library(tidyverse)
library(scales)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinytest)
# Load modules
lapply(
  X = list.files(path = "modules", full.names = TRUE, recursive = TRUE),
  FUN = source
)

# Functions for probability illness code
# Single parameter exponential
f1 <- function(x, n) {
  1 - (exp(-x * n))
}
# Two parameter beta-poisson
f2 <- function(a, b, n) {
  1 - (1 + n / a) ^ -b
}
# Hypergeometric
f3 <- function(a, b, n){
  1 - hypergeom1F1(-n, a, b)
}

text_blocks <- list(
  appinfo = list(tags$b("What it does:"),
                 "Models the probability of illnesses due to swimming in a sewage impacted waters",
                 tags$br(),
                 tags$b("Outputs:"),
                 "Downloadable data of computational steps, boxplot of probability of illnesses due to any pathogens for which the user inputs reference values",
                 tags$br(),
                 tags$b("How to use:"),
                 "Input the measured concentration of an indicator organism along with reference values specified in dropdown boxes below"
                 #    "The intended audience for this tool are scientists or public health professionals who have access to the wide variety of reference information that is required to run the tool.",
                 #    "This tool is useful and unique because it can take one measurement from an impacted water body and tell the user ranges of probabilities of illness for any number or type of pathogen."
  ),
  doseresponseinfo = paste(
    "Default water dose in mL is from 'Water ingestion during swimming activities in a pool: a pilot study', (Dufor et al. 2006).",
    "Log-e normal is only distribution currently supported."
  ),
  plot1 = "The dashed line represents the EPA recreational water standard of 30 illnesses per 1000 bathers",
  download_info = "File preview only shows first six values. Actual table will as long as the number of samplings. The export is a .csv."
)