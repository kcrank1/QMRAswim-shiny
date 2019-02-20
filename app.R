
###Copy of app.R, with no for loops
library(shiny)
library(CharFun)
library(reshape2)
library(ggplot2)
library(scales)
library(shinydashboard)
# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(disable=TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
      
      fluidRow(
        box(titlePanel("QMRA of swimming in sewage impacted environmental waters"),width=12,background = "maroon"),
      br()
      ),
      fluidRow(
        box(width=12,textOutput("appinfo") ),
        br()
      ),
      fluidRow(
      ##indicator
      box( width=3,
        numericInput(inputId="indic_enviro_conc", 
                   label = "Concentration of indicator in environmental waters in log 10 copies/100mL", 
                   value=1,
                   min = 0, 
                   max = 100, 
                   step = NA,
                   width = NULL)
      ),
      
      ##indicator in sewage
      box( width=3,
      selectInput(inputId = "indic_sewage_dist",
                  label= "Which distribution does the indicator concentration in sewage follow?", 
                  choices= c("Log Uniform", "Log Normal"),
                  selected = "Log Uniform"),
      conditionalPanel(
        condition = "input.indic_sewage_dist == 'Log Uniform'",
           numericInput(inputId = "max",
                     label = "Maximum concentration of indicator in sewage in log10 copies/L ",
                     value=9.5,
                     min = 0,
                     max=100,
                     step=NA,
                     width=NULL),
        numericInput(inputId = "min",
                     label =  "Minimum concentration of indicator in sewage in log10 copies/L",
                     value = 7,
                     min = 0,
                     max=100,
                     step = NA,
                     width = NULL)
        
     
      ),
      conditionalPanel(
        condition = "input.indic_sewage_dist == 'Log Normal'",
        numericInput(inputId = "alpha",
                     label =  "Enter alpha for log normal distribution",
                     value = 1,
                     min = 0,
                     max=100,
                     step = NA,
                     width = NULL),
        numericInput(inputId = "beta",
                     label =  "Enter beta for log normal distribution",
                     value = 1,
                     min = 0.000,
                     max=100,
                     step = NA,
                     width = NULL)
      )
      ),
      #seed and count
      box(width=3,
      numericInput(inputId = "seed",
                   label = "Set seed of random number generator, default is 1.",
                   value=1,
                   min=0,
                   max=1000000000,
                   step=NA,
                   width=NULL),
      
      sliderInput(inputId="count",
                  label="How many samplings? Many Monte Carlo simulations use 10,000",
                  min = 0,
                  max = 100000,
                  value = 10000,
                  step=5000)
      ),
  ###exposure
  box(width=3,
      textOutput("doseresponseinfo"),
      
      numericInput(inputId="dosemean",
                   label="Mean of dose",
                   min=0,
                   max=100,
                   value=2.92,
                   step=NA
      ),
      numericInput(inputId="dosesd",
                   label="Standard deviation of dose ",
                   min=0,
                   max=10,
                   value=1.42,
                   step=NA),
      br(),
      numericInput(inputId="number_of_pathogens",
                   label="How many pathogen dose responses to include, up to 3 currently supported.",
                   min=1,
                   max=3,
                   value=1,
                   step=1)
  ) 
      ),
  
  ###end row 1
  
  ###begin row 2
  fluidRow(
    box(width=6, title="Probability of Illness from Swimming in Contaminated Water",solidHeader = TRUE,status = "primary",
      plotOutput(outputId = "boxplot1"),
      "The dashed line represents the EPA recreational water standard of 30 illnesses per 1000 bathers"),
    box(width=6, title="Wastewater Dose Distribution",solidHeader = TRUE,status = "primary",
      plotOutput(outputId = "histWWdose"))
  ),
  ##begin row 3
  fluidRow(
    box(width=4,
#begin conditional panel n=1
      conditionalPanel(condition= "input.number_of_pathogens == '1'",
                       textInput(inputId = "pill1_name1o1",
                                 label="Name of pathogen 1",
                                 value="Pathogen 1"),
                       
                       selectInput(inputId = "doseresp1o1",
                                   label= "Choose which dose response model to use for pathogen 1.",
                                   choices= c("Single parameter exponential", "Two-parameter beta-poisson","Two-parameter hypergeometric1F1"),
                                   selected = "Single parameter exponential",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.doseresp1o1 != 'Single parameter exponential'",
                         numericInput(inputId = "alpha1o1",
                                      label =  "Enter the first parameter, alpha",
                                      value = 1,
                                      min = 0.000,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         
                         numericInput(inputId = "beta1o1",
                                      label =  "Enter the second parameter, beta",
                                      value = 1,
                                      min = 0.000,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       conditionalPanel(
                         condition = "input.doseresp1o1 == 'Single parameter exponential'",
                         numericInput(inputId = "lambda1o1",
                                      label =  "Enter the exponential parameter, lambda",
                                      value = 0.4172,
                                      min = 0.000,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       
                       selectInput(inputId = "Pillinfcat1o1",
                                   label= "Is Pill/inf for pathogen 1 a uniform distribution or a single decimal value?",
                                   choices= c("single value", "uniform"),
                                   selected = "single value",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat1o1== 'single value'",
                         numericInput(inputId = "Pillinf1o1",
                                      label =  "Enter the probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0.0000000,
                                      max=1,
                                      step = NA,
                                      width = NULL) ),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat1o1 == 'uniform'",
                         numericInput(inputId = "Pillinfmin1o1",
                                      label =  "Enter the min value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0.0000000,
                                      max=1,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "Pillinfmax1o1",
                                      label =  "Enter the max value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0.0000000,
                                      max=1,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       selectInput(inputId = "path_sewage_dist1o1",
                                   label= "Choose the distrubtion that pathogen 1 follows in sewage",
                                   choices= c("Log Uniform", "Log Normal"),
                                   selected = "Log Uniform"),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist1o1 == 'log uniform'",
                         numericInput(inputId = "p_min1o1",
                                      label =  "Enter the min value of pathogen 1 in sewage in log10 copies/L",
                                      value = 5.5,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_max1o1",
                                      label =  "Enter the max value of pathogen 1 in sewage in log10 copies/L",
                                      value = 8,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist1o1 == 'log normal'",
                         numericInput(inputId = "p_alpha1o1",
                                      label =  "Enter the alpha value, or the mean in log10 copies/L",
                                      value = 1,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_beta1o1",
                                      label =  "Enter the beta value, or the standard deviation",
                                      value = 1,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL) 
                         
                         
                       ) #end n=1/1
                       
      )),#end of dose response conditional panel n=1
     box(width=4,
#begin dose response conditional panel n=2
      conditionalPanel(condition= "input.number_of_pathogens == '2'",
                       textInput(inputId = "pill1_name1o2",
                                 label="Name of pathogen 1",
                                 value="Pathogen 1"),
                       selectInput(inputId = "doseresp1o2",
                                   label= "Choose which dose response model to use for pathogen 1.",
                                   choices= c("Single parameter exponential", "Two-parameter beta-poisson","Two-parameter hypergeometric1F1"),
                                   selected = "Single parameter exponential",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.doseresp1o2 != 'Single parameter exponential'",
                         numericInput(inputId = "alpha1o2",
                                      label =  "Enter the first parameter, alpha",
                                      value = 1,
                                      min = 0.000,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         
                         numericInput(inputId = "beta1o2",
                                      label =  "Enter the second parameter, beta",
                                      value = 1,
                                      min = 0.0000,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       conditionalPanel(
                         condition = "input.doseresp1o2 == 'Single parameter exponential'",
                         numericInput(inputId = "lambda1o2",
                                      label =  "Enter the exponential parameter, lambda",
                                      value = 0.4172,
                                      min = 0.000,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       
                       selectInput(inputId = "Pillinfcat1o2",
                                   label= "Is Pill/inf for pathogen 1 a uniform distribution or a single decimal value?",
                                   choices= c("uniform", "single value"),
                                   selected = "single value",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat1o2== 'single value'",
                         numericInput(inputId = "Pillinf1o2",
                                      label =  "Enter the probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0.0000000,
                                      max=1,
                                      step = NA,
                                      width = NULL) ),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat1o2 == 'uniform'",
                         numericInput(inputId = "Pillinfmin1o2",
                                      label =  "Enter the min value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0.0000000,
                                      max=1,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "Pillinfmax1o2",
                                      label =  "Enter the max value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0.0000000,
                                      max=1,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       selectInput(inputId = "path_sewage_dist1o2",
                                   label= "Choose the distrubtion that pathogen 1 follows in sewage",
                                   choices= c("Log Uniform", "Log Normal"),
                                   selected = "Log Uniform",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist1o2 == 'log uniform'",
                         numericInput(inputId = "p_min1o2",
                                      label =  "Enter the min value of pathogen 1 in sewage in log10 copies/L",
                                      value = 5.5,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_max1o2",
                                      label =  "Enter the max value of pathogen 1 in sewage in log10 copies/L",
                                      value = 8,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist1o2 == 'log normal'",
                         numericInput(inputId = "p_alpha1o2",
                                      label =  "Enter the alpha value, or the mean in log10 copies/L",
                                      value = 1,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_beta1o2",
                                      label =  "Enter the beta value, or the standard deviation",
                                      value = 1,
                                      min = 0.00,
                                      max=100,
                                      step = NA,
                                      width = NULL) 
                         
                         
                         
                       ),##end of n=1/2 beginning of n=2/2
                       textInput(inputId = "pill2_name2o2",
                                 label="Name of pathogen 2",
                                 value="Pathogen 2"),
                       selectInput(inputId = "doseresp2o2",
                                   label= "Choose which dose response model to use for pathogen 2.",
                                   choices= c("Single parameter exponential", "Two-parameter beta-poisson","Two-parameter hypergeometric1F1"),
                                   selected = "Two-parameter hypergeometric1F1",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.doseresp2o2 != 'Single parameter exponential'",
                         numericInput(inputId = "alpha2o2",
                                      label =  "Enter the first parameter, alpha",
                                      value = 0.04,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         
                         numericInput(inputId = "beta2o2",
                                      label =  "Enter the second parameter, beta",
                                      value = 0.095,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       conditionalPanel(
                         condition = "input.doseresp2o2 == 'Single parameter exponential'",
                         numericInput(inputId = "lambda2o2",
                                      label =  "Enter the exponential parameter, lambda",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       
                       selectInput(inputId = "Pillinfcat2o2",
                                   label= "Is Pill/inf for pathogen 2 a uniform distribution or a single decimal value?",
                                   choices= c("uniform", "single value"),
                                   selected = "single value",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat2o2 == 'single value'",
                         numericInput(inputId = "Pillinf2o2",
                                      label =  "Enter the probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.6,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL) ),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat2o2 == 'uniform'",
                         numericInput(inputId = "Pillinfmin2o2",
                                      label =  "Enter the min value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "Pillinfmax2o2",
                                      label =  "Enter the max value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       selectInput(inputId = "path_sewage_dist2o2",
                                   label= "Choose the distrubtion that pathogen 2 follows in sewage",
                                   choices= c("Log Uniform", "Log Normal"),
                                   selected = "Log Uniform",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist2o2 == 'log uniform'",
                         numericInput(inputId = "p_min2o2",
                                      label =  "Enter the min value of pathogen 2 in sewage in log10 copies/L",
                                      value = 3,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_max2o2",
                                      label =  "Enter the max value of pathogen 2 in sewage in log10 copies/L",
                                      value = 6,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist2o2 == 'log normal'",
                         numericInput(inputId = "p_alpha2o2",
                                      label =  "Enter the alpha value, or the mean in log10 copies/L",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_beta2o2",
                                      label =  "Enter the beta value, or the standard deviation",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL) 
                         
                         
                       ) ##end of n=2/2
                       
                       
      )),#end of dose response conditional panel n=2
    box(width=4,
#Beginning of dose response conditional panel n=3
      conditionalPanel(condition= "input.number_of_pathogens== '3'",
                       textInput(inputId = "pill1_name1o3",
                                 label="Name of pathogen 1",
                                 value="Pathogen 1"),
                       selectInput(inputId = "doseresp1o3",
                                   label= "Choose which dose response model to use for pathogen 1.",
                                   choices= c("Single parameter exponential", "Two-parameter beta-poisson","Two-parameter hypergeometric1F1"),
                                   selected = "Single parameter exponential",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.doseresp1o3 != 'Single parameter exponential'",
                         numericInput(inputId = "alpha1o3",
                                      label =  "Enter the first parameter, alpha",
                                      value = 1,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         
                         numericInput(inputId = "beta1o3",
                                      label =  "Enter the second parameter, beta",
                                      value = 1,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       conditionalPanel(
                         condition = "input.doseresp1o3 == 'Single parameter exponential'",
                         numericInput(inputId = "lambda1o3",
                                      label =  "Enter the exponential parameter, lambda",
                                      value = 0.4172,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       
                       selectInput(inputId = "Pillinfcat1o3",
                                   label= "Is Pill/inf for pathogen 1 a uniform distribution or a single decimal value?",
                                   choices= c("uniform", "single value"),
                                   selected = "single value",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat1o3== 'single value'",
                         numericInput(inputId = "Pillinf1o3",
                                      label =  "Enter the probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL) ),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat1o3 == 'uniform'",
                         numericInput(inputId = "Pillinfmin1o3",
                                      label =  "Enter the min value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "Pillinfmax1o3",
                                      label =  "Enter the max value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       selectInput(inputId = "path_sewage_dist1o3",
                                   label= "Choose the distrubtion that pathogen 1 follows in sewage",
                                   choices= c("Log Uniform", "Log Normal"),
                                   selected = "Log Uniform",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist1o3 == 'log uniform'",
                         numericInput(inputId = "p_min1o3",
                                      label =  "Enter the min value of pathogen 1 in sewage in log10 copies/L",
                                      value = 5.5,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_max1o3",
                                      label =  "Enter the max value of pathogen 1 in sewage in log10 copies/L",
                                      value = 8,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist1o3 == 'log normal'",
                         numericInput(inputId = "p_alpha1o3",
                                      label =  "Enter the alpha value, or the mean in log10 copies/L",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_beta1o3",
                                      label =  "Enter the beta value, or the standard deviation",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL) 
                         
                         
                         
                         
                       ),##end of n=1/3 
                       textInput(inputId = "pill2_name2o3",
                                 label="Name of pathogen 2",
                                 value="Pathogen 2"),
                       
                       selectInput(inputId = "doseresp2o3",
                                   label= "Choose which dose response model to use for pathogen 2.",
                                   choices= c("Single parameter exponential", "Two-parameter beta-poisson","Two-parameter hypergeometric1F1"),
                                   selected = "Two-parameter hypergeometric1F1",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.doseresp2o3 != 'Single parameter exponential'",
                         numericInput(inputId = "alpha2o3",
                                      label =  "Enter the first parameter, alpha",
                                      value = 0.04,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         
                         numericInput(inputId = "beta2o3",
                                      label =  "Enter the second parameter, beta",
                                      value = 0.095,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       conditionalPanel(
                         condition = "input.doseresp2o3 == 'Single parameter exponential'",
                         numericInput(inputId = "lambda2o3",
                                      label =  "Enter the exponential parameter, lambda",
                                      value = 1,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       
                       selectInput(inputId = "Pillinfcat2o3",
                                   label= "Is Pill/inf for pathogen 2 a uniform distribution or a single decimal value?",
                                   choices= c("uniform", "single value"),
                                   selected = "single value",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat2o3 == 'single value'",
                         numericInput(inputId = "Pillinf2o3",
                                      label =  "Enter the probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.6,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL) ),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat2o3 == 'uniform'",
                         numericInput(inputId = "Pillinfmin2o3",
                                      label =  "Enter the min value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "Pillinfmax2o3",
                                      label =  "Enter the max value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       selectInput(inputId = "path_sewage_dist2o3",
                                   label= "Choose the distrubtion that pathogen 2 follows in sewage",
                                   choices= c("Log Uniform", "Log Normal"),
                                   selected = "Log Uniform",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist2o3 == 'log uniform'",
                         numericInput(inputId = "p_min2o3",
                                      label =  "Enter the min value of pathogen 2 in sewage in log10 copies/L",
                                      value = 3,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_max2o3",
                                      label =  "Enter the max value of pathogen 2 in sewage in log10 copies/L",
                                      value = 6,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist2o3 == 'log normal'",
                         numericInput(inputId = "p_alpha2o3",
                                      label =  "Enter the alpha value, or the mean in log10 copies/L",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_beta2o3",
                                      label =  "Enter the beta value, or the standard deviation",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL) 
                       ), ##end of n=2/3
                       textInput(inputId = "pill3_name3o3",
                                 label="Name of pathogen 3",
                                 value="Pathogen 3"),
                       selectInput(inputId = "doseresp3o3",
                                   label= "Choose which dose response model to use for pathogen 3.",
                                   choices= c("Single parameter exponential", "Two-parameter beta-poisson","Two-parameter hypergeometric1F1"),
                                   selected = "Two-parameter beta-poisson",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.doseresp3o3 != 'Single parameter exponential'",
                         numericInput(inputId = "alpha3o3",
                                      label =  "Enter the first parameter, alpha",
                                      value = 0.42,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         
                         numericInput(inputId = "beta3o3",
                                      label =  "Enter the second parameter, beta",
                                      value = 0.26,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       conditionalPanel(
                         condition = "input.doseresp3o3 == 'Single parameter exponential'",
                         numericInput(inputId = "lambda3o3",
                                      label =  "Enter the exponential parameter, lambda",
                                      value = 1,
                                      min = 0.0001,
                                      max=100,
                                      step = NA,
                                      width = NULL) ),
                       
                       selectInput(inputId = "Pillinfcat3o3",
                                   label= "Is Pill/inf for pathogen 3 a uniform distribution or a single decimal value?",
                                   choices= c("uniform", "single value"),
                                   selected = "single value",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat3o3== 'single value'",
                         numericInput(inputId = "Pillinf3o3",
                                      label =  "Enter the probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.35,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL) ),
                       
                       conditionalPanel(
                         condition = "input.Pillinfcat3o3 == 'uniform'",
                         numericInput(inputId = "Pillinfmin3o3",
                                      label =  "Enter the min value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "Pillinfmax3o3",
                                      label =  "Enter the max value of probability of illness if infected in decimal form i.e.60% ->0.6",
                                      value = 0.5,
                                      min = 0,
                                      max=1,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       selectInput(inputId = "path_sewage_dist3o3",
                                   label= "Choose the distribution that pathogen 3 follows in sewage",
                                   choices= c("Log Uniform", "Log Normal"),
                                   selected = "Log Uniform",
                                   multiple = FALSE,
                                   selectize = FALSE,
                                   width = NULL,
                                   size = NULL),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist3o3 == 'log uniform'",
                         numericInput(inputId = "p_min3o3",
                                      label =  "Enter the min value of pathogen 3 in sewage in log10 copies/L",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_max3o3",
                                      label =  "Enter the max value of pathogen 3 in sewage in log10 copies/L",
                                      value = 2.5,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL)
                       ),
                       
                       conditionalPanel(
                         condition = "input.path_sewage_dist3o3 == 'log normal'",
                         numericInput(inputId = "p_alpha3o3",
                                      label =  "Enter the alpha value, or the mean in log10 copies/L",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL),
                         numericInput(inputId = "p_beta3o3",
                                      label =  "Enter the beta value, or the standard deviation",
                                      value = 1,
                                      min = 0,
                                      max=100,
                                      step = NA,
                                      width = NULL) 
                         
                         
                       ))), ##end of n=3/3 DELETE COMMA IF GOING BACK TO SIDEBAR
                       
                       
    #  ) 
   # ),
    # Show a plot of the generated distribution
   # sidebarPanel(
      #textInput("total_pi",label="Do not touch or change",value="Total Pi"),
      #textOutput("test"),
      
      
      fluidRow(
        box(title="Download a dataset generated by this app",width=6,
          selectInput("dataset", "Choose a dataset:",
                        choices = c("Wastewater Dose", "Probability of Illness")),
            
            # Button
            
          downloadButton("downloadData", "Download"),
          actionButton("do","Preview data for download"),
          br(),
          "File preview only shows first six values. Actual table will as long as the number of samplings. The export will be a .csv."
    ),
    box(width=6,
      tableOutput("table")
    )
  )
)
)
)

# Define server logic
server <- function(input, output, session) {
  output$appinfo<-renderText(print("QMRAswim is a web tool that models the range of probability of illnesses from swimming in a sewage impacted water body. The user inputs the measured concentration of the indicator organism and inputs desired reference values, and the output is a histogram of probability of illness from pathogens the user inputs data for. Downloads will be available for every computational step in the form of an Excel document.
            
                                   The intended audience for this tool are scientists or public health professionals who have access to the wide variety of reference information that is required to run the tool. 
                                   
                                   Currently the web tool is coded in R Shiny, but will ideally be published on the web for general use.
                                   
                                   What makes this tool useful and unique it can take one measurement from an impacted water body and tell the user ranges of probabilities of illness for any number or type of pathogen.
                                   "))
  output$doseresponseinfo <-renderText(
    print("Below is the default values for water consumed by a swimmer in mL. Water dose is from 'Water ingestion during swimming activities in a pool: a pilot study', lognormal distribution with a mean of 2.92 and a standard deviation of 1.43 (Dufor et al. 2006). The parameters can be changed but using any distribution besides log normal is not currently supported.")
  )
  
  # Wastewater Dose function
  #WastewaterDose<-function(input.indic_enviro_conc, input.indic_sewage_dist='Log Uniform',input.min,input.max, input.alpha, input.beta, input.seed =1, input.count=10000,input.dosemean, input.dosesd){
  count<-reactive({input$count})
  reactive({set.seed(reactive({input$seed}))})
  a<-reactive(input$indic_enviro_conc)
  b<-reactive(input$indic_sewage_dist)
  c1<-reactive(input$min)
  d<-reactive(input$max)
  e<-reactive(input$alpha)
  f<-reactive(input$beta)
  g<-reactive(input$dosemean)
  h<-reactive(input$dosesd)
  # for log uniform, WWdose=EnvWaterDose/1000*(fractionWW) 
  # exposure volume while swimming event occurs in mL changed
  # to L/indicator concentration changed divided by the range of concentration in sewage.
  # The EnvWaterDose is equal to the rlnorm distribution with the dose mean and dose sd as arguments.
  # This is the water in mouth from swimming value that is default.
  # The fractionWW is equal to the concentration of the indicator in the environment
  # divided by 10^Ci_Sew, or Concentration in Sewage.This is to convert to log10. Concentration in 
  # Sewage, Ci_Sew, is calculated using the rlnorm, or normal distribution with the inputs being 
  # alpha and beta respectively.
  # For log normal, it is similarly calculated except that Ci_Sew, the concentration of indicator in
  # sewage, is a runif, or a uniform distribution with the min and max arguments included.
  WWdose<-reactive(if (b()=='Log Uniform')
    
    ((rlnorm(count(),g(),h()))/1000*((a()/10^(runif(count(),min=c1(),max=d())))))
    
    else if (b()=='Log Normal')
      
      ((rlnorm(count(),g(),h())/1000*(a()/10^(rlnorm(count(),e(),f())))))
    
    else print("an error occurred between lines 889 and 893")
    
  ) 
  WWdosedf<-reactive(data.frame(WWdose()))
  
  #  reactive({if (input$indic_sewage_dist== 'Log Uniform'){
  #   Ci_Sew<-runif(count,min=input$min,max=input$max)
  #   EnvWaterDose<-(rlnorm(count, input$dosemean,input$dosesd))
  #   fractionWW<-(input$indic_enviro_conc/10^Ci_Sew)
  #   WWdose<-(EnvWaterDose/1000*(fractionWW))
  #   return(WWdose)
  # } else if(input$indic_sewage_dist == 'Log Normal'){
  #     Ci_Sew<-rlnorm(count,input$alpha,input$beta)
  #     EnvWaterDose<-(rlnorm(count, input$dosemean,input$dosesd))
  #     fractionWW<-(input$indic_enviro_conc/10^Ci_Sew)
  #     WWdose<-(EnvWaterDose/1000*(fractionWW))###exposure volume while swimming event occurs in mL changed to L/indicator concentration changed divided by the range of concentration in sewage
  #    return(WWdose)
  #      } else
  #     stop("Invalid entry for distribution, must be either 'Log Uniform' or 'Log Normal'")
  #  })
  
  # output$histWWdose<-renderPlot({
  #   p<-qplot(WWdosedf()[,1], geom="histogram")
  #   print(p)
  #   })
  
  #WWdoseprint<-isolate(WWdose()) to check if it is working
  # print(head(WWdoseprint)
  
  #Probability_Illness function
  
  number_of_pathogens<-reactive(input$number_of_pathogens)
  doseresp1o1<-reactive(input$doseresp1o1) #1/1
  alpha1o1 <-reactive(input$alpha1o1)
  beta1o1<-reactive(input$beta1o1)
  lambda1o1<-reactive(input$lambda1o1)
  Pillinfcat1o1 <-reactive(input$Pillinfcat1o1)
  Pillinf1o1<-reactive(input$Pillinf1o1)
  Pillinfmin1o1<-reactive(input$Pillinfmin1o1)
  Pillinfmax1o1<-reactive(input$Pillinfmax1o1)
  path_sewage_dist1o1  <-reactive(input$path_sewage_dist1o1)
  p_min1o1<-reactive(input$p_min1o1)
  p_max1o1<-reactive(input$p_max1o1)
  p_alpha1o1<-reactive(input$p_alpha1o1)
  p_beta1o1 <-reactive(input$p_beta1o1) 
  doseresp1o2<-reactive(input$doseresp1o2) #1/2
  alpha1o2<-reactive(input$alpha1o2)
  beta1o2 <-reactive(input$beta1o2)
  lambda1o2<-reactive(input$lambda1o2)
  Pillinfcat1o2 <-reactive(input$Pillinfcat1o2)
  Pillinf1o2<-reactive(input$Pillinf1o2)
  Pillinfmin1o2<-reactive(input$Pillinfmin1o2)
  Pillinfmax1o2 <-reactive(input$Pillinfmax1o2)
  path_sewage_dist1o2 <-reactive(input$path_sewage_dist1o2)
  p_min1o2<-reactive(input$p_min1o2)
  p_max1o2 <-reactive(input$p_max1o2)
  p_alpha1o2 <-reactive(input$p_alpha1o2)
  p_beta1o2 <-reactive(input$p_beta1o2)
  doseresp2o2 <-reactive(input$doseresp2o2)#2/2
  alpha2o2<-reactive(input$alpha2o2)
  beta2o2 <-reactive(input$beta2o2)
  lambda2o2 <-reactive(input$lambda2o2)
  Pillinfcat2o2<-reactive(input$Pillinfcat2o2)
  Pillinf2o2<-reactive(input$Pillinf2o2)
  Pillinfmin2o2 <-reactive(input$Pillinfmin2o2)
  Pillinfmax2o2<-reactive(input$Pillinfmax2o2)
  path_sewage_dist2o2<-reactive(input$path_sewage_dist2o2)
  p_min2o2<-reactive(input$p_min2o2)
  p_max2o2<-reactive(input$p_max2o2)
  p_alpha2o2 <-reactive(input$p_alpha2o2)
  p_beta2o2 <-reactive(input$p_beta2o2) 
  doseresp1o3 <-reactive(input$doseresp1o3) #1/3
  alpha1o3<-reactive(input$alpha1o3)
  beta1o3<-reactive(input$beta1o3)
  lambda1o3 <-reactive(input$lambda1o3)
  Pillinfcat1o3<-reactive(input$Pillinfcat1o3)
  Pillinf1o3<-reactive(input$Pillinf1o3)
  Pillinfmin1o3<-reactive(input$Pillinfmin1o3)
  Pillinfmax1o3<-reactive(input$Pillinfmax1o3)
  path_sewage_dist1o3 <-reactive(input$path_sewage_dist1o3)
  p_min1o3<-reactive(input$p_min1o3)
  p_max1o3<-reactive(input$p_max1o3)
  p_alpha1o3<-reactive(input$p_alpha1o3)
  p_beta1o3 <-reactive(input$p_beta1o3)
  doseresp2o3 <-reactive(input$doseresp2o3)#2/3
  alpha2o3<-reactive(input$alpha2o3)
  beta2o3<-reactive(input$beta2o3)
  lambda2o3 <-reactive(input$lambda2o3)
  Pillinfcat2o3<-reactive(input$Pillinfcat2o3)
  Pillinf2o3 <-reactive(input$Pillinf2o3)
  Pillinfmin2o3 <-reactive(input$Pillinfmin2o3)
  Pillinfmax2o3 <-reactive(input$Pillinfmax2o3)
  path_sewage_dist2o3 <-reactive(input$path_sewage_dist2o3)
  p_min2o3 <-reactive(input$p_min2o3)
  p_max2o3 <-reactive(input$p_max2o3)
  p_alpha2o3 <-reactive(input$p_alpha2o3)
  p_beta2o3 <-reactive(input$p_beta2o3)
  doseresp3o3 <-reactive(input$doseresp3o3)#3/3
  alpha3o3<-reactive(input$alpha3o3)
  beta3o3 <-reactive(input$beta3o3)
  lambda3o3 <-reactive(input$lambda3o3)
  Pillinfcat3o3 <-reactive(input$Pillinfcat3o3)
  Pillinf3o3 <-reactive(input$Pillinf3o3)
  Pillinfmin3o3<-reactive(input$Pillinfmin3o3)
  Pillinfmax3o3 <-reactive(input$Pillinfmax3o3)
  path_sewage_dist3o3<-reactive(input$path_sewage_dist3o3)
  p_min3o3<-reactive(input$p_min3o3)
  p_max3o3<-reactive(input$p_max3o3)
  p_alpha3o3 <-reactive(input$p_alpha3o3)
  p_beta3o3<-reactive(input$p_beta3o3)
  pill1_name1o3<-reactive(input$pill1_name1o3)
  pill2_name2o3<-reactive(input$pill2_name2o3)
  pill3_name3o3<-reactive(input$pill3_name3o3)
  pill1_name1o1<-reactive(input$pill1_name1o1)
  pill1_name1o2<-reactive(input$pill1_name1o2)
  pill2_name2o2<-reactive(input$pill2_name2o2)
  #total_pi<-reactive(input$total_pi)
  
  
  
  
  
  ###ORGANIZED BY TYPE
  #########Possible Error with log normal, since inputting variables maybe need just rnorm not rlnorm
  Cp_Sew1 <<- reactive( if (number_of_pathogens()== 1) (
    
    if (path_sewage_dist1o1() == "Log Uniform") (
      runif(count(),min=p_min1o1(),max=p_max1o1()) ###removed log check
    ) else if (path_sewage_dist1o1()== "Log Normal")(
      rlnorm(count(),p_alpha1o1(),p_beta1o1())
    )else ( print("equal is not working"))
  )
  )
  Cp_Sew1o2<-reactive(if (number_of_pathogens()==2)(
    #1/2
    if (path_sewage_dist1o2()== 'Log Uniform')(
      runif(count(),min=p_min1o2(),max=p_max1o2())
    ) else if (path_sewage_dist1o2 ()== 'Log Normal')(
      rlnorm(count(),p_alpha1o2(),p_beta1o2())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  Cp_Sew2o2<-reactive( if (number_of_pathogens()==2)(
    if (path_sewage_dist2o2()== 'Log Uniform')(
      runif(count(),min=p_min2o2(),max=p_max2o2())
    ) else if (path_sewage_dist2o2 ()== 'Log Normal')(
      rlnorm(count(),p_alpha2o2(),p_beta2o2())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  Cp_Sew1o3<-reactive( if (number_of_pathogens()==3)(
    #1/3
    if (path_sewage_dist1o3()== 'Log Uniform')(
      runif(count(),min=p_min1o3(),max=p_max1o3())
    ) else if (path_sewage_dist1o3 ()== 'Log Normal')(
      rlnorm(count(),p_alpha1o3(),p_beta1o3())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  Cp_Sew2o3<-reactive( if (number_of_pathogens()==3)(
    if (path_sewage_dist2o3()== 'Log Uniform')(
      runif(count(),min=p_min2o3(),max=p_max2o3())
    ) else if (path_sewage_dist2o3 ()== 'Log Normal')(
      rlnorm(count(),p_alpha2o3(),p_beta2o3())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  
  Cp_Sew3o3<-reactive( if (number_of_pathogens()==3)(
    if (path_sewage_dist3o3()== 'Log Uniform')(
      runif(count(),min=p_min3o3(),max=p_max3o3())
    ) else if (path_sewage_dist3o3 ()== 'Log Normal')(
      rlnorm(count(),p_alpha3o3(),p_beta3o3())
    )  else (
      stop("Invalid entry distribution, must be either 'Log Uniform' or 'Log Normal'")
    )))
  e1o1<<-reactive(WWdose()*10^Cp_Sew1())
  e1o2<<-reactive(WWdose()*10^Cp_Sew1o2())
  e2o2<<-reactive(WWdose()*10^Cp_Sew2o2())
  e1o3<<-reactive(WWdose()*10^Cp_Sew1o3())
  e2o3<<-reactive(WWdose()*10^Cp_Sew2o3())
  e3o3<<-reactive(WWdose()*10^Cp_Sew3o3())
  pi1o1<-reactive( if (number_of_pathogens()==1)( ##[p]robability of [i]nfection
    if (doseresp1o1()== "Single parameter exponential")  (
      fu1(lambda1o1(),e1o1())
    ) else if (doseresp1o1 ()=="Two-parameter beta-poisson") (
      f2(alpha1o1(), beta1o1(), e1o1())
    ) else if (doseresp1o1 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha1o1(), beta1o1(), e1o1())
    )  else(
      stop("Invalid entry for Dose Response")
    ) )
  )
  pi1o2<-reactive(  if (number_of_pathogens()==2)(
    if (doseresp1o2 ()== "Single parameter exponential")  (
      fu1(lambda1o2(),e1o2())
    ) else if (doseresp1o2 ()=="Two-parameter beta-poisson") (
      f2(alpha1o2(), beta1o2(), e1o2())
    ) else if (doseresp1o2 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha1o2(), beta1o2(), e1o2())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi2o2<-reactive( if (number_of_pathogens()==2)(
    if (doseresp2o2 ()== "Single parameter exponential")  (
      fu1(lambda2o2(),e2o2())
    ) else if (doseresp2o2 ()=="Two-parameter beta-poisson") (
      f2(alpha2o2(), beta2o2(), e2o2())
    ) else if (doseresp2o2 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha2o2(), beta2o2(), e2o2())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi1o3<-reactive( if (number_of_pathogens()==3)(
    if (doseresp1o3 ()== "Single parameter exponential")  (
      fu1(lambda1o3(),e1o3())
    ) else if (doseresp1o3 ()=="Two-parameter beta-poisson") (
      f2(alpha1o3(), beta1o3(), e1o3())
    ) else if (doseresp1o3 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha1o3(), beta1o3(), e1o3())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi2o3<-reactive( if (number_of_pathogens()==3)(
    if (doseresp2o3 ()== "Single parameter exponential")  (
      fu1(lambda2o3(),e2o3())
    ) else if (doseresp2o3 ()=="Two-parameter beta-poisson") (
      f2(alpha2o3(), beta2o3(), e2o3())
    ) else if (doseresp2o3 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha2o3(), beta2o3(), e2o3())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  pi3o3<-reactive( if (number_of_pathogens()==3)(
    if (doseresp3o3 ()== "Single parameter exponential")  (
      fu1(lambda3o3(),e3o3())
    ) else if (doseresp3o3 ()=="Two-parameter beta-poisson") (
      f2(alpha3o3(), beta3o3(), e3o3())
    ) else if (doseresp3o3 ()=="Two-parameter hypergeometric1F1") (
      f3(alpha3o3(), beta3o3(), e3o3())
    )  else(
      stop("Invalid entry for Dose Response")
    )))
  Pillinf1o1a<-reactive(if (number_of_pathogens()==1)
    if (Pillinfcat1o1()== "uniform")(
      runif(count(),min=Pillinfmin1o1(),max=Pillinfmax1o1())
    ) else if (Pillinfcat1o1()=="single value")(
      Pillinf1o1()
    ) else(
      stop("Something is wrong between lines 888 and 911")
    )
  )
  Pillinf1o2a<-reactive( if (number_of_pathogens()==2)
    if (Pillinfcat1o2()== "uniform")(
      runif(count(),min=Pillinfmin1o2(),max=Pillinfmax1o2())
    ) else if (Pillinfcat1o2()=="single value")(
      Pillinf1o2()
    ) else(
      stop("Something is wrong between lines 888 and 911")
    )
  )
  Pillinf2o2a<-reactive( if (number_of_pathogens()==2)
    if (Pillinfcat2o2()== "uniform")(
      runif(count(),min=Pillinfmin2o2(),max=Pillinfmax2o2())
    ) else if (Pillinfcat2o2()=="single value")(
      Pillinf2o2()
    ))
  Pillinf1o3a<-reactive( if (number_of_pathogens()==3)
    if (Pillinfcat1o3()== "uniform")(
      runif(count(),min=Pillinfmin1o3(),max=Pillinfmax1o3())
    ) else  if (Pillinfcat1o3()=="single value")(
      Pillinf1o3()
    ))
  Pillinf2o3a<-reactive( if (number_of_pathogens()==3)
    if (Pillinfcat2o3()== "uniform")(
      runif(count(),min=Pillinfmin2o3(),max=Pillinfmax2o3())
    ) else if (Pillinfcat2o3()=="single value")(
      Pillinf2o3()
    ))
  Pillinf3o3a<-reactive( if (number_of_pathogens()==3)
    if (Pillinfcat3o3()== "uniform")(
      runif(count(),min=Pillinfmin3o3(),max=Pillinfmax3o3())
    ) else if (Pillinfcat3o3()=="single value")(
      Pillinf3o3()
    ))
  pill1o1<<-reactive(if (number_of_pathogens()==1) #[p]robability of [ill]ness
    pi1o1()*Pillinf1o1a())
  pill1o2<<-reactive(if (number_of_pathogens()==2)
    pi1o2()*Pillinf1o2a())
  pill2o2<<-reactive(if (number_of_pathogens()==2)
    pi2o2()*Pillinf2o2a())
  pill1o3<<-reactive(if (number_of_pathogens()==3)
    pi1o3()*Pillinf1o3a())
  pill2o3<<-reactive(if (number_of_pathogens()==3)
    pi2o3()*Pillinf2o3a())
  pill3o3<<-reactive(if (number_of_pathogens()==3)
    pi3o3()*Pillinf3o3a())
  
  totpill1o1<-reactive(if (number_of_pathogens()==1)
    1-((1-pill1o1())))
  
  totpill2<-reactive(if (number_of_pathogens()==2)
    1-((1-pill1o2())*(1-pill2o2())))
  
  totpill3<-reactive(if (number_of_pathogens()==3)
    1-((1-pill1o3())*(1-pill2o3())*(1-pill3o3())))
  
  df1 <-reactive({ 
    df1p<-data.frame(pill1o1(),totpill1o1())
    colnames(df1p) <- c(pill1_name1o1(),"Total Pill")
    df1p
  })
  df2 <-reactive({
    df2p<-data.frame(pill1o2(),pill2o2(),totpill2()) 
    colnames(df2p) <- c(pill1_name1o2(),pill2_name2o2(),"Total Pill")
    df2p
  })
  df3 <- reactive({
    df3p<-data.frame(pill1o3(),pill2o3(),pill3o3(),totpill3())
    colnames(df3p) <- c(pill1_name1o3(), pill2_name2o3(),pill3_name3o3(),"Total Pill")   #c(pill1_name1o2(),pill2_name2o2(),"Total Pill")
    df3p
  })
  dfm1<-reactive(if (number_of_pathogens()==1)(
    melt(df1(),variable.name = "pathogen id", value.name = "p_ill")))
  dfm2<-reactive(if (number_of_pathogens()==2)(
    melt(df2(),variable.name = "pathogen id", value.name = "p_ill")))
  dfm3<-reactive(if (number_of_pathogens()==3)
    melt(df3(),variable.name = "pathogen id", value.name = "p_ill"))

  
  finaldf<-reactive(if (is.data.frame(dfm1())==TRUE)
    dfm1()
    else if (is.data.frame(dfm2())==TRUE)
      dfm2()
    else if (is.data.frame(dfm3())==TRUE)
      dfm3()
    else print("All dataframes are null, check input values")
  )
  
  
  #plots
  output$boxplot1<-renderPlot({ggplot(finaldf(), 
                                      aes(x = finaldf()[,1], y=finaldf()[,2])) + 
      scale_y_log10(labels=trans_format('log10',math_format(10^.x)))+
      geom_boxplot() +
      ylab("")+
      xlab("")+
      geom_hline(yintercept=0.03,linetype="dashed")+
      annotation_logticks(sides="l") +
      theme_linedraw(base_size = 15)+
      stat_summary(fun.y=mean, colour="black", geom="point", hape=18, size=1,show_legend = FALSE)+
      stat_summary(fun.y=mean, colour="black", geom="text", show_legend = FALSE, 
                   vjust=-0.7, aes( label=round(..y.., digits=1)))
       
  })
  output$histWWdose<-renderPlot({ggplot(data=WWdosedf(),
                                        aes(x=WWdosedf()[,1])) + 
      scale_x_log10(labels=trans_format('log10',math_format(10^.x)))+
      geom_histogram(bins=50)  +
      #ggtitle("Wastewater Dose Distribution") +
      xlab("Dose (mL)") +
      theme_linedraw(base_size = 15)
  })
  #  
  # output$boxplot1<-renderPlot(boxplot(pill1(),pill2(),totpill, log = 'y',main="Probability of Illness from Swimming in Contaminated Water", ylab="Probability of Illness"))
  # 
  # 
  
  ##functions for use by Probability_Illness code
  
  fu1<-function(x,N){ ##single parameter exponential
    1-(exp(-x*N))
  }
  
  f2<-function(a,b,N){##two parameter beta-poisson
    1-(1+N/a)^-b
  }
  
  f3<-function(a,b,N){###hypergeometric from packageCharFun
    1- hypergeom1F1(-N,a,b)
  }
  
  dfEXPORT<-reactive(if (is.data.frame(df1())==TRUE)
    df1()
    else if (is.data.frame(df2())==TRUE)
      df2()
    else if (is.data.frame(df3())==TRUE)
      df3()
    else print("All dataframes are null, check input values")
  )
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Wastewater Dose" = isolate(WWdosedf()),
           "Probability of Illness" = isolate(dfEXPORT()))
           
  })
  # Table of selected dataset ----
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  observeEvent(input$do,{
    output$table <- renderTable({
    head(datasetInput())
    
  },
  digits = -2)
  })
  dfmprint<-isolate({df2()}) #to check if it is working
  print(head(dfmprint))
  
}
#run the application
shinyApp(ui = ui, server = server)
