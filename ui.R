
fluidPage(

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
