
shinyServer(function(input, output, session) {

  # Load modules
  path1 <- callModule(module = pathogens, id = 'path1', n = reactive(1), sample_n = sample_n, ww_dose = ww_dose)
  path2 <- callModule(module = pathogens, id = 'path2', n = reactive(2), sample_n = sample_n, ww_dose = ww_dose)
  path3 <- callModule(module = pathogens, id = 'path3', n = reactive(3), sample_n = sample_n, ww_dose = ww_dose)

  # UI ----------------------------------------------------------------------

  output$ui <- renderUI({

    tagList(

      titlePanel("QMRA of swimming in sewage impacted environmental waters"),
      tags$p(text_blocks$appinfo),

      fluidRow(
        column(
          width = 3,
          # Indicator
          uiOutput("ui_indic")
        ),
        column(
          width = 3,
          # Indicator in sewage
          uiOutput("ui_indic_sewage")
        ),
        column(
          width = 3,
          # Seed and count for sampling
          uiOutput("ui_sample")
        ),
        column(
          width = 3,
          # Parameters for distribution of water consumed by swimmers
          uiOutput('ui_dose_pars')
        )
      ),

      fluidRow(
        column(
          width = 6,
          plotOutput("plot_box1"),
          tags$p(text_blocks$plot1)
        ),
        column(
          width = 6,
          plotOutput("plot_hist1")
        )
      ),

      # UI conditional on input$ui_dose_num_pathogens

      fluidRow(
        uiOutput("ui_pathogens")
      ),

      fluidRow(
        uiOutput("ui_download")
      )

    )

  })

  # UI ROW 1 ----------------------------------------------------------------

  output$ui_indic <- renderUI({
    numericInput(
      inputId = "indic",
      label   = "Concentration of indicator in environmental waters in log 10 copies/100mL",
      value   = 1,
      min     = 0,
      max     = 100
    )
  })

  output$ui_indic_sewage <- renderUI({
    tagList(
      uiOutput("ui_indic_sewage_dist"),
      uiOutput("ui_indic_sewage_dist_pars")
    )
  })

  output$ui_indic_sewage_dist <- renderUI({
    selectInput(
      inputId  = "indic_sewage_dist",
      label    = "Which distribution does the indicator concentration in sewage follow?",
      choices  = c("log uniform", "log normal"),
      selected = "Log Uniform"
    )
  })

  output$ui_indic_sewage_dist_pars <- renderUI({
    validate(need(input$indic_sewage_dist, 'Loading...'))
    if(input$indic_sewage_dist == "log uniform") {
      tagList(
        numericInput(
          inputId = "indic_sewage_dist_par_max",
          label   = "Maximum concentration of indicator in sewage in log10 copies/L ",
          value   = 9.5,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = "indic_sewage_dist_par_min",
          label   = "Minimum concentration of indicator in sewage in log10 copies/L",
          value   = 7,
          min     = 0,
          max     = 100
        )
      )
    } else if(input$indic_sewage_dist == "log normal") {
      tagList(
        numericInput(
          inputId = "indic_sewage_dist_par_alpha",
          label   = "Enter alpha for log normal distribution",
          value   = 1,
          min     = 0,
          max     = 100
        ),
        numericInput(
          inputId = "indic_sewage_dist_par_beta",
          label   = "Enter beta for log normal distribution",
          value   = 1,
          min     = 0,
          max     = 100
        )
      )
    }

  })

  output$ui_sample <- renderUI({
    tagList(
      uiOutput("ui_sample_seed"),
      uiOutput("ui_sample_n")
    )
  })

  output$ui_sample_seed <- renderUI({
    numericInput(
      inputId = "sample_seed",
      label   = "Set seed of random number generator, default is 1.",
      value   = 1,
      min     = 0,
      max     = 1000000000
    )
  })

  output$ui_sample_n <- renderUI({
    sliderInput(
      inputId = "sample_n",
      label   = "How many samplings? Many Monte Carlo simulations use 10,000",
      min     = 0,
      max     = 100000,
      value   = 10000,
      step    = 5000
    )
  })

  output$ui_dose_pars <- renderUI({
    tagList(
      tags$p(text_blocks$doseresponseinfo),
      uiOutput("ui_dose_mean"),
      uiOutput("ui_dose_sd"),
      uiOutput("ui_path_num")
    )
  })

  output$ui_dose_mean <- renderUI({
    numericInput(
      inputId = "dose_mean",
      label   = "Mean of dose",
      min     = 0,
      max     = 100,
      value   = 2.92
    )
  })

  output$ui_dose_sd <- renderUI({
    numericInput(
      inputId = "dose_sd",
      label   = "Standard deviation of dose ",
      min     = 0,
      max     = 10,
      value   = 1.42
    )
  })

  output$ui_path_num <- renderUI({
    numericInput(
      inputId = "path_num",
      label   = "How many pathogen dose responses to include, up to 3 currently supported.",
      min     = 1,
      max     = 3,
      value   = 1,
      step    = 1
    )
  })

  # UI ROW 2 ----------------------------------------------------------------

  output$plot_box1 <- renderPlot({
    ggplot(data = df_final(), mapping = aes(x = path_id, y = p_ill)) +
      scale_y_log10(labels = trans_format(trans = 'log10', format = math_format(10 ^ .x))) +
      geom_boxplot() +
      ylab("") +
      xlab("") +
      ggtitle("Probability of Illness from Swimming in Contaminated Water") +
      geom_hline(yintercept = 0.03, linetype = "dashed") +
      annotation_logticks(sides = "l") +
      theme_linedraw(base_size = 15) +
      stat_summary(
        fun.y       = mean,
        colour      = "black",
        geom        = "point",
        show.legend = FALSE,
        size        = 1
      ) +
      stat_summary(
        mapping     = aes(label = round(..y.., digits = 1)),
        fun.y       = mean,
        colour      = "black",
        geom        = "text",
        show.legend = FALSE,
        vjust       = -0.7
      )
  })

  output$plot_hist1 <- renderPlot({
    ggplot(data = ww_dose(), mapping = aes(x = dose)) +
      scale_x_log10(labels = trans_format(trans = "log10", format = math_format(10 ^ .x))) +
      geom_histogram(bins = 50) +
      xlab("Dose (mL)") +
      theme_linedraw(base_size = 15)
  })

  # UI ROW 3 ----------------------------------------------------------------

  output$ui_pathogens <- renderUI({
    validate(need(input$path_num, 'Loading...'))
    lapply(X = seq_len(input$path_num), FUN = function(n) {
      column(width = 4, pathogensUI(paste0('path', n)))
    })
  })

  # UI ROW 4 ----------------------------------------------------------------

  output$ui_download <- renderUI({
    tagList(
      uiOutput("ui_download_data"),
      tableOutput("ui_download_preview"),
      tags$p(text_blocks$download_info)
    )
  })

  output$ui_download_data <- renderUI({
    tagList(
      tags$p("Download a dataset generated by this app"),
      selectInput(
        inputId = "dl_data_name",
        label   = "Choose a dataset:",
        choices = c("wastewater dose", "probability of illness")
      ),
      downloadButton(
        outputId = "dl_data",
        label    = "Download"
      ),
      actionButton(
        inputId = "dl_data_preview",
        label   = "Preview Data"
      )
    )
  })

  output$ui_download_preview <- renderTable({
    head(dl_dataset())
  })

  # DOWNLOAD HANDLER --------------------------------------------------------

  dl_dataset <- eventReactive(
    eventExpr = {
      input$dl_data
      input$dl_data_preview
    },
    valueExpr = {
      if(input$dl_data_name == "wastewater dose") {
        ww_dose()
      } else if(input$dl_data_name == "probability of illness") {
        df_final()
      }
    }
  )

  output$dl_data <- downloadHandler(
    filename = function() {
      input$dl_data_name %>%
        gsub(pattern = ' ', replacement = '_') %>%
        paste0(., ".csv")
    },
    content = function(file) {
      write.csv(x = isolate(dl_dataset()), file = file, row.names = FALSE)
    }
  )

  # SAMPLING ----------------------------------------------------------------

  # Set seed
  observe({
    set.seed(input$sample_seed)
  })

  # WASTEWATER DOSE FUNCTION ------------------------------------------------

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

  ww_dose <- reactive({
    validate(need(input$indic_sewage_dist, 'Loading...'))
    if(input$indic_sewage_dist == "log uniform") {
      validate(need(input$indic_sewage_dist_par_min, 'Loading...'))
      validate(need(input$indic_sewage_dist_par_max, 'Loading...'))
      dist <- runif(
        n   = input$sample_n,
        min = input$indic_sewage_dist_par_min,
        max = input$indic_sewage_dist_par_max
      )
    } else if(input$indic_sewage_dist == "log normal") {
      validate(need(input$indic_sewage_dist_par_alpha, 'Loading...'))
      validate(need(input$indic_sewage_dist_par_beta, 'Loading...'))
      dist <- rlnorm(
        n       = input$sample_n,
        meanlog = input$indic_sewage_dist_par_alpha,
        sdlog   = input$indic_sewage_dist_par_beta
      )
    }
    env_water_dose <- rlnorm(
      n       = input$sample_n,
      meanlog = input$dose_mean,
      sdlog   = input$dose_sd
    )
    fraction_ww <- input$indic / 10 ^ dist
    dose <- env_water_dose / 1000 * fraction_ww
    data.frame(dose)
  })

  # PROBABILITY ILLNESS FUNCTION --------------------------------------------

  sample_n <- reactive({
    input$sample_n
  })

  p_ill_tot <- reactive({
    req(input$path_num)
    if(input$path_num == 1) {
      1 - (1 - path1$p_ill())
    } else if(input$path_num == 2) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) )
    } else if(input$path_num == 3) {
      1 - ( (1 - path1$p_ill()) * (1 - path2$p_ill()) * (1 - path3$p_ill()) )
    }
  })

  df <- reactive({
    req(input$path_num)
    if(input$path_num == 1) {
      data.frame(path1$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), "Total Pill"))
    } else if(input$path_num == 2) {
      data.frame(path1$p_ill(), path2$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), "Total Pill"))
    } else if(input$path_num == 3) {
      data.frame(path1$p_ill(), path2$p_ill(), path3$p_ill(), p_ill_tot()) %>%
        set_names(nm = c(path1$name(), path2$name(), path3$name(), "Total Pill"))
    }
  })

  df_final <- reactive({
    melt(
      data          = df(),
      variable.name = "path id",
      value.name    = "p_ill"
    ) %>%
      set_names(c("path_id", "p_ill"))
  })

})
